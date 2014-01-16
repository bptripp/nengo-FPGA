-- Simple Ethernet transmit handler that is
-- only responsible for output channel data.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ethernet_tx_handler is port (
  clk: in std_logic;
  rst: in std_logic;
  station_mac: in std_logic_vector(47 downto 0);

  -- channel side
  ch_fifo_data: in std_logic_vector(11 downto 0);
  ch_fifo_re: out std_logic;
  ch_fifo_count: in std_logic_vector(10 downto 0);
  ch_fifo_frame_ready: in std_logic;    -- programmable full flag = 512
  ch_done: in std_logic;
  -- TX FIFO side
  tx_fifo_data: out std_logic_vector(7 downto 0);
  tx_fifo_last: out std_logic;
  tx_fifo_we: out std_logic;
  tx_fifo_full: in std_logic
); end entity ethernet_tx_handler;

architecture rtl of ethernet_tx_handler is
  type state_type is (state_idle,state_armed,state_check_count,state_tx_dest_mac,state_tx_src_mac,state_tx_ethertype,
  state_tx_packet_header,state_tx_dv_0,state_tx_dv_1,state_tx_dv_2);
  type ci_type is record
    state: state_type;
    ch_fifo_re: std_logic;
    tx_fifo_data: std_logic_vector(7 downto 0);
    tx_fifo_last: std_logic;
    tx_fifo_we: std_logic;
    sequence_number: unsigned(15 downto 0);
    pair_count: unsigned(7 downto 0); -- number of pairs to transmit, minus one
	 last_sequence: std_logic; -- enabled by ch_done
	 tx_count: unsigned(3 downto 0);
	 tx_buffer: std_logic_vector(7 downto 0); -- buffered ch_fifo_data, in order to allow us to pack 2 DVs into 3 octets
  end record;
  constant reg_reset: ci_type := (
    state => state_idle,
    ch_fifo_re => '0',
    tx_fifo_data => X"00",
    tx_fifo_last => '0',
    tx_fifo_we => '0',
    sequence_number => X"0000",
    pair_count => X"00",
	 last_sequence => '0',
	 tx_count => "0000",
	 tx_buffer => X"00"
  );
  
  signal reg: ci_type := reg_reset;
  signal ci_next: ci_type;
  
  constant MATCHED_ETHERTYPE: std_logic_vector(15 downto 0) := X"88B5"; -- FIXME make sure this is consistent with ethernet_rx_handler
  
begin

ch_fifo_re <= reg.ch_fifo_re;
tx_fifo_data <= reg.tx_fifo_data;
tx_fifo_last <= reg.tx_fifo_last;
tx_fifo_we <= reg.tx_fifo_we;

-- FIXME we do not stall when tx_fifo_full is asserted, but we should!

  COMB: process(reg, rst, ch_fifo_data, ch_fifo_count, ch_fifo_frame_ready, ch_done, tx_fifo_full)
    variable ci: ci_type;
	 variable ch_fifo_count_u: unsigned(10 downto 0);
	 variable pair_count_tmp: unsigned(10 downto 0);
  begin
    ci := reg;
    -- self-clearing flags
    ci.ch_fifo_re := '0';
    ci.tx_fifo_last := '0';
    ci.tx_fifo_we := '0';
	 -- automatic countdown
	 if(reg.tx_count /= "0000") then
		ci.tx_count := reg.tx_count - "0001";
	 end if;
	 
	 ch_fifo_count_u := unsigned(ch_fifo_count);
	 
    if(rst = '1') then
      ci := reg_reset;
    else
      case reg.state is
			when state_idle =>
				-- since output channels initially assert done, wait for done to go to 0, meaning the channel has started running
				if(ch_done = '0') then
					ci.state := state_armed;
				end if;
			when state_armed =>
				if(ch_fifo_frame_ready = '1' or ch_done = '1') then
					-- FIXME this is probably not right but try capturing the count
					if(ch_fifo_frame_ready = '1') then
						-- 512 elements in FIFO = 256 pairs
						ci.pair_count := X"FF";
						ci.state := state_tx_dest_mac;
						ci.tx_count := "0101"; -- 5+1 bytes
					elsif(ch_fifo_count /= "00000000000") then
						-- wait one cycle for last write to become visible
						ci.state := state_check_count;
						ci.last_sequence := '1'; -- FIXME hoping that this is not a race condition					
					else
						-- false alarm, there is no data available
						ci.state := state_idle;
					end if;
				end if;
			when state_check_count =>
				-- fewer than 512 elements in FIFO, but at least 1
				-- so only bits 8-0 in ch_fifo_count can be set
				-- in order to get the correct pair count, which needs to be one less than the actual number of pairs,
				-- we subtract 1 and then divide by 2
				pair_count_tmp := ch_fifo_count_u - "000000001";
				ci.pair_count := pair_count_tmp(8 downto 1);
				ci.state := state_tx_dest_mac;
				ci.tx_count := "0101"; -- 5+1 bytes
			when state_tx_dest_mac =>
				-- transmit to broadcast MAC. this is easy
				ci.tx_fifo_data := X"FF";
				ci.tx_fifo_we := '1';
				if(reg.tx_count = "0000") then
					ci.state := state_tx_src_mac;
					ci.tx_count := "0101"; -- 5+1 bytes
				end if;
			when state_tx_src_mac =>
				ci.tx_fifo_we := '1';
				case reg.tx_count is
					when "0101" =>
						ci.tx_fifo_data := station_mac(47 downto 40);
					when "0100" =>
						ci.tx_fifo_data := station_mac(39 downto 32);
					when "0011" =>
						ci.tx_fifo_data := station_mac(31 downto 24);
					when "0010" =>
						ci.tx_fifo_data := station_mac(23 downto 16);
					when "0001" =>
						ci.tx_fifo_data := station_mac(15 downto 8);
					when "0000" =>
						ci.tx_fifo_data := station_mac(7 downto 0);
						ci.state := state_tx_ethertype;
						ci.tx_count := "0001"; -- 1+1 bytes
					when others => null;
				end case;
			when state_tx_ethertype =>
				ci.tx_fifo_we := '1';
				case reg.tx_count is
					when "0001" =>
						ci.tx_fifo_data := MATCHED_ETHERTYPE(15 downto 8);
					when "0000" =>
						ci.tx_fifo_data := MATCHED_ETHERTYPE(7 downto 0);
						ci.state := state_tx_packet_header;
						ci.tx_count := "0100"; -- 4+1 bytes
					when others => null;
				end case;
			when state_tx_packet_header =>
				ci.tx_fifo_we := '1';
				case reg.tx_count is
					when "0100" => -- tag
						ci.tx_fifo_data := X"FF";
					when "0011" => -- opcode
						ci.tx_fifo_data := X"01";
					when "0010" => -- seqnum HI
						ci.tx_fifo_data := std_logic_vector(reg.sequence_number(15 downto 8));
					when "0001" => -- seqnum LO
						ci.tx_fifo_data := std_logic_vector(reg.sequence_number(7 downto 0));
					when "0000" => -- pair count
						ci.tx_fifo_data := std_logic_vector(reg.pair_count);
						ci.state := state_tx_dv_0;
						ci.ch_fifo_re := '1'; -- acknowledge the DV we're about to read
					when others => null;
				end case;
			when state_tx_dv_0 =>
				ci.tx_fifo_we := '1';
				-- transmit high 8 bits of current DV
				ci.tx_fifo_data := ch_fifo_data(11 downto 4);
				-- buffer low 4 bits of current DV
				ci.tx_buffer(3 downto 0) := ch_fifo_data(3 downto 0);
				ci.ch_fifo_re := '1'; -- acknowledge the DV we're about to read
				ci.state := state_tx_dv_1;
			when state_tx_dv_1 =>
				ci.tx_fifo_we := '1';
				-- transmit buffered low 4 bits and high 4 bits of current DV
				ci.tx_fifo_data(7 downto 4) := reg.tx_buffer(3 downto 0);
				ci.tx_fifo_data(3 downto 0) := ch_fifo_data(11 downto 8);
			   -- buffer low 8 bits of current DV
				ci.tx_buffer(7 downto 0) := ch_fifo_data(7 downto 0);				
				ci.state := state_tx_dv_2;
			when state_tx_dv_2 =>
				ci.tx_fifo_we := '1';
				-- transmit buffered low 8 bits
				ci.tx_fifo_data := reg.tx_buffer(7 downto 0);
				-- check/decrement pair count
				if(reg.pair_count = X"00") then
					ci.tx_fifo_last := '1';
					if(reg.last_sequence = '1') then
						-- restart from idle
						ci.state := state_idle;
						ci.sequence_number := X"0000";
						ci.last_sequence := '0';
					else
						-- continue sequence from armed
						ci.state := state_armed;
						ci.sequence_number := reg.sequence_number + X"0001";						
					end if;
				else
					ci.state := state_tx_dv_0;
					ci.pair_count := reg.pair_count - X"01";
					ci.ch_fifo_re := '1'; -- acknowledge the DV we're about to read
				end if;
		end case;
    end if;
    -- finally
    ci_next <= ci;
  end process COMB;

  SEQ: process(clk, ci_next)
  begin
    if(rising_edge(clk)) then
      reg <= ci_next;
    end if;
  end process SEQ;
  
end architecture rtl;
