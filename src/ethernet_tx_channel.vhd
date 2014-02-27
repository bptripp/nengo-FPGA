----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Murphy Berzish
-- 
-- Create Date:    10:28:31 09/27/2012 
-- Design Name: 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ethernet_tx_channel is port (
	clk: in std_logic;
	rst: in std_logic;
	data: in std_logic_vector(7 downto 0);
	tx_en: in std_logic; -- to TX FIFO !empty
	tx_last: in std_logic;
	rd_en: out std_logic; -- to TX FIFO read enable
	MAC_TD: out std_logic_vector(7 downto 0);
	MAC_TX_EN: out std_logic;
	MAC_TX_ERR: out std_logic
);
end ethernet_tx_channel;

architecture rtl of ethernet_tx_channel is
	signal timer: unsigned(3 downto 0) := "0000";
	constant interframe_gap_length: unsigned(3 downto 0) := "1011"; -- IFG is 96 bits = 12 octets, so set this to 11
	constant preamble_length: unsigned(3 downto 0) := "0110"; -- preamble is 7 octets, so set to 6
	constant frame_check_length: unsigned(3 downto 0) := "0011"; -- frame check sequence has 4 octets, so set to 3
	signal data_length: unsigned(5 downto 0) := "000000";
	constant minimum_data_length: unsigned(5 downto 0) := "111011"; -- minimum frame is 64 octets; therefore always send at least 60 octets before FCS
	
	type state_type is (state_interframe,state_preamble,state_sfd,state_data,state_pad,state_fcs);
	signal state: state_type := state_interframe;

   COMPONENT CRC_gen
   PORT(
         Reset : IN  std_logic;
         Clk : IN  std_logic;
         Init : IN  std_logic;
         Frame_data : IN  std_logic_vector(7 downto 0);
         Data_en : IN  std_logic;
         CRC_rd : IN  std_logic;
         CRC_end : OUT  std_logic;
         CRC_out : OUT  std_logic_vector(7 downto 0)
        );
   END COMPONENT;
	signal crc_reset: std_logic;
	signal crc_en: std_logic; 
	signal crc_out: std_logic_vector(7 downto 0);
	signal crc_rd: std_logic;

	type intermediate_type is record
		state: state_type;
		timer: unsigned(3 downto 0);
		data_length: unsigned(5 downto 0);
		rd_en: std_logic;
		tx_en: std_logic;
		tx_err: std_logic;
		td: std_logic_vector(7 downto 0);
		crc_en: std_logic;
		crc_reset: std_logic;
		crc_rd: std_logic;
	end record;
	signal ci_next: intermediate_type;
begin

COMB: process (rst,state,data,tx_en,tx_last,timer,data_length,crc_out)
	variable ci: intermediate_type;
begin
	-- reasonable defaults
	ci.state := state;
	ci.tx_en := '0';
	ci.tx_err := '0';
	ci.td := "00000000";
	ci.crc_en := '0';
	ci.crc_reset := '1';
	ci.crc_rd := '0';
	ci.rd_en := '0';
	if(timer = "0000") then
		ci.timer := "0000";
	else
		ci.timer := timer - 1;
	end if;
	if(data_length = "000000") then
		ci.data_length := "000000";
	else
		ci.data_length := data_length - 1;
	end if;
	-- combinational intermediate logic
	if(rst = '1') then
		ci.state := state_interframe;
		ci.timer := interframe_gap_length;
		-- take the default state for everything else
	else
		case state is
			when state_interframe =>
				if(timer = "0000" and tx_en = '1') then
					ci.state := state_preamble;
					ci.timer := preamble_length;
				end if;
			when state_preamble =>
				ci.tx_en := '1';
				ci.td := "01010101";
				if(timer = "0000") then
					ci.state := state_sfd;
				end if;
			when state_sfd =>
				ci.tx_en := '1';
				ci.rd_en := '1';
				ci.crc_reset := '0';
				ci.crc_en := '1';
				ci.td := "11010101";
				ci.state := state_data;
				ci.data_length := minimum_data_length;
			when state_data =>
				ci.tx_en := '1';
				ci.rd_en := '1';
				ci.crc_en := '1';
				ci.crc_reset := '0';
				ci.td := data;
				if(tx_last = '1') then
					-- if we need to pad, we better pad;
					-- otherwise skip straight to the frame check sequence
					if(data_length = "000000") then
						ci.state := state_fcs;
						ci.crc_en := '0';
						ci.crc_rd := '1';
						ci.timer := frame_check_length;
					else
						ci.state := state_pad;
					end if;
				end if;
			when state_pad =>
				ci.tx_en := '1';
				ci.crc_en := '1';
				ci.crc_reset := '0';
				ci.td := "00000000";
				if(data_length = "000000") then
					-- don't need to insert any more padding
					ci.state := state_fcs;
					ci.crc_en := '0';
					ci.crc_rd := '1';
					ci.timer := frame_check_length;
				end if;
			when state_fcs =>
				ci.tx_en := '1';
				ci.crc_reset := '0';
				ci.crc_rd := '1';
				ci.td := crc_out;
				if(timer = "0000") then
					ci.state := state_interframe;
					ci.timer := interframe_gap_length;
				end if;
			when others =>
				null;
		end case; -- case state
	end if; -- rst = '0'
	-- must be last
	ci_next <= ci;
end process COMB;

SEQ: process(clk,ci_next)
begin
	if(rising_edge(clk)) then
		state <= ci_next.state;
		timer <= ci_next.timer;
		MAC_TX_EN <= ci_next.tx_en;
		MAC_TX_ERR <= ci_next.tx_err;
		MAC_TD <= ci_next.td;
		crc_en <= ci_next.crc_en;
		crc_reset <= ci_next.crc_reset;
		crc_rd <= ci_next.crc_rd;
		data_length <= ci_next.data_length;
		rd_en <= ci_next.rd_en;
	end if;
end process SEQ;

CRC_FCS: CRC_gen PORT MAP (
          Reset => rst,
          Clk => clk,
          Init => crc_reset,
          Frame_data => ci_next.td,
          Data_en => crc_en,
          CRC_rd => crc_rd,
          CRC_end => open,
          CRC_out => crc_out
        );
 
end architecture rtl;

