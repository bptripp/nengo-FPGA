library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ethernet_rx_handler is port (
    clk: in std_logic;
    rst: in std_logic; -- must be an OFF-BOARD reset
    station_mac: in std_logic_vector(47 downto 0);    
    
    -- Ethernet RX FIFO interface
    fifo_data: in std_logic_vector(8 downto 0);
    fifo_empty: in std_logic;
    fifo_re: out std_logic;
    
    -- Nengo programming and control interface
    prog_addr: out std_logic_vector(23 downto 0);
    prog_we: out std_logic;
    prog_data: out std_logic_vector(39 downto 0);
    
    prog_ok: in std_logic;
    prog_ack: in std_logic;
    prog_nyet: in std_logic;
    
    system_reset: out std_logic;
    sim_start: out std_logic;
    sim_pause: out std_logic
); end entity ethernet_rx_handler;

architecture rtl of ethernet_rx_handler is
    type state_type is (state_interframe, state_skip_dst, state_src_mac, state_ethertype,
    state_tag, state_opcode,
    state_cmd_write);
    
    type ci_type is record
        state: state_type;
        fifo_re: std_logic;
        skip_count: unsigned(5 downto 0);
        src_mac: std_logic_vector(47 downto 0);
        
        cmd_tag: std_logic_vector(7 downto 0);
        cmd_opcode: std_logic_vector(7 downto 0);
        
        prog_addr: std_logic_vector(23 downto 0);
        prog_we: std_logic;
        prog_data: std_logic_vector(39 downto 0);
        system_reset: std_logic;
        sim_start: std_logic;
        sim_pause: std_logic;
    end record;
    
    constant reg_reset: ci_type := (
        state => state_interframe,
        fifo_re => '0',
        skip_count => (others=>'0'),
        src_mac => (others=>'0'),
        
        cmd_tag => (others=>'0'),
        cmd_opcode => (others=>'0'),
        
        prog_addr => (others=>'0'),
        prog_we => '0',
        prog_data => (others=>'0'),
        system_reset => '0',
        sim_start => '0',
        sim_pause => '0'       
    );    
    signal reg: ci_type := reg_reset;
    signal ci_next: ci_type;
    
    constant MATCHED_ETHERTYPE: std_logic_vector(15 downto 0) := X"88B5";
    
begin

fifo_re <= reg.fifo_re;
prog_addr <= reg.prog_addr;
prog_we <= reg.prog_we;
prog_data <= reg.prog_data;
sim_start <= reg.sim_start;
sim_pause <= reg.sim_pause;
system_reset <= reg.system_reset;

COMB: process(reg, rst, station_mac, fifo_data, fifo_empty, prog_ok, prog_ack, prog_nyet)
    variable ci: ci_type;
    variable frame_data: std_logic_vector(7 downto 0);
    variable frame_first: std_logic;
begin

    frame_data := fifo_data(7 downto 0);
    frame_first := fifo_data(8);

    ci := reg;
    if(reg.skip_count = "000000") then
        ci.skip_count := "000000";
    else
        ci.skip_count := reg.skip_count - "1";
    end if;
    ci.prog_we := '0';
    ci.sim_start := '0';
    ci.sim_pause := '0';
    ci.system_reset := '0';
    -- always able to read, even when FIFO is empty
    ci.fifo_re := '1';
    if(rst = '1') then
        ci := reg_reset;
    elsif(fifo_empty = '0') then -- data is valid
        case reg.state is
            when state_interframe =>
                if(frame_first = '1') then
                    ci.state := state_skip_dst;
                    ci.skip_count := "000100"; -- skip the next 4+1 bytes (destination MAC)
                end if;
            when state_skip_dst =>
                if(reg.skip_count = "000000") then
                    ci.state := state_src_mac;
                    ci.skip_count := "000101"; -- read 5+1 bytes (source MAC)
                end if;
            when state_src_mac =>
                case reg.skip_count is
                    when "000101" => ci.src_mac(47 downto 40) := frame_data;
                    when "000100" => ci.src_mac(39 downto 32) := frame_data;
                    when "000011" => ci.src_mac(31 downto 24) := frame_data;
                    when "000010" => ci.src_mac(23 downto 16) := frame_data;
                    when "000001" => ci.src_mac(15 downto 8) := frame_data;
                    when "000000" => ci.src_mac(7 downto 0) := frame_data;
                    when others => null;
                end case;
                if(reg.skip_count = "000000") then
                    ci.state := state_ethertype;
                    ci.skip_count := "000001"; -- read 1+1 bytes (ethertype)
                end if;
            when state_ethertype =>
                if(reg.skip_count = "000001") then
                    -- we must receive the correct EtherType in order to process the packet
                    if(frame_data /= MATCHED_ETHERTYPE(15 downto 8)) then
                        ci.state := state_interframe;
                    end if;
                elsif(reg.skip_count = "000000") then
                    if(frame_data /= MATCHED_ETHERTYPE(7 downto 0)) then
                        ci.state := state_interframe;
                    else
                        -- okay, we got a good ethertype and the frame is definitely for us.
                        ci.state := state_tag;
                    end if;
                end if;
            when state_tag =>
                ci.cmd_tag := frame_data;
                ci.state := state_opcode;                
            when state_opcode =>
                ci.cmd_opcode := frame_data;
                -- FIXME check whether programming is allowed (for most of these)
                case frame_data is
                    when X"00" => -- WRITE (PROGRAM)                 
                        ci.state := state_cmd_write;
                        ci.skip_count := "00000111"; -- read 7+1 bytes (3 address, 5 data)
                    when X"FC" => -- PAUSE RUN
                        ci.sim_pause := '1';
                        ci.state := state_interframe;
                    when X"FD" => -- SINGLE-STEP RUN
                        ci.sim_start := '1';
                        ci.sim_pause := '1';
                        ci.state := state_interframe;
                    when X"FE" => -- START RUN
                        ci.sim_start := '1';
                        ci.state := state_interframe;
                    when X"FF" => -- RESET
                        ci.system_reset := '1';
                        ci.state := state_interframe;
                    when others =>
                        ci.state := state_interframe; -- FIXME signal an error
                end case;
            when state_cmd_write =>
                case reg.skip_count is
                    when "000111" => ci.prog_addr(23 downto 16) := frame_data;                    
                    when "000110" => ci.prog_addr(15 downto 8) := frame_data;                    
                    when "000101" => ci.prog_addr(7 downto 0) := frame_data;                    
                    when "000100" => ci.prog_data(39 downto 32) := frame_data;           
                    when "000011" => ci.prog_data(31 downto 24) := frame_data;                    
                    when "000010" => ci.prog_data(23 downto 16) := frame_data;                    
                    when "000001" => ci.prog_data(15 downto 8) := frame_data;                    
                    when "000000" => ci.prog_data(7 downto 0) := frame_data;                    
                    when others => null;
                end case;
                if(reg.skip_count = "000000") then
                    ci.prog_we := '1';
                    -- FIXME wait for acknowledgement and give confirmation
                    ci.state := state_interframe;
                end if;
        end case;
    end if;
    ci_next <= ci;
end process COMB;

SEQ: process(clk, ci_next)
begin
    if(rising_edge(clk)) then
        reg <= ci_next;
    end if;
end process SEQ;

end architecture rtl;
