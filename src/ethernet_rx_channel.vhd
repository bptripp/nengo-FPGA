library ieee;
use ieee.std_logic_1164.all;

entity ethernet_rx_channel is port (
    clk: in std_logic;
    rst: in std_logic; -- must be an OFF-BOARD reset
    station_mac: in std_logic_vector(47 downto 0);    
    -- from Ethernet PHY
    rx_data: in std_logic_vector(7 downto 0);
    rx_en: in std_logic;
    rx_err: in std_logic;
    -- to RX FIFO
    fifo_data: out std_logic_vector(7 downto 0);
    fifo_first: out std_logic; -- FIFO bit 8
    fifo_we: out std_logic
); end entity ethernet_rx_channel;

architecture rtl of ethernet_rx_channel is
    constant broadcast_mac: std_logic_vector(47 downto 0) := (others=>'1');
    -- we want to delay our data by 6 clock cycles in order to compare against all bits of the station MAC simultaneously.
    type delay_line_type is array(0 to 5) of std_logic_vector(8 downto 0);    
    type state_type is (state_interframe, state_preamble, state_check_mac, state_write_frame, state_wait);
    
    type ci_type is record
        state: state_type;
        delay_line: delay_line_type;
        fifo_data: std_logic_vector(7 downto 0);
        fifo_first: std_logic;
        fifo_we: std_logic;
    end record;
    
    constant reg_reset: ci_type := (
        state => state_interframe,
        delay_line => (others=>(others=>'0')),
        fifo_data => X"00",
        fifo_first => '0',
        fifo_we => '0'
    );
    signal reg: ci_type := reg_reset;
    signal ci_next: ci_type;
    
    signal dst_mac: std_logic_vector(47 downto 0);
    signal mac_valid: std_logic;    
begin

-- since we receive the highest octet of the MAC first and we shift from low to high  
dst_mac <= reg.delay_line(5)(7 downto 0) & reg.delay_line(4)(7 downto 0) & reg.delay_line(3)(7 downto 0)
            & reg.delay_line(2)(7 downto 0) & reg.delay_line(1)(7 downto 0) & reg.delay_line(0)(7 downto 0);
mac_valid <= '1' when (dst_mac = station_mac or dst_mac = broadcast_mac) else '0';


fifo_data <= reg.fifo_first & reg.fifo_data;
fifo_we <= reg.fifo_we;

COMB: process(reg, rst, rx_data, rx_en, dst_mac, mac_valid)
    variable ci: ci_type;
    variable data_valid: std_logic;
begin
    ci := reg;
    -- self-clearing/-setting      
    ci.fifo_first := '0';
    ci.fifo_we := '0';   
    -- shift register from rx_en&rx_data to fifo_data
    ci.fifo_data := reg.delay_line(5)(7 downto 0);
    data_valid := reg.delay_line(5)(8);
    for I in 5 downto 1 loop
        ci.delay_line(I) := reg.delay_line(I-1);
    end loop;
    ci.delay_line(0) := rx_en & rx_data;    
    if(rst = '1') then
        ci := reg_reset;
    else
        case reg.state is
            when state_interframe =>
                if(data_en = '1') then
                    ci.state := state_preamble;
                end if;
            when state_preamble =>                
                if(ci.fifo_data = X"D5") then -- end of preamble, start receiving frame
                    ci.state := state_check_mac;
                end if;
            when state_check_mac =>
                -- the delay lines contain the entire MAC address, so we can check it all at once
                if(mac_valid = '1') then
                    ci.fifo_we := '1';
                    ci.fifo_first := '1';
                    ci.state := state_write_frame;
                else
                    -- invalid MAC; wait for this frame to end
                    ci.state := state_wait;
                end if;
            when state_write_frame =>
                if(data_valid = '1') then
                    ci.fifo_we := '1';
                else
                    -- done receiving frame
                    ci.state := state_interframe;
                end if;
            when state_wait =>
                if(data_valid = '0') then
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