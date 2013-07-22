----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/06/2013 09:59:51 AM
-- Design Name: 
-- Module Name: normal - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity normal is
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           q : out signed (15 downto 0);
           prog_addr: in std_logic_vector(1 downto 0);
           prog_we: in std_logic;
           prog_data: in std_logic_vector(31 downto 0)
           );
end normal;

architecture Behavioral of normal is
    component lfsr32 Port ( 
          clk : in STD_LOGIC;
          rst : in STD_LOGIC;
          load : in STD_LOGIC_VECTOR (31 downto 0);
          q : out STD_LOGIC_VECTOR (31 downto 0));
     end component;
     
     signal x1: std_logic_vector(31 downto 0);
     signal x2: std_logic_vector(31 downto 0);
     signal x3: std_logic_vector(31 downto 0);
     signal x4: std_logic_vector(31 downto 0);
     
     signal y1: signed(14 downto 0);
     signal y2: signed(14 downto 0);
     
     signal load0: std_logic_vector(31 downto 0) := X"7782691f";
     signal load1: std_logic_vector(31 downto 0) := X"deadbeef";
     signal load2: std_logic_vector(31 downto 0) := X"abadd00d";
     signal load3: std_logic_vector(31 downto 0) := X"d0cf11e0";
     
     signal rst0: std_logic;
     signal rst1: std_logic;
     signal rst2: std_logic;
     signal rst3: std_logic;
     
begin

    PROG_INTERFACE: process(clk, rst, prog_addr, prog_we, prog_data)
        variable rst0_v: std_logic;
        variable rst1_v: std_logic;
        variable rst2_v: std_logic;
        variable rst3_v: std_logic;        
    begin
        rst0_v := '0';
        rst1_v := '0';
        rst2_v := '0';
        rst3_v := '0';
        if(rising_edge(clk)) then
            if(rst = '1') then
                rst0_v := '1';
                rst1_v := '1';
                rst2_v := '1';
                rst3_v := '1';
            elsif(prog_we = '1') then
                case prog_addr is
                    when "00" => 
                        load0 <= prog_data;
                        rst0_v := '1';
                    when "01" => 
                        load1 <= prog_data;
                        rst1_v := '1';
                    when "10" => 
                        load2 <= prog_data;
                        rst2_v := '1';
                    when "11" => 
                        load3 <= prog_data;
                        rst3_v := '1';
                    when others => null;
                end case;
            end if;
            rst0 <= rst0_v;
            rst1 <= rst1_v;
            rst2 <= rst2_v;
            rst3 <= rst3_v;
        end if;
    end process PROG_INTERFACE;

    gen0: lfsr32 port map (
        clk => clk,
        rst => rst0,
        load => load0,
        q => x1
    );
    gen1: lfsr32 port map (
        clk => clk,
        rst => rst1,
        load => load1,
        q => x2 
    );
    gen2: lfsr32 port map (
        clk => clk,
        rst => rst2,
        load => load2,
        q => x3 
    );
    gen3: lfsr32 port map (
        clk => clk,
        rst => rst3,
        load => load3,
        q => x4 
    );
    
    y1 <= signed(x1(13) & x1(13 downto 0)) + signed(x2(13 downto 0));
    y2 <= signed(x3(13) & x3(13 downto 0)) + signed(x4(13 downto 0));
    q <= (y1(14) & y1) + y2;

end Behavioral;
