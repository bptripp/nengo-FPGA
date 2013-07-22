----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/14/2013 02:02:49 PM
-- Design Name: 
-- Module Name: principal_component_1d - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

library std;
use std.textio.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity principal_component_1d is
    generic (
        loadfile: string
    );
    Port ( clk : in STD_LOGIC;
           addr : in STD_LOGIC_VECTOR (9 downto 0);
           valid: in std_logic;
           data : out STD_LOGIC_VECTOR (11 downto 0);
           ready: out std_logic;
           
           prog_addr: in std_logic_vector(9 downto 0);
           prog_cs: in std_logic;
           prog_we: in std_logic;
           prog_data: in std_logic_vector(11 downto 0)
           
           );
end principal_component_1d;

architecture Behavioral of principal_component_1d is    
    type PrincipalComponentMemoryType is array(0 to 1023) of std_logic_vector(11 downto 0);
          
    impure function InitFromFile (FileName: in string) return PrincipalComponentMemoryType is
        FILE ROMFile : text is in FileName;
        variable ROMFileLine : line;
        variable ROM: PrincipalComponentMemoryType;
        variable tmp: bit_vector(11 downto 0);
    begin
        for I in PrincipalComponentMemoryType'range loop
            readline(ROMFile, ROMFileLine);
            read(ROMFileLine, tmp);
            ROM(I) := to_stdlogicvector(tmp);
        end loop;
        return ROM;
    end function; 
    
    shared variable PC : PrincipalComponentMemoryType := InitFromFile(loadfile);
    
begin

    HANDSHAKE: process(clk, valid)
    begin
        if(rising_edge(clk)) then
            ready <= valid; -- single-cycle read
        end if;
    end process HANDSHAKE;

    PC_ROM: process(clk, addr)
    begin
        if(rising_edge(clk)) then
            data <= PC(to_integer(unsigned(addr)));
        end if;
    end process PC_ROM;
    
    PC_PROG: process(clk, prog_addr, prog_cs, prog_we, prog_data)
    begin
        if(rising_edge(clk)) then
            if(prog_cs = '1' and prog_we = '1') then
                PC(to_integer(unsigned(prog_addr))) := prog_data;
            end if;
        end if;
    end process PC_PROG;


end Behavioral;
