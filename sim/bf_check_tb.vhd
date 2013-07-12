library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.textio.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity bf_check_tb is
end entity;

architecture sim of bf_check_tb is
    
    constant CLOCK_PERIOD: time := 5 ns;
    signal clk: std_logic;

    signal input: sfixed(1 downto -10) := "100000000000"; -- smallest representable value

    signal bf_addr: std_logic_vector(9 downto 0);
    type BasisFunctionROMType is array(0 to 1023) of std_logic_vector(11 downto 0);
    impure function InitBasisFunctionROMFromFile (FileName: in string) return BasisFunctionROMType is
     FILE ROMFile : text is in FileName;
     variable ROMFileLine : line;
     variable ROM: BasisFunctionROMType;
     variable tmp: bit_vector(11 downto 0);
    begin
     for I in BasisFunctionROMType'range loop
         readline(ROMFile, ROMFileLine);
         read(ROMFileLine, tmp);
         ROM(I) := to_stdlogicvector(tmp);
     end loop;
     return ROM;
    end function;
    
    constant BasisFunction0 : BasisFunctionROMType := InitBasisFunctionROMFromFile("bf0.rom");
    signal bf0_data: std_logic_vector(11 downto 0);
    constant BasisFunction1 : BasisFunctionROMType := InitBasisFunctionROMFromFile("bf1.rom");
    signal bf1_data: std_logic_vector(11 downto 0);
    constant BasisFunction2 : BasisFunctionROMType := InitBasisFunctionROMFromFile("bf2.rom");
    signal bf2_data: std_logic_vector(11 downto 0);
    constant BasisFunction3 : BasisFunctionROMType := InitBasisFunctionROMFromFile("bf3.rom");
    signal bf3_data: std_logic_vector(11 downto 0);
    constant BasisFunction4 : BasisFunctionROMType := InitBasisFunctionROMFromFile("bf4.rom");
    signal bf4_data: std_logic_vector(11 downto 0);
    constant BasisFunction5 : BasisFunctionROMType := InitBasisFunctionROMFromFile("bf5.rom");
    signal bf5_data: std_logic_vector(11 downto 0);
    constant BasisFunction6 : BasisFunctionROMType := InitBasisFunctionROMFromFile("bf6.rom");
    signal bf6_data: std_logic_vector(11 downto 0);

begin

    CLKGEN: process
    begin
        clk <= '0';
        loop
            clk <= '0';
            wait for CLOCK_PERIOD/2;
            clk <= '1';
            wait for CLOCK_PERIOD/2;
        end loop;        
    end process CLKGEN;

    bf_addr <= std_logic_vector(resize(input, 1,-8)); -- drop lowest two bits
    BF0: process(clk, bf_addr)
    begin
        if(rising_edge(clk)) then
            bf0_data <= BasisFunction0(to_integer(unsigned(bf_addr)));
        end if;
    end process BF0;
    BF1: process(clk, bf_addr)
    begin
        if(rising_edge(clk)) then
            bf1_data <= BasisFunction1(to_integer(unsigned(bf_addr)));
        end if;
    end process BF1;
    BF2: process(clk, bf_addr)
    begin
        if(rising_edge(clk)) then
            bf2_data <= BasisFunction2(to_integer(unsigned(bf_addr)));
        end if;
    end process BF2;
    BF3: process(clk, bf_addr)
    begin
        if(rising_edge(clk)) then
            bf3_data <= BasisFunction3(to_integer(unsigned(bf_addr)));
        end if;
    end process BF3;
    BF4: process(clk, bf_addr)
    begin
        if(rising_edge(clk)) then
            bf4_data <= BasisFunction4(to_integer(unsigned(bf_addr)));
        end if;
    end process BF4;
    BF5: process(clk, bf_addr)
    begin
        if(rising_edge(clk)) then
            bf5_data <= BasisFunction5(to_integer(unsigned(bf_addr)));
        end if;
    end process BF5;
    BF6: process(clk, bf_addr)
    begin
        if(rising_edge(clk)) then
            bf6_data <= BasisFunction6(to_integer(unsigned(bf_addr)));
        end if;
    end process BF6;

    TB: process
    begin
        loop
            wait until falling_edge(clk);
            input <= resize(input + to_sfixed(0.0009765625, 1,-10), input);
        end loop;
    end process TB;

end architecture sim;