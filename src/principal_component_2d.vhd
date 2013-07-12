----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/18/2013 10:50:44 AM
-- Design Name: 
-- Module Name: principal_component_2d - Behavioral
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
use IEEE.numeric_std.all;

library std;
use std.textio.all;

library IEEE_proposed;
use IEEE_proposed.fixed_pkg.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity principal_component_2d is
    generic (
        loadfile: string
    );
    Port ( clk : in STD_LOGIC;
           x_in : in STD_LOGIC_VECTOR (11 downto 0); -- FIXME was (9 downto 0)
           y_in: in std_logic_vector(11 downto 0);
           data : out STD_LOGIC_VECTOR (11 downto 0);
           ready : in STD_LOGIC;
           valid : out STD_LOGIC;
           
           prog_addr: in std_logic_vector(9 downto 0);
           prog_cs: in std_logic;
           prog_we: in std_logic;
           prog_data: in std_logic_vector(11 downto 0)
           );
end principal_component_2d;

architecture Behavioral of principal_component_2d is
    type state_type is (state_idle, state_int1, state_int2, state_int3, state_int4,
                        state_f1, state_f2, state_f3, state_f4, state_wait);
    
    type ci_type is record
      state: state_type;
      -- floor and ceiling of x with the binary point before the seven LSBs
      x1: sfixed(1 downto -3);
      x2: sfixed(1 downto -3);
      y1: sfixed(1 downto -3);
      y2: sfixed(1 downto -3);
      -- (x - x1), (x2 - x)
      xx1: ufixed(-4 downto -10);
      x2x: ufixed(-4 downto -10);
      -- (y - y1), (y2 - y)
      yy1: ufixed(-4 downto -10);
      y2y: ufixed(-4 downto -10);

      -- remember that ?fixed(a downto b) * ?fixed(c downto d)
      -- = ?fixed(a+c+1 downto b+d)
      
      -- (x2 - x)(y2 - y)
      x2xy2y: ufixed(-7 downto -20);
      -- (x - x1)(y2 - y)
      xx1y2y: ufixed(-7 downto -20);
      -- (x2 - x)(y - y1)
      x2xyy1: ufixed(-7 downto -20);
      -- (x - x1)(y - y1)
      xx1yy1: ufixed(-7 downto -20);

      -- RAM holds sampled points at 32x32 spacing, so 10 bits
      -- address format is MSB "XXXXX YYYYY" LSB
      ram_addr: std_logic_vector(9 downto 0);
      
      -- finally
      data: sfixed(-2 downto -30);      -- FIXME seems too large
      valid: std_logic;
    end record;
   
    signal ci_next: ci_type;
    signal reg: ci_type := (
      state => state_idle,
      x1 => "00000",
      x2 => "00000",
      y1 => "00000",
      y2 => "00000",
      xx1 => "0000000",
      x2x => "0000000",
      yy1 => "0000000",
      y2y => "0000000",
      
      x2xy2y => (others=>'0'),
      xx1y2y => (others=>'0'),
      x2xyy1 => (others=>'0'),
      xx1yy1 => (others=>'0'),

      ram_addr => "0000000000",

      data => (others=>'0'),
      valid => '0'
    );

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

    signal ram_data: std_logic_vector(11 downto 0);
    signal ram_data_sfixed: sfixed(1 downto -10);
begin

    COMB: process(reg, x_in, y_in, ready, ram_data_sfixed)
      variable ci: ci_type;
      variable x: sfixed(1 downto -10);
      variable y: sfixed(1 downto -10);    
      variable x_trunc: std_logic_vector(4 downto 0);
      variable y_trunc: std_logic_vector(4 downto 0);
      
      variable increment_slv: std_logic_vector(4 downto 0) := "00001";
      variable increment: sfixed(1 downto -3);
      variable decrement_slv: std_logic_vector(7 downto 0) := "10000000";
      variable decrement: ufixed(-3 downto -10);
    begin
      ci := reg;

      x := to_sfixed(x_in, 1,-10);
      y := to_sfixed(y_in, 1,-10);
      x_trunc := x_in(11 downto 7);
      y_trunc := y_in(11 downto 7);    
      
      increment := to_sfixed(increment_slv, 1,-3);
      decrement := to_ufixed(decrement_slv, -3,-10);
      
      case reg.state is
        when state_idle =>
          if(ready = '1') then
            ci.valid := '0';
            ci.data := (others=>'0');
            
            ci.x1 := to_sfixed(x_trunc, 1,-3);
            --ci.x2 := resize(ci.x1 + from_string("00001", 1,-3), ci.x2);
            ci.x2 := resize(ci.x1 + increment, ci.x2);
            ci.y1 := to_sfixed(y_trunc, 1,-3);
            --ci.y2 := resize(ci.y1 + from_string("00001", 1,-3), ci.y2);
            ci.y2 := resize(ci.y1 + increment, ci.y2);
            -- FIXME check whether x1 == x2 or y1 == y2, and saturate instead of interpolating
            -- so, by definition, the seven bits dropped from x_trunc are equal to (x - x1)
            ci.xx1 := to_ufixed(x_in(6 downto 0), -4,-10);
            ci.yy1 := to_ufixed(y_in(6 downto 0), -4,-10);
            -- by a simple identity we should also have this relationship for (x2 - x)
            --ci.x2x := resize(from_string("10000000", -3,-10) - to_ufixed(x_in(6 downto 0), -4,-10), ci.x2x);
            ci.x2x := resize(decrement - to_ufixed(x_in(6 downto 0), -4,-10), ci.x2x);
            --ci.y2y := resize(from_string("10000000", -3,-10) - to_ufixed(y_in(6 downto 0), -4,-10), ci.y2y);
            ci.y2y := resize(decrement - to_ufixed(y_in(6 downto 0), -4,-10), ci.y2y);
            ci.state := state_int1;          
          end if;
        when state_int1 =>
          -- calculate (x2 - x)(y2 - y)
          ci.x2xy2y := reg.x2x * reg.y2y;
          ci.state := state_int2;
        when state_int2 =>
          -- calculate (x - x1)(y2 - y)
          ci.xx1y2y := reg.xx1 * reg.y2y;
          ci.state := state_int3;
        when state_int3 =>
          -- calculate (x2 - x)(y - y1) 
          ci.x2xyy1 := reg.x2x * reg.yy1;
          -- also schedule the read for f(x1, y1)
          ci.ram_addr := to_slv(reg.x1) & to_slv(reg.y1);
          ci.state := state_int4;
        when state_int4 =>
          -- calculate (x - x1)(y - y1)
          ci.xx1yy1 := reg.xx1 * reg.yy1;
          -- also schedule the read for f(x2, y1)
          ci.ram_addr := to_slv(reg.x2) & to_slv(reg.y1);
          ci.state := state_f1;
        when state_f1 =>
          -- MAC f(x1, y1)(x2 - x)(y2 - y)
          ci.data := resize(reg.data + ram_data_sfixed * to_sfixed(reg.x2xy2y), ci.data); -- FIXME should truncate/overflow
          -- also schedule the read for f(x1, y2)
          ci.ram_addr := to_slv(reg.x1) & to_slv(reg.y2);
          ci.state := state_f2;
        when state_f2 =>
          -- MAC f(x2, y1)(x - x1)(y2 - y)
          ci.data := resize(reg.data + ram_data_sfixed * to_sfixed(reg.xx1y2y), ci.data);
          -- also schedule the read for f(x2, y2)
          ci.ram_addr := to_slv(reg.x2) & to_slv(reg.y2);
          ci.state := state_f3;          
        when state_f3 =>
          -- MAC f(x1, y2)(x2 - x)(y - y1)
          ci.data := resize(reg.data + ram_data_sfixed * to_sfixed(reg.x2xyy1), ci.data);
          ci.state := state_f4;
        when state_f4 =>
          -- MAC f(x2, y2)(x - x1)(y - y1)
          ci.data := resize(reg.data + ram_data_sfixed * to_sfixed(reg.xx1yy1), ci.data);
          ci.valid := '1';
          ci.state := state_wait;
        when state_wait =>
          if(ready = '0') then
            ci.valid := '0';
            ci.state := state_idle;
          end if;
      end case;
      ci_next <= ci;
    end process COMB;

    SEQ: process(clk, ci_next)
    begin
      if(rising_edge(clk)) then
        reg <= ci_next;
      end if;
    end process SEQ;

    data <= to_slv(scalb(reg.data, 6)(1 downto -10));
    valid <= reg.valid;

    PC_ROM: process(clk, reg.ram_addr)
    begin
       if(rising_edge(clk)) then
           ram_data <= PC(to_integer(unsigned(reg.ram_addr)));
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

    ram_data_sfixed <= to_sfixed(ram_data, 1,-10);
    
end Behavioral;
