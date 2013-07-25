----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/23/2013 11:28:42 AM
-- Design Name: 
-- Module Name: delayed_shift_register - Behavioral
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

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity delayed_shift_register is
    generic (
        N: integer; -- output width
        T: integer -- number of shifts to ignore
    );
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           din : in STD_LOGIC;
           shift : in STD_LOGIC;         
           dout : out STD_LOGIC_VECTOR (N-1 downto 0);
           done: out std_logic
           );
end delayed_shift_register;

architecture Behavioral of delayed_shift_register is
    -- clogb2 function - ceiling of log base 2
      function clogb2 (size : integer) return integer is
        variable base : integer := 1;
        variable inp : integer := 0;
      begin
        inp := size - 1;
        while (inp > 1) loop
          inp := inp/2 ;
          base := base + 1;
        end loop;
        return base;
      end function;
      
    constant TIMER_WIDTH: integer := clogb2(T+1); -- number of bits required for the timer
    constant TIMER_RESET_VALUE: unsigned(TIMER_WIDTH-1 downto 0) := to_unsigned(T, TIMER_WIDTH);
    constant TIMER_ZERO_VALUE: unsigned(TIMER_WIDTH-1 downto 0) := (others=>'0');
    signal timer: unsigned(TIMER_WIDTH-1 downto 0) := TIMER_RESET_VALUE;

    constant COUNTER_WIDTH: integer := clogb2(N+1);
    constant COUNTER_RESET_VALUE: unsigned(COUNTER_WIDTH-1 downto 0) := to_unsigned(N, COUNTER_WIDTH);
    constant COUNTER_ZERO_VALUE: unsigned(COUNTER_WIDTH-1 downto 0) := (others=>'0');
    signal counter: unsigned(COUNTER_WIDTH-1 downto 0) := COUNTER_RESET_VALUE;

    signal data : std_logic_vector(N-1 downto 0) := (others=>'0');
    signal done_flag: std_logic := '0';
begin
    dout <= data;
    done <= done_flag;
    
    process(clk, rst, din, shift, timer, counter)
        variable next_timer: unsigned(TIMER_WIDTH-1 downto 0);
        variable next_counter: unsigned(COUNTER_WIDTH-1 downto 0);
    begin
        next_timer := timer - "1";
        next_counter := counter - "1";
        
        if(rising_edge(clk)) then
            if(rst = '1') then
                data <= (others=>'0');
                timer <= TIMER_RESET_VALUE;
                counter <= COUNTER_RESET_VALUE;
                done_flag <= '0';
            elsif(shift = '1') then
                if(timer /= TIMER_ZERO_VALUE) then -- if we're still counting down, decrement timer and don't shift
                    timer <= next_timer;
                elsif(counter = COUNTER_ZERO_VALUE) then -- if we've already shifted N times, don't shift
                    done_flag <= '1';
                else -- shift and decrease counter
                    data <= data(N-2 downto 0) & din; -- shift into LSB (i.e. present words with MSB first)
                    counter <= next_counter;
                    if(next_counter = COUNTER_ZERO_VALUE) then
                        done_flag <= '1';
                    end if;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
