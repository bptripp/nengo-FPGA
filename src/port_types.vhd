----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/16/2013 10:40:54 AM
-- Design Name: 
-- Module Name: port_types - Behavioral
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
use IEEE.numeric_std.ALL;

package port_types is

type encoder_addresses is array(integer range <>) of std_logic_vector(19 downto 0);
type dv_data is array(integer range <>) of std_logic_vector(11 downto 0);

end;

