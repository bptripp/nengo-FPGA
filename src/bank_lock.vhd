library ieee;
use ieee.std_logic_1164.all;

entity bank_lock is port (
    clk: in std_logic;
    rst: in std_logic;
    en: in std_logic;
    lock: in std_logic;
    timestep: in std_logic;
    swap_banks: out std_logic
); end entity bank_lock;

architecture rtl of bank_lock is

type ci_type is record
    swap_banks: std_logic;
    can_switch: std_logic;
    must_switch: std_logic;
end record;    

constant reg_reset: ci_type := (
    swap_banks => '0',
    can_switch => '0',
    must_switch => '0'
);

signal reg: ci_type := reg_reset;
signal ci_next: ci_type;
begin

swap_banks <= reg.swap_banks;

COMB: process(reg, rst, en, lock, timestep)
    variable ci: ci_type;
    variable lock_en: std_logic;
begin
    ci := reg;
    ci.swap_banks := '0';
    lock_en := en AND lock;
    
    if(rst = '1') then
        ci := reg_reset;
    else
        if(lock_en = '1') then
            if(timestep = '1') then
                ci.must_switch := '1';
            else
                ci.can_switch := '1';
            end if;
        else
            if(reg.must_switch = '1') then
                ci.swap_banks := '1';
                ci.can_switch := '0';
                ci.must_switch := '0';
            elsif(reg.can_switch = '1' and timestep = '1') then
                ci.swap_banks := '1';
                ci.can_switch := '0';
            end if;
        end if;
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
