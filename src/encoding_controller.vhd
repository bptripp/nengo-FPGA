----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/13/2013 02:54:41 PM
-- Design Name: 
-- Module Name: encoding_controller - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- Instruction format is as follows:
-- |L|TTTTTTT|P|AAAAAAAAAAAAAAAAAAA|WWWWWWWWWWWW|
-- where L is the 'last flag', T is the time delay, P is the port number,
-- A is the decoded value address, and W is the weight (sfixed, 1 downto -10)
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

library IEEE_proposed;
use IEEE_proposed.fixed_pkg.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity encoding_controller is
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           next_population: in std_logic;
           next_insn: out std_logic;
           insn_data : in STD_LOGIC_VECTOR (39 downto 0);
           dv_addr : out STD_LOGIC_VECTOR (18 downto 0);
           dv_port : out STD_LOGIC;
           dv_data : in STD_LOGIC_VECTOR (11 downto 0);
           sum: out sfixed(1 downto -10);
           done: out std_logic
           );
end encoding_controller;

architecture Behavioral of encoding_controller is

    type state_type is (state_idle, state_delay, state_execute);

    type ci_type is record
        dv_addr: std_logic_vector(18 downto 0);
        dv_port: std_logic;
        sum: sfixed(1 downto -10); -- FIXME accumulator width 
        done: std_logic;

        state: state_type;
        next_insn: std_logic;
        stalled: std_logic;
        dt: unsigned(6 downto 0);
        
        -- 3 pipeline registers for data delay
        weight_s1: sfixed(1 downto -10);
        weight_s2: sfixed(1 downto -10);
        weight_s3: sfixed(1 downto -10);
        last_s1: std_logic;
        last_s2: std_logic;
        last_s3: std_logic;
        
        weight: sfixed(1 downto -10);
        last: std_logic;
    end record;
    
    signal reg: ci_type :=
    (
        dv_addr => "0000000000000000000",
        dv_port => '0',
        sum => "000000000000", 
        done => '0', -- FIXME was 1

        state => state_idle,
        next_insn => '0',
        stalled => '0',        
        dt => "0000000",
        
        weight_s1 => X"000",
        weight_s2 => X"000",
        weight_s3 => X"000",
        last_s1 => '0',
        last_s2 => '0',
        last_s3 => '0',
        
        weight => X"000",
        last => '0' -- FIXME was 1
    );
    signal ci_next: ci_type;
    
    signal next_instruction: std_logic;
    
begin
    
    next_insn <= reg.next_insn;
    dv_addr <= reg.dv_addr;
    dv_port <= reg.dv_port;
    sum <= reg.sum;
    done <= reg.done;

    COMB: process(reg, rst, next_population, insn_data, dv_data)
        variable ci: ci_type;
        --variable dv: sfixed(1 downto -10);
    begin
        ci := reg;
        -- certain flags are self-clearing
        --ci.dv_cs := '0';
        ci.next_insn := '0';
        ci.weight_s1 := to_sfixed(0, ci.weight_s1); -- to prevent us from multiplying bogus data 
        ci.last_s1 := '0';
        -- reset condition
        
        -- multiply and accumulate by default...this should always happen first so it can be overridden by e.g. a reset
        ci.sum := resize(reg.sum + reg.weight * to_sfixed(dv_data, 1, -10), ci.sum);
        
        if(rst = '1') then            
            ci.dv_addr := "0000000000000000000";
            --ci.dv_cs := '0';
            ci.dv_port := '0';
            ci.sum := "000000000000";
            ci.done := '0'; -- FIXME was 1, this should not break anything in the pipelined design
            
            ci.state := state_idle;
            ci.next_insn := '0';
            ci.stalled := '0';
            ci.dt := "0000000";
            
            ci.weight_s1 := X"000";
            ci.weight_s2 := X"000";
            ci.weight_s3 := X"000";
            ci.last_s1 := '0';
            ci.last_s2 := '0';
            ci.last_s3 := '0';
            
            ci.weight := X"000";
            ci.last := '0';
        else
            case reg.state is
                when state_idle =>
                    if(next_population = '1') then
                        ci.state := state_delay;
                        ci.sum := "000000000000";
                        ci.done := '0';
                        -- flush pipeline
                        ci.weight_s1 := X"000";
                        ci.weight_s2 := X"000";
                        ci.weight_s3 := X"000";
                        ci.last_s1 := '0';
                        ci.last_s2 := '0';
                        ci.last_s3 := '0';
                        
                        ci.weight := X"000";
                        ci.last := '0';
                    end if;
                when state_delay =>
                    -- if we've been here before...
                    if(reg.stalled = '1') then
                        -- check delay register
                        if(reg.dt = "0000000") then
                            -- acknowledge
                            ci.next_insn := '1';
                            ci.stalled := '0';
                            ci.state := state_execute;
                            -- but here's where we actually use the instruction...
                            --ci.dv_cs := '1';
                            ci.dv_port := insn_data(31);
                            ci.dv_addr := insn_data(30 downto 12);
                            
                            -- propagate weight and L
                            ci.weight_s1 := to_sfixed(insn_data(11 downto 0), ci.weight_s1);
                            ci.last_s1 := insn_data(39);
                        else
                            ci.dt := reg.dt - "0000001";
                        end if;
                    else
                        -- read instruction and set delay register appropriately
                        if(insn_data(38 downto 32) = "0000000") then
                            ci.next_insn := '1';
                            ci.state := state_execute;
                            -- but here's where we actually use the instruction...
                            --ci.dv_cs := '1';
                            ci.dv_port := insn_data(31);
                            ci.dv_addr := insn_data(30 downto 12);
                            
                            -- propagate weight and L
                            ci.weight_s1 := to_sfixed(insn_data(11 downto 0), ci.weight_s1);
                            ci.last_s1 := insn_data(39);
                        else
                            ci.dt := unsigned(insn_data(38 downto 32)) - "0000001";
                            ci.stalled := '1';
                        end if;
                    end if;
                when state_execute =>
                    -- if this is the last one, idle after this.
                    if(insn_data(39) = '1') then
                        ci.state := state_idle;
                    else
                        ci.state := state_delay;
                    end if;
            end case;
            
            -- pipeline weight and L
            ci.weight_s2 := reg.weight_s1;
            ci.weight_s3 := reg.weight_s2;
            ci.weight := reg.weight_s3;
            
            ci.last_s2 := reg.last_s1;
            ci.last_s3 := reg.last_s2;
            ci.last := reg.last_s3;
                        
            if(reg.last = '1') then
                ci.done := '1';
            end if;
        end if;
        -- finally
        ci_next <= ci;
    end process COMB;
    
    SEQ: process(clk, ci_next)
    begin
        if(rising_edge(clk)) then
            reg <= ci_next;
        end if;
    end process SEQ;

end Behavioral;
