library ieee;
use ieee.std_logic_1164.all;

entity encoder_output_coordinator is port (
  clk: in std_logic;
  rst: in std_logic;
  all_encoders_done: in std_logic;
  all_outputs_done: in std_logic;
  mux_select: out std_logic;            -- 0 selects encoders, 1 selects outputs
  output_start: out std_logic           -- strobed for 1 cycle to signal outputs                                      
); end entity encoder_output_coordinator;

architecture rtl of encoder_output_coordinator is
  type state_type is (state_wait_encoder_0, state_wait_encoder_1, state_wait_output_0, state_wait_output_1);

  type ci_type is record
    state: state_type;
    mux_select: std_logic;
    output_start: std_logic;
  end record;

  constant reg_reset: ci_type := (
    state => state_wait_encoder_0,
    mux_select => '0',
    output_start => '0'
  );
  signal reg: ci_type := reg_reset;
  signal ci_next: ci_type;
  
begin

  mux_select <= reg.mux_select;
  output_start <= reg.output_start;

  COMB: process(reg, rst, all_encoders_done, all_outputs_done)
    variable ci: ci_type;
  begin
    ci := reg;
    -- self-clearing
    ci.output_start := '0';

    if(rst = '1') then
      ci := reg_reset;
    else
      case reg.state is
        when state_wait_encoder_0 =>
          if(all_encoders_done = '0') then
            ci.state := state_wait_encoder_1;
          end if;
        when state_wait_encoder_1 =>
          if(all_encoders_done = '1') then
            ci.mux_select := '1';
            ci.output_start := '1';
            ci.state := state_wait_output_0;
          end if;
        when state_wait_output_0 =>
          if(all_outputs_done = '0') then
            ci.state := state_wait_output_1;
          end if;
        when state_wait_output_1 =>
          if(all_outputs_done = '1') then
            ci.mux_select := '0';
            ci.state := state_wait_encoder_0;
          end if;
      end case;
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
  
end architecture rtl;
