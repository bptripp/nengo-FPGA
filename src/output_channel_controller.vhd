library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity output_channel_controller is port (
  clk: in std_logic;
  rst: in std_logic;

  start: in std_logic;
  next_insn: out std_logic;
-- The instruction format for this controller is:
-- |L|P|DDDDDDDD|AAAAAAAAAAA|NNNNNNNNNNN|TTTT|
  insn_data: in std_logic_vector(35 downto 0);
  no_insns: in std_logic;
  dv_addr: out std_logic_vector(18 downto 0);
  dv_port: out std_logic;
  dv_data: in std_logic_vector(11 downto 0);
  channel_data: out std_logic_vector(11 downto 0);
  channel_we: out std_logic;
  done: out std_logic
);
end entity;

architecture rtl of output_channel_controller is

  type state_type is (state_idle, state_delay, state_execute);

  type ci_type is record
    -- outputs
    next_insn: std_logic;
    dv_addr: std_logic_vector(18 downto 0);
    dv_port: std_logic;
    channel_data: std_logic_vector(11 downto 0);
    channel_we: std_logic;
    done: std_logic;
    -- state variables
    state: state_type;
	 stalled: std_logic;
	 dt: unsigned(3 downto 0);
	 insn: std_logic_vector(35 downto 0);
	 read_count: unsigned(10 downto 0);
	 next_addr: std_logic_vector(10 downto 0);
	  -- 3 pipeline registers for data delay
	 last_s1: std_logic;
    last_s2: std_logic;
    last_s3: std_logic;            
    last: std_logic;
	 valid_s1: std_logic;
    valid_s2: std_logic;
    valid_s3: std_logic;            
    valid: std_logic;
  end record;

  constant reg_reset: ci_type :=
  (
    -- outputs
    next_insn => '0',
    dv_addr => "0000000000000000000",
    dv_port => '0',
    channel_data => X"000",
    channel_we => '0',
    done => '1',
    -- state variables
    state => state_idle,
	 stalled => '0',
	 dt => "0000",
	 insn => X"000000000",
	 read_count => "00000000000",
	 next_addr => "00000000000",
	 last_s1 => '0',
	 last_s2 => '0',
	 last_s3 => '0',
	 last => '0',
	 valid_s1 => '0',
	 valid_s2 => '0',
	 valid_s3 => '0',
	 valid => '0'
  );
  
  signal reg: ci_type := reg_reset;
  signal ci_next: ci_type;
  
begin

	next_insn <= reg.next_insn;
	dv_addr <= reg.dv_addr;
	dv_port <= reg.dv_port;
	channel_data <= reg.channel_data;
	channel_we <= reg.channel_we;
	done <= reg.done;
  
  COMB: process(reg, rst, start, insn_data, no_insns, dv_data)
    variable ci: ci_type;
  begin
    ci := reg;
    -- self-clearing flags
    ci.next_insn := '0';
    ci.channel_we := '0';
	 ci.last_s1 := '0';
	 ci.valid_s1 := '0';

    if(rst = '1') then
      ci := reg_reset;
    else
      case reg.state is
        when state_idle =>
		    if(no_insns = '1') then
				-- finish immediately if the channel is unused; assert done for one clock cycle
				if(start = '1') then
					ci.done := '1';
				else
					ci.done := '0';
				end if;
			 else
				 if(start = '1') then
					ci.state := state_delay;
					ci.dt := unsigned(insn_data(3 downto 0));
					ci.done := '0';
					-- flush pipeline
					ci.last_s1 := '0';
					ci.last_s2 := '0';
					ci.last_s3 := '0';
					ci.valid_s1 := '0';
					ci.valid_s2 := '0';
					ci.valid_s3 := '0';
					
					ci.last := '0';
					ci.valid := '0';
				 end if;
			 end if;
		  when state_delay =>
			 -- if we've been here before...
			 if(reg.stalled = '1') then
				-- check delay register
				if(reg.dt = "0000") then
					-- acknowledge and load instruction
					ci.state := state_execute;
					ci.insn := insn_data;
				   ci.read_count := unsigned(insn_data(14 downto 4));
					ci.next_addr(10 downto 0) := insn_data(25 downto 15);
					ci.next_insn := '1';
					ci.stalled := '0';
				else					
					ci.dt := reg.dt - "0001";					
				end if;
			 else
				-- read instruction and load delay register
				if(insn_data(3 downto 0) = "0000") then -- no extra delay
					ci.state := state_execute;
					ci.insn := insn_data;
				   ci.read_count := unsigned(insn_data(14 downto 4));
					ci.next_addr(10 downto 0) := insn_data(25 downto 15);
					ci.next_insn := '1';
					ci.stalled := '0';
				else
					ci.dt := unsigned(insn_data(3 downto 0)) - "0001";
					ci.stalled := '1';
				end if;
			 end if;
        when state_execute =>
			 -- execute instruction: issue a read and indicate valid data pending
			 ci.valid_s1 := '1';
			 ci.dv_port := reg.insn(34);
		    ci.dv_addr(18 downto 11) := reg.insn(33 downto 26);
			 ci.dv_addr(10 downto 0) := reg.next_addr;
			 
			 -- if there are no more reads left then execute the next instruction or go idle
			 if(reg.read_count = "00000000000") then
				-- if this is the last instruction, idle and wait to start over
				if(reg.insn(35) = '1') then -- last instruction.
					ci.last_s1 := '1';
					ci.state := state_idle;
				else
					ci.state := state_delay;
				end if;
			 else				
				-- increment low part of DV address
			   ci.next_addr := std_logic_vector(unsigned(reg.next_addr) + "00000000001");
			   -- decrement read count
				ci.read_count := reg.read_count - "00000000001";
			 end if;
      end case;
    end if;
    
	 -- in every state, carry through pipeline registers
	 ci.last_s2 := reg.last_s1;
	 ci.last_s3 := reg.last_s2;
	 ci.last := reg.last_s3;
	 
	 ci.valid_s2 := reg.valid_s1;
	 ci.valid_s3 := reg.valid_s2;
	 ci.valid := reg.valid_s3;
	 
	 -- in every state, if we see valid data, then update channel_data and channel_we
	 if(reg.valid = '1') then
		ci.channel_data := dv_data;
		ci.channel_we := '1';
	 end if;
	 
	 -- in every state, if we just read the last (pipeline-delayed) instruction, indicate that we are done
	 if(reg.last = '1') then
		ci.done := '1';
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
