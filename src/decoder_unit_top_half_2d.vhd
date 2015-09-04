library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity decoder_unit_top_half_2d is port (
    clk: in std_logic;
    rst: in std_logic;    
	 
    x_encoder_fifo: in std_logic_vector(11 downto 0);
    x_encoder_fifo_empty: in std_logic;
    x_encoder_fifo_rd_en: out std_logic;
	 
	 y_encoder_fifo: in std_logic_vector(11 downto 0);
    y_encoder_fifo_empty: in std_logic;
    y_encoder_fifo_rd_en: out std_logic;
	 
    pc0: out std_logic_vector(11 downto 0);
    pc1: out std_logic_vector(11 downto 0);
    pc2: out std_logic_vector(11 downto 0);
    pc3: out std_logic_vector(11 downto 0);
    pc4: out std_logic_vector(11 downto 0);
    pc5: out std_logic_vector(11 downto 0);
    pc6: out std_logic_vector(11 downto 0);
    pc7: out std_logic_vector(11 downto 0);
	 pc8: out std_logic_vector(11 downto 0);
    pc9: out std_logic_vector(11 downto 0);
    pc10: out std_logic_vector(11 downto 0);
    pc11: out std_logic_vector(11 downto 0);
    pc12: out std_logic_vector(11 downto 0);
    pc13: out std_logic_vector(11 downto 0);
    pc14: out std_logic_vector(11 downto 0);
    --pc15: out std_logic_vector(11 downto 0); -- moved to bottom half
    pc_valid: out std_logic;
    pc_ack: in std_logic;
    
    -- programming interface
    pc_prog_addr: in std_logic_vector(13 downto 0); -- 13 downto 10 chooses a PC; 9 downto 0 addresses in PC
    pc_prog_we: in std_logic;
    pc_prog_data: in std_logic_vector(11 downto 0)
); end entity decoder_unit_top_half_2d;

architecture rtl of decoder_unit_top_half_2d is

    type state_type is (state_wait_for_fifo, state_wait_for_pc, state_wait_for_next_stage_ack);
    type ci_type is record
        state: state_type;
        fifo_rd: std_logic;
        pc_input_x: std_logic_vector(11 downto 0);
		  pc_input_y: std_logic_vector(11 downto 0);
        pc0: std_logic_vector(11 downto 0);
        pc1: std_logic_vector(11 downto 0);
        pc2: std_logic_vector(11 downto 0);
        pc3: std_logic_vector(11 downto 0);
        pc4: std_logic_vector(11 downto 0);
        pc5: std_logic_vector(11 downto 0);
        pc6: std_logic_vector(11 downto 0);
        pc7: std_logic_vector(11 downto 0);
        pc8: std_logic_vector(11 downto 0);
        pc9: std_logic_vector(11 downto 0);
        pc10: std_logic_vector(11 downto 0);
        pc11: std_logic_vector(11 downto 0);
        pc12: std_logic_vector(11 downto 0);
        pc13: std_logic_vector(11 downto 0);
        pc14: std_logic_vector(11 downto 0);
        pc_valid: std_logic;
    end record;
    constant reg_reset: ci_type := (
        state => state_wait_for_fifo,
        fifo_rd => '0',
        pc_input_x => X"000",
        pc_input_y => X"000",
        pc0 => X"000",
        pc1 => X"000",
        pc2 => X"000",
        pc3 => X"000",
        pc4 => X"000",
        pc5 => X"000",
        pc6 => X"000",
        pc7 => X"000",
        pc8 => X"000",
        pc9 => X"000",
        pc10 => X"000",
        pc11 => X"000",
        pc12 => X"000",
        pc13 => X"000",
        pc14 => X"000",
        pc_valid => '0'
    );
    signal reg: ci_type := reg_reset;
    signal ci_next: ci_type; 
	 
    component principal_component_2d generic (
        loadfile: string
    );
    Port ( clk : in STD_LOGIC;
           x_in : in STD_LOGIC_VECTOR (11 downto 0);
           y_in: in std_logic_vector(11 downto 0);
           data : out STD_LOGIC_VECTOR (11 downto 0);
           ready : in STD_LOGIC;
           valid : out STD_LOGIC;
           
           prog_addr: in std_logic_vector(9 downto 0);
           prog_cs: in std_logic;
           prog_we: in std_logic;
           prog_data: in std_logic_vector(11 downto 0)
           ); end component principal_component_2d;
    
    type pc_data_array_type is array(0 to 15) of std_logic_vector(11 downto 0);
    signal pc_data: pc_data_array_type;
    signal pc_ready: std_logic_vector(14 downto 0);
    signal pc_outputs_ready: std_logic;
    
    signal pc_prog_cs: std_logic_vector(14 downto 0);
    
begin

    x_encoder_fifo_rd_en <= reg.fifo_rd;
    y_encoder_fifo_rd_en <= reg.fifo_rd;
    pc0 <= reg.pc0;
    pc1 <= reg.pc1;
    pc2 <= reg.pc2;
    pc3 <= reg.pc3;
    pc4 <= reg.pc4;
    pc5 <= reg.pc5;
    pc6 <= reg.pc6;
    pc7 <= reg.pc7;
    pc8 <= reg.pc8;
    pc9 <= reg.pc9;
    pc10 <= reg.pc10;
    pc11 <= reg.pc11;
    pc12 <= reg.pc12;
    pc13 <= reg.pc13;
    pc14 <= reg.pc14;
    pc_valid <= reg.pc_valid;

    COMB: process(reg, rst, x_encoder_fifo, x_encoder_fifo_empty, y_encoder_fifo, y_encoder_fifo_empty,
        pc_data, pc_outputs_ready,
        pc_ack)
        variable ci: ci_type;
    begin
        ci := reg;
        -- self-clearing
        ci.fifo_rd := '0';
        if(rst = '1') then
            ci := reg_reset;
        else
            case reg.state is
                when state_wait_for_fifo =>
                    if(x_encoder_fifo_empty = '0' and y_encoder_fifo_empty = '0') then
                        ci.fifo_rd := '1';
                        ci.pc_input_x := x_encoder_fifo;
								ci.pc_input_y := y_encoder_fifo;
                        ci.state := state_wait_for_pc;
                    end if;
                when state_wait_for_pc =>
                    if(pc_outputs_ready = '1') then
                        ci.pc0 := pc_data(0);
                        ci.pc1 := pc_data(1);
                        ci.pc2 := pc_data(2);
                        ci.pc3 := pc_data(3);
                        ci.pc4 := pc_data(4);
                        ci.pc5 := pc_data(5);
                        ci.pc6 := pc_data(6);
                        ci.pc7 := pc_data(7);
                        ci.pc8 := pc_data(8);
                        ci.pc9 := pc_data(9);
                        ci.pc10 := pc_data(10);
                        ci.pc11 := pc_data(11);
                        ci.pc12 := pc_data(12);
                        ci.pc13 := pc_data(13);
                        ci.pc14 := pc_data(14);
                        ci.pc_valid := '1';
                        ci.state := state_wait_for_next_stage_ack;
                    end if;
                when state_wait_for_next_stage_ack =>
                    if(pc_ack = '1') then
                        ci.pc_valid := '0';
                        ci.state := state_wait_for_fifo;
                    end if;
            end case;
        end if;
        ci_next <= ci;
    end process COMB;
    
    SEQ: process(clk, ci_next)
    begin
        if(rising_edge(clk)) then
            reg <= ci_next;
        end if;
    end process SEQ;        

    DECODE_PC_PROG: process(pc_prog_addr)
    begin
        case pc_prog_addr(13 downto 10) is
            when "0000" =>
                pc_prog_cs <= "000000000000001";        
            when "0001" =>
                pc_prog_cs <= "000000000000010"; 
            when "0010" =>
                pc_prog_cs <= "000000000000100"; 
            when "0011" =>
                pc_prog_cs <= "000000000001000"; 
            when "0100" =>
                pc_prog_cs <= "000000000010000"; 
            when "0101" =>
                pc_prog_cs <= "000000000100000";
            when "0110" =>
                pc_prog_cs <= "000000001000000";
				when "0111" =>
                pc_prog_cs <= "000000010000000";
					 --here
            when "1000" =>
                pc_prog_cs <= "000000100000000";        
            when "1001" =>
                pc_prog_cs <= "000001000000000"; 
            when "1010" =>
                pc_prog_cs <= "000010000000000"; 
            when "1011" =>
                pc_prog_cs <= "000100000000000"; 
            when "1100" =>
                pc_prog_cs <= "001000000000000"; 
            when "1101" =>
                pc_prog_cs <= "010000000000000";
            when "1110" =>
                pc_prog_cs <= "100000000000000";				
            when others =>
                pc_prog_cs <= "000000000000000";
        end case;
    end process;
    
    -- BLATANT CHEATING
    pc_outputs_ready <= pc_ready(0);
    
    PRINCIPAL_COMPONENTS: for I in 0 to 14 generate
        PC: principal_component_2d generic map (loadfile => "pc_blank.rom") port map (
            clk => clk,
            x_in => reg.pc_input_x,
				y_in => reg.pc_input_y,
            ready => reg.fifo_rd,
            data => pc_data(I),
            valid => pc_ready(I),
        
            prog_addr => pc_prog_addr(9 downto 0),
            prog_cs => pc_prog_cs(I),
            prog_we => pc_prog_we,
            prog_data => pc_prog_data
        );
    end generate;

end architecture rtl;
