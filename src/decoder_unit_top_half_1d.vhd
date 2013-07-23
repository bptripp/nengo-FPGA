library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity decoder_unit_top_half_1d is port (
    clk: in std_logic;
    rst: in std_logic;    
    encoder_fifo_u: in std_logic_vector(11 downto 0);
    encoder_fifo_empty: in std_logic;
    encoder_fifo_rd_en: out std_logic;
    pc0: out std_logic_vector(11 downto 0);
    pc1: out std_logic_vector(11 downto 0);
    pc2: out std_logic_vector(11 downto 0);
    pc3: out std_logic_vector(11 downto 0);
    pc4: out std_logic_vector(11 downto 0);
    pc5: out std_logic_vector(11 downto 0);
    pc6: out std_logic_vector(11 downto 0);
    pc7: out std_logic_vector(11 downto 0);
    pc_valid: out std_logic;
    pc_ack: in std_logic;
    
    -- programming interface
    pc_prog_addr: in std_logic_vector(13 downto 0); -- top bit unused (due to only 7 PCs); 12 downto 10 chooses a PC; 9 downto 0 addresses in PC
    pc_prog_we: in std_logic;
    pc_prog_data: in std_logic_vector(11 downto 0);
    
    normal_prog_addr: in std_logic_vector(1 downto 0);
    normal_prog_we: in std_logic;
    normal_prog_data: in std_logic_vector(31 downto 0)
); end entity decoder_unit_top_half_1d;

architecture rtl of decoder_unit_top_half_1d is
    type state_type is (state_wait_for_fifo, state_wait_for_pc, state_wait_for_next_stage_ack);
    type ci_type is record
        state: state_type;
        fifo_rd: std_logic;
        pc_input: std_logic_vector(11 downto 0);
        pc0: std_logic_vector(11 downto 0);
        pc1: std_logic_vector(11 downto 0);
        pc2: std_logic_vector(11 downto 0);
        pc3: std_logic_vector(11 downto 0);
        pc4: std_logic_vector(11 downto 0);
        pc5: std_logic_vector(11 downto 0);
        pc6: std_logic_vector(11 downto 0);
        pc7: std_logic_vector(11 downto 0);
        pc_valid: std_logic;
    end record;
    constant reg_reset: ci_type := (
        state => state_wait_for_fifo,
        fifo_rd => '0',
        pc_input => X"000",
        pc0 => X"000",
        pc1 => X"000",
        pc2 => X"000",
        pc3 => X"000",
        pc4 => X"000",
        pc5 => X"000",
        pc6 => X"000",
        pc7 => X"000",
        pc_valid => '0'
    );
    signal reg: ci_type := reg_reset;
    signal ci_next: ci_type; 
    
    component principal_component_1d generic (
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
           
           ); end component;
    
    type pc_data_array_type is array(0 to 7) of std_logic_vector(11 downto 0);
    signal pc_data: pc_data_array_type;
    signal pc_ready: std_logic_vector(6 downto 0);
    signal pc_outputs_ready: std_logic;
    
    signal pc_prog_cs: std_logic_vector(6 downto 0);
    
    component normal Port ( 
           clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           q : out signed (15 downto 0);
           prog_addr: in std_logic_vector(1 downto 0);
           prog_we: in std_logic;
           prog_data: in std_logic_vector(31 downto 0)
           ); end component;
    signal normal_out: signed(15 downto 0);
    signal normal_out_sfixed : sfixed(1 downto -14);
    
    component bandpass Port ( 
           clk : in  STD_LOGIC;
           valid : in  STD_LOGIC;
           u : in  SFIXED (1 downto -14);
           y : out  SFIXED (1 downto -14)); end component;    
    signal h3_output: sfixed(1 downto -14);
    
begin

    encoder_fifo_rd_en <= reg.fifo_rd;
    pc0 <= reg.pc0;
    pc1 <= reg.pc1;
    pc2 <= reg.pc2;
    pc3 <= reg.pc3;
    pc4 <= reg.pc4;
    pc5 <= reg.pc5;
    pc6 <= reg.pc6;
    pc7 <= reg.pc7;
    pc_valid <= reg.pc_valid;

    COMB: process(reg, rst, encoder_fifo_u, encoder_fifo_empty, 
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
                    if(encoder_fifo_empty = '0') then
                        ci.fifo_rd := '1';
                        ci.pc_input := encoder_fifo_u;
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
        case pc_prog_addr(12 downto 10) is
            when "000" =>
                pc_prog_cs <= "0000001";        
            when "001" =>
                pc_prog_cs <= "0000010"; 
            when "010" =>
                pc_prog_cs <= "0000100"; 
            when "011" =>
                pc_prog_cs <= "0001000"; 
            when "100" =>
                pc_prog_cs <= "0010000"; 
            when "101" =>
                pc_prog_cs <= "0100000";
            when "110" =>
                pc_prog_cs <= "1000000";                                                                                                                                                                                                                                                                                  
            when others =>
                pc_prog_cs <= "0000000";
        end case;
    end process;
    
    -- BLATANT CHEATING
    pc_outputs_ready <= pc_ready(0);
    
    PRINCIPAL_COMPONENTS: for I in 0 to 6 generate
        PC: principal_component_1d generic map (loadfile => "pc_blank.rom") port map (
            clk => clk,
            addr => reg.pc_input(11 downto 2),
            valid => reg.fifo_rd,
            data => pc_data(I),
            ready => pc_ready(I),
        
            prog_addr => pc_prog_addr(9 downto 0),
            prog_cs => pc_prog_cs(I),
            prog_we => pc_prog_we,
            prog_data => pc_prog_data
        );
    end generate;
    
    NORMAL_GEN: normal port map (
        clk => clk,
        rst => rst,
        q => normal_out,
        prog_addr => normal_prog_addr,
        prog_we => normal_prog_we,
        prog_data => normal_prog_data
    );
    
    normal_out_sfixed <= to_sfixed(std_logic_vector(normal_out), 1,-14);
    H3: bandpass port map (
        clk => clk,
        valid => '1', -- CHEATING: sample 100% of the time
        u => normal_out_sfixed,
        y => h3_output
    );
    pc_data(7) <= to_slv(normal_out_sfixed)(15 downto 4);

end architecture rtl;