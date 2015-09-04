library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity decoder_unit_bottom_half_1d is generic (
    shift: integer := 0;
    skip_count: integer -- for shift register
); port (
    clk: in std_logic;
    rst: in std_logic;
    
    -- from top-half
    pc0: in std_logic_vector(11 downto 0);
    pc1: in std_logic_vector(11 downto 0);
    pc2: in std_logic_vector(11 downto 0);
    pc3: in std_logic_vector(11 downto 0);
    pc4: in std_logic_vector(11 downto 0);
    pc5: in std_logic_vector(11 downto 0);
    pc6: in std_logic_vector(11 downto 0);
    --pc7: in std_logic_vector(11 downto 0); -- normal gen now integrated here
    pc_ready: in std_logic;
    pc_ack: out std_logic;
    
    prog_ok: in std_logic;
    prog_addr: in std_logic_vector(5 downto 0); -- top 2 bits select which of the 4 DVs are being decoded; bottom 4 choose a decoder buffer for that DV
    prog_we: in std_logic;
    prog_data: in std_logic_vector(11 downto 0);
	 
    normal_prog_addr: in std_logic_vector(3 downto 0); -- top 2 bits select a DV; bottom 2 bits select an LFSR
    normal_prog_we: in std_logic;
    normal_prog_data: in std_logic_vector(31 downto 0);
    
    -- DV write ports
    dv0_addr: out std_logic_vector(10 downto 0);
    dv0_we: out std_logic;
    dv0_data: out std_logic_vector(11 downto 0);
    dv1_addr: out std_logic_vector(10 downto 0);
    dv1_we: out std_logic;
    dv1_data: out std_logic_vector(11 downto 0);
    dv2_addr: out std_logic_vector(10 downto 0);
    dv2_we: out std_logic;
    dv2_data: out std_logic_vector(11 downto 0);
    dv3_addr: out std_logic_vector(10 downto 0);
    dv3_we: out std_logic;
    dv3_data: out std_logic_vector(11 downto 0);
    
    timestep: in std_logic;
    all_done: out std_logic
    
); end entity decoder_unit_bottom_half_1d;

architecture rtl of decoder_unit_bottom_half_1d is
    component programmable_decoder_1d generic (
        shift: integer := 0 -- for the actual decoder. FIXME can this really be a compile-time constant?
    ); 
    port (
    clk: in std_logic;
    rst: in std_logic;
    -- principal components from top-half
    pc0_data: in std_logic_vector(11 downto 0);
    pc1_data: in std_logic_vector(11 downto 0);
    pc2_data: in std_logic_vector(11 downto 0);
    pc3_data: in std_logic_vector(11 downto 0);
    pc4_data: in std_logic_vector(11 downto 0);
    pc5_data: in std_logic_vector(11 downto 0);
    pc6_data: in std_logic_vector(11 downto 0);
    pc7_data: in std_logic_vector(11 downto 0);
    pc_ready: in std_logic;
    
    -- decoder coefficients from shift register
    v0: in std_logic_vector(11 downto 0);
    v1: in std_logic_vector(11 downto 0);
    v2: in std_logic_vector(11 downto 0);
    v3: in std_logic_vector(11 downto 0);
    v4: in std_logic_vector(11 downto 0);
    v5: in std_logic_vector(11 downto 0);
    v6: in std_logic_vector(11 downto 0);
    v7: in std_logic_vector(11 downto 0);
    shreg_ready: in std_logic;
    
    data_ack: out std_logic; -- PC and shreg acknowledge strobe
    -- interface to DV double-buffer write port
    dv_addr: out std_logic_vector(9 downto 0); -- only 9 instead of 10, because a second decoder can use the other 1024 elements in the same block
    dv_we: out std_logic;
    dv_data: out std_logic_vector(11 downto 0);
    
    timestep: in std_logic;
    all_done: out std_logic        
    ); end component;
    
    component normal Port ( 
           clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           q : out signed (15 downto 0);
           prog_addr: in std_logic_vector(1 downto 0);
           prog_we: in std_logic;
           prog_data: in std_logic_vector(31 downto 0)
           ); end component;
    type normal_out_type is array(0 to 3) of signed(15 downto 0);
    type normal_out_sfixed_type is array(0 to 3) of sfixed(1 downto -14);
    signal normal_out: normal_out_type;
    signal normal_out_sfixed: normal_out_sfixed_type;

    signal normal_prog_cs: std_logic_vector(3 downto 0);
    signal normal_prog_cs_we: std_logic_vector(3 downto 0);
        
    component bandpass Port ( 
           clk : in  STD_LOGIC;
           valid : in  STD_LOGIC;
           u : in  SFIXED (1 downto -14);
           y : out  SFIXED (1 downto -14)); end component;
    type h3_output_type is array(0 to 3) of sfixed(1 downto -14);
    signal h3_output: h3_output_type;

    type pc7_data_type is array(0 to 3) of std_logic_vector(11 downto 0);
    signal pc7_data: pc7_data_type;
	 
   component decoder_coefficient_fifo   PORT (
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(11 DOWNTO 0);
      full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC
    ); end component;
    
   type decoder_coefficient_bank is array(0 to 7) of std_logic_vector(11 downto 0);
   type decoder_bank_type is array(0 to 3) of decoder_coefficient_bank;
   signal decoder_bank: decoder_bank_type;
   signal decoder_bank_feedback: decoder_bank_type;
   signal decoder_bank_we: std_logic_vector(63 downto 0);
   signal decoder_ack: std_logic_vector(3 downto 0);
   signal decoder_done: std_logic_vector(3 downto 0);
   
   type decoder_addr_type is array(0 to 3) of std_logic_vector(9 downto 0);
   signal decoder_dv_addr: decoder_addr_type;
   signal decoder_dv_we: std_logic_vector(3 downto 0);
   type decoder_data_type is array(0 to 3) of std_logic_vector(11 downto 0);
   signal decoder_dv_data: decoder_data_type;
   
   signal shreg_ready: std_logic_vector(31 downto 0); -- convenience. all shregs will finish simultaneously
begin

  DECODE_NORMAL_PROG: process(normal_prog_addr)
  begin
    case normal_prog_addr(3 downto 2) is
      when "00" =>
        normal_prog_cs <= "0001";
      when "01" =>
        normal_prog_cs <= "0010";
      when "10" =>
        normal_prog_cs <= "0100";
      when "11" =>
        normal_prog_cs <= "1000";
      when others =>
        normal_prog_cs <= "0000";
    end case;
  end process;
  normal_prog_cs_we(3) <= normal_prog_cs(3) and normal_prog_we;
  normal_prog_cs_we(2) <= normal_prog_cs(2) and normal_prog_we;
  normal_prog_cs_we(1) <= normal_prog_cs(1) and normal_prog_we;
  normal_prog_cs_we(0) <= normal_prog_cs(0) and normal_prog_we;


  DECODERS: for I in 0 to 3 generate
    NORMAL_GEN: normal port map (
        clk => clk,
        rst => rst,
        q => normal_out(I),
        prog_addr => normal_prog_addr(1 downto 0),
        prog_we => normal_prog_cs_we(I),
        prog_data => normal_prog_data
    );
    
    normal_out_sfixed(I) <= to_sfixed(std_logic_vector(normal_out(I)), 1,-14);
    H3: bandpass port map (
        clk => clk,
        valid => '1', -- CHEATING: sample 100% of the time
        u => normal_out_sfixed(I),
        y => h3_output(I)
    );
    --pc7_data(I) <= to_slv(normal_out_sfixed(I))(15 downto 4);
    pc7_data(I) <= to_slv(h3_output(I))(15 downto 4);
    
        FIFOS: for J in 0 to 7 generate
            FIFO: decoder_coefficient_fifo port map (
                clk => clk,
                rst => rst,
                din => decoder_bank_feedback(I)(J),
                wr_en => decoder_bank_we(16 * I + J),
                rd_en => decoder_ack(I),
                dout => decoder_bank(I)(J),
                full => open,
                empty => open
            );
            decoder_bank_feedback(I)(J) <= prog_data when prog_ok = '1' else decoder_bank(I)(J);
            decoder_bank_we(16 * I + J) <= prog_we when (prog_ok = '1' and 
                -- decode prog_addr: top 2 bits choose an I, bottom 4 bits choose a J
                to_integer(unsigned(prog_addr(5 downto 4))) = I and
                to_integer(unsigned(prog_addr(3 downto 0))) = J
            ) else decoder_ack(I);
        end generate;
        DRIVE_UNUSED_DECODERS: for J in 8 to 15 generate
            decoder_bank_we(16 * I + J) <= '0';
        end generate;
        
        DECODER: programmable_decoder_1d generic map (
            shift => shift
        ) port map (
            clk => clk,
            rst => rst,
            pc0_data => pc0,
            pc1_data => pc1,
            pc2_data => pc2,
            pc3_data => pc3,
            pc4_data => pc4,
            pc5_data => pc5,
            pc6_data => pc6,
            pc7_data => pc7_data(I),
            pc_ready => pc_ready,
            v0 => decoder_bank(I)(0),
            v1 => decoder_bank(I)(1),
            v2 => decoder_bank(I)(2),
            v3 => decoder_bank(I)(3),
            v4 => decoder_bank(I)(4),
            v5 => decoder_bank(I)(5),
            v6 => decoder_bank(I)(6),
            v7 => decoder_bank(I)(7),
            shreg_ready => pc_ready, -- even bigger cheating, remove this signal
            data_ack => decoder_ack(I),
            dv_addr => decoder_dv_addr(I),
            dv_we => decoder_dv_we(I),
            dv_data => decoder_dv_data(I),
            timestep => timestep,
            all_done => decoder_done(I)
        );        
    end generate;
    
    -- blatant cheating, but if all shregs finish simultaneously then this is okay
    pc_ack <= decoder_ack(0);
    --shreg_ack <= decoder_ack(0); -- I thiiiink this is what I want here
    all_done <= decoder_done(0);
    
    -- dv port wiring
    dv0_addr(10) <= '0';
    dv1_addr(10) <= '1';
    dv2_addr(10) <= '0';
    dv3_addr(10) <= '1';
    dv0_addr(9 downto 0) <= decoder_dv_addr(0); 
    dv1_addr(9 downto 0) <= decoder_dv_addr(1); 
    dv2_addr(9 downto 0) <= decoder_dv_addr(2); 
    dv3_addr(9 downto 0) <= decoder_dv_addr(3);    
    dv0_we <= decoder_dv_we(0);
    dv1_we <= decoder_dv_we(1);
    dv2_we <= decoder_dv_we(2);
    dv3_we <= decoder_dv_we(3);
    dv0_data <= decoder_dv_data(0);
    dv1_data <= decoder_dv_data(1);
    dv2_data <= decoder_dv_data(2);
    dv3_data <= decoder_dv_data(3);

end architecture rtl;
