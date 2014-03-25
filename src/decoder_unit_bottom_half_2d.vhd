library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity decoder_unit_bottom_half_2d is generic (
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
    pc7: in std_logic_vector(11 downto 0);
    pc8: in std_logic_vector(11 downto 0);
    pc9: in std_logic_vector(11 downto 0);
    pc10: in std_logic_vector(11 downto 0);
    pc11: in std_logic_vector(11 downto 0);
    pc12: in std_logic_vector(11 downto 0);
    pc13: in std_logic_vector(11 downto 0);
    pc14: in std_logic_vector(11 downto 0);
    pc15: in std_logic_vector(11 downto 0);
    pc_ready: in std_logic;
    pc_ack: out std_logic;
    
    prog_ok: in std_logic;
    prog_addr: in std_logic_vector(5 downto 0); -- top 2 bits select which of the 4 DVs are being decoded; bottom 4 choose a decoder buffer for that DV
    prog_we: in std_logic;
    prog_data: in std_logic_vector(11 downto 0);
    
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
    
); end entity decoder_unit_bottom_half_2d;

architecture rtl of decoder_unit_bottom_half_2d is
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
           
   type decoder_coefficient_bank is array(0 to 15) of std_logic_vector(11 downto 0);
   type decoder_bank_type is array(0 to 3) of decoder_coefficient_bank;
   signal decoder_bank: decoder_bank_type;
   signal decoder_bank_feedback: decoder_bank_type;
   signal decoder_bank_we: std_logic_vector(63 downto 0);
   signal decoder_ack: std_logic_vector(3 downto 0);
   signal decoder_done: std_logic_vector(3 downto 0);
   
   type decoder_addr_type is array(0 to 3) of std_logic_vector(9 downto 0);
	type decoder_data_type is array(0 to 3) of std_logic_vector(11 downto 0);
	type decoder_sfixed_data_type is array(0 to 3) of sfixed(1 downto -10);
	
   signal decoder0_dv_addr: decoder_addr_type;
   signal decoder0_dv_we: std_logic_vector(3 downto 0);
   signal decoder0_dv_data: decoder_data_type;
	signal decoder0_sfixed_dv_data: decoder_sfixed_data_type;
	
	signal decoder1_dv_addr: decoder_addr_type;
   signal decoder1_dv_we: std_logic_vector(3 downto 0);
   signal decoder1_dv_data: decoder_data_type;
	signal decoder1_sfixed_dv_data: decoder_sfixed_data_type;
   
	component pipelined_adder port (
		 clk: in std_logic;
		 rst: in std_logic;
		 
		 a: in sfixed(1 downto -10);
		 a_valid: in std_logic;
		 b: in sfixed(1 downto -10);
		 b_valid: in std_logic;
		 sum: out sfixed(1 downto -10);
		 sum_ready: out std_logic;
		 sum_ack: in std_logic
	); end component;
	signal final_ready: std_logic_vector(3 downto 0);
	signal last_final_ready: std_logic_vector(3 downto 0);
	
	signal final_dv_we: std_logic_vector(3 downto 0);
	signal final_dv_data: decoder_data_type;
	signal final_sfixed_dv_data: decoder_sfixed_data_type;
	
   signal shreg_ready: std_logic_vector(31 downto 0); -- convenience. all shregs will finish simultaneously
	
begin

    DECODERS: for I in 0 to 3 generate
        FIFOS: for J in 0 to 15 generate
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
        
        DECODER0: programmable_decoder_1d generic map (
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
            pc7_data => pc7,
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
            dv_addr => decoder0_dv_addr(I),
            dv_we => decoder0_dv_we(I),
            dv_data => decoder0_dv_data(I),
            timestep => timestep,
            all_done => decoder_done(I)
        );
		  DECODER1: programmable_decoder_1d generic map (
            shift => shift
        ) port map (
            clk => clk,
            rst => rst,
            pc0_data => pc8,
            pc1_data => pc9,
            pc2_data => pc10,
            pc3_data => pc11,
            pc4_data => pc12,
            pc5_data => pc13,
            pc6_data => pc14,
            pc7_data => pc15,
            pc_ready => pc_ready,
            v0 => decoder_bank(I)(8),
            v1 => decoder_bank(I)(9),
            v2 => decoder_bank(I)(10),
            v3 => decoder_bank(I)(11),
            v4 => decoder_bank(I)(12),
            v5 => decoder_bank(I)(13),
            v6 => decoder_bank(I)(14),
            v7 => decoder_bank(I)(15),
            shreg_ready => pc_ready, -- even bigger cheating, remove this signal
            --data_ack => decoder_ack(I),
				data_ack => open,
            dv_addr => decoder1_dv_addr(I),
            dv_we => decoder1_dv_we(I),
            dv_data => decoder1_dv_data(I),
            timestep => timestep,
            --all_done => decoder_done(I)
				all_done => open
        );                
    end generate;
	 
	 FINAL_ADDER_STAGE: for I in 0 to 3 generate

		decoder0_sfixed_dv_data(I) <= to_sfixed(decoder0_dv_data(I), 1,-10);
		decoder1_sfixed_dv_data(I) <= to_sfixed(decoder1_dv_data(I), 1,-10);
		final_dv_data(I) <= to_slv(final_sfixed_dv_data(I));
	 
		FINAL_ADDER: pipelined_adder port map (
			clk => clk,
			rst => rst,
			
			a => decoder0_sfixed_dv_data(I),
			a_valid => decoder0_dv_we(I),
			b => decoder1_sfixed_dv_data(I),
			b_valid => decoder1_dv_we(I),
			
			sum => final_sfixed_dv_data(I),
			sum_ready => final_ready(I),
			sum_ack => final_dv_we(I)
		);
		EDGE_DETECTOR: process(clk, rst, final_ready, last_final_ready)
		begin
			if(rst = '1') then
				last_final_ready(I) <= '0';
				final_dv_we(I) <= '0';
			elsif(rising_edge(clk)) then
				if(last_final_ready(I) = '0' and final_ready(I) = '1') then
					final_dv_we(I) <= '1';
				else
					final_dv_we(I) <= '0';
				end if;			
				last_final_ready(I) <= final_ready(I);
			end if;
		end process EDGE_DETECTOR;
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
    dv0_addr(9 downto 0) <= decoder0_dv_addr(0); 
    dv1_addr(9 downto 0) <= decoder0_dv_addr(1); 
    dv2_addr(9 downto 0) <= decoder0_dv_addr(2); 
    dv3_addr(9 downto 0) <= decoder0_dv_addr(3);    
    dv0_we <= final_dv_we(0);
    dv1_we <= final_dv_we(1);
    dv2_we <= final_dv_we(2);
    dv3_we <= final_dv_we(3);
    dv0_data <= final_dv_data(0);
    dv1_data <= final_dv_data(1);
    dv2_data <= final_dv_data(2);
    dv3_data <= final_dv_data(3);

end architecture rtl;
