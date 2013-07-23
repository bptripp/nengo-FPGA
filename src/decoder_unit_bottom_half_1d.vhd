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
    pc7: in std_logic_vector(11 downto 0);
    pc_ready: in std_logic;
    pc_ack: out std_logic;
    
    -- shift registers    
    shreg_data: in std_logic_vector(31 downto 0); -- 8 coefficients per decoder * 4 decoders
    shreg_shift: in std_logic;
    shreg_clear: in std_logic;
    --shreg_ready: in std_logic;
    shreg_ack: out std_logic;
    
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
    
    component delayed_shift_register     generic (
        N: integer; -- output width
        T: integer -- number of shifts to ignore
    );
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           din : in STD_LOGIC;
           shift : in STD_LOGIC;         
           dout : out STD_LOGIC_VECTOR (N-1 downto 0);
           done: out std_logic
           ); end component delayed_shift_register;
           
   type decoder_coefficient_bank is array(0 to 7) of std_logic_vector(11 downto 0);
   type decoder_bank_type is array(0 to 3) of decoder_coefficient_bank;
   signal decoder_bank: decoder_bank_type;
   signal decoder_ack: std_logic_vector(3 downto 0);
   signal decoder_done: std_logic_vector(3 downto 0);
   
   type decoder_addr_type is array(0 to 3) of std_logic_vector(9 downto 0);
   signal decoder_dv_addr: decoder_addr_type;
   signal decoder_dv_we: std_logic_vector(3 downto 0);
   type decoder_data_type is array(0 to 3) of std_logic_vector(11 downto 0);
   signal decoder_dv_data: decoder_data_type;
   
   signal shreg_ready: std_logic_vector(31 downto 0); -- convenience. all shregs will finish simultaneously
begin

    DECODERS: for I in 0 to 3 generate
        SHIFT_REGISTERS: for J in 0 to 7 generate
            SHREG: delayed_shift_register generic map (
                N => 12,
                T => skip_count
            ) port map (
                clk => clk,
                rst => shreg_clear,
                din => shreg_data(8*I+J),
                shift => shreg_shift,
                dout => decoder_bank(I)(J),
                done => shreg_ready(8*I+J)
            );
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
            shreg_ready => shreg_ready(0), -- cheating, as mentioned above
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
    shreg_ack <= decoder_ack(0); -- I thiiiink this is what I want here
    
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
