library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library IEEE_proposed;
use IEEE_proposed.fixed_pkg.all;
use IEEE_proposed.fixed_float_types.all; -- for fixed_truncate

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity variable_coefficient_decoder is
    generic (
        shift: integer := 0        
    );
    Port ( clk : in STD_LOGIC;
           pc_output_ready : in STD_LOGIC;
           pc0_data: in std_logic_vector(11 downto 0);
           pc1_data: in std_logic_vector(11 downto 0);
           pc2_data: in std_logic_vector(11 downto 0);
           pc3_data: in std_logic_vector(11 downto 0);
           pc4_data: in std_logic_vector(11 downto 0);
           pc5_data: in std_logic_vector(11 downto 0);
           pc6_data: in std_logic_vector(11 downto 0);
           normal_data: in std_logic_vector(11 downto 0);
           
           v0: sfixed(1 downto -10);
           v1: sfixed(1 downto -10);
           v2: sfixed(1 downto -10);
           v3: sfixed(1 downto -10);
           v4: sfixed(1 downto -10);
           v5: sfixed(1 downto -10);
           v6: sfixed(1 downto -10);
           vr: sfixed(1 downto -10); -- for Gaussian noise
           
           valid: out std_logic;
           decoded_value: out sfixed(1 downto -10)                   
           );
end variable_coefficient_decoder;

architecture Behavioral of variable_coefficient_decoder is

    attribute dont_touch: string;

    constant pc_data_high: integer := 1 + shift;
    constant pc_data_low: integer := -10 + shift;

    type decoder_intermediate_type is array(0 to 7) of sfixed(
     sfixed_high(pc_data_high, pc_data_low, '*', 1,-10)
     downto
     sfixed_low(pc_data_high, pc_data_low, '*', 1,-10) 
     ); -- was (3 downto -20)
    signal decode_intermediate: decoder_intermediate_type;
    signal decode_stage1_25_valid: std_logic := '0';
    
    signal decode_stage1_25: decoder_intermediate_type;
    signal decode_stage1_5_valid: std_logic := '0';
    
    signal decode_stage1_5: decoder_intermediate_type;    
    signal decode_stage2_valid: std_logic := '0';
    
    type decoder_stage2_type is array(0 to 3) of sfixed(
     sfixed_high(decode_intermediate(0), '+', decode_intermediate(1)) 
     downto 
     sfixed_low(decode_intermediate(0), '+', decode_intermediate(1))
     ); -- FIXME try maintaining less precision for this, was (4 downto -20)
    signal decode_stage2: decoder_stage2_type; attribute dont_touch of decode_stage2: signal is "true";
    signal decode_stage3_valid: std_logic := '0';
    
    type decoder_stage3_type is array(0 to 1) of sfixed(
     sfixed_high(decode_stage2(0), '+', decode_stage2(1)) 
     downto 
     sfixed_low(decode_stage2(0), '+', decode_stage2(1))
     ); -- FIXME try maintaining less precision for this, was (5 downto -20)
    signal decode_stage3: decoder_stage3_type;
    signal decode_stage4_valid: std_logic := '0';
    
    signal decoded_value_reg: std_logic_vector(11 downto 0) := X"000";
begin

    DECODE: process(clk, pc_output_ready, pc0_data, pc1_data, pc2_data, pc3_data, pc4_data, pc5_data, pc6_data, normal_data, 
        decode_intermediate, decode_stage1_25_valid,
        decode_stage1_25, decode_stage1_5_valid, 
        decode_stage1_5, decode_stage2_valid,
        decode_stage2, decode_stage3_valid,
        decode_stage3, decode_stage4_valid)
        
        variable decoded_value_tmp: sfixed(1 downto -10);
    begin
            
        decoded_value_tmp := resize(decode_stage3(0) + decode_stage3(1), 1,-10, fixed_saturate, fixed_truncate);
    
        if(rising_edge(clk)) then
            -- STAGE 1
            decode_intermediate(0) <= to_sfixed(pc0_data, pc_data_high, pc_data_low) * v0; -- this should implement the shift for free...
            decode_intermediate(1) <= to_sfixed(pc1_data, pc_data_high, pc_data_low) * v1;
            decode_intermediate(2) <= to_sfixed(pc2_data, pc_data_high, pc_data_low) * v2;
            decode_intermediate(3) <= to_sfixed(pc3_data, pc_data_high, pc_data_low) * v3;
            decode_intermediate(4) <= to_sfixed(pc4_data, pc_data_high, pc_data_low) * v4;
            decode_intermediate(5) <= to_sfixed(pc5_data, pc_data_high, pc_data_low) * v5;
            decode_intermediate(6) <= to_sfixed(pc6_data, pc_data_high, pc_data_low) * v6;
            decode_intermediate(7) <= to_sfixed(normal_data, pc_data_high, pc_data_low) * vr; -- FIXME is this correct?           
            
--            decode_stage1_25_valid <= bf_output_ready;
            decode_stage2_valid <= pc_output_ready;
            
            -- STAGE 1.25 (extra register stage)
            
--            decode_y_stage1_25 <= decode_y_intermediate;
--            decode_x_stage1_25 <= decode_x_intermediate;
--            decode_stage1_5_valid <= decode_stage1_25_valid;
            
            -- STAGE 1.5 (extra register stage)
--            decode_y_stage1_5 <= decode_y_stage1_25;
--          decode_x_stage1_5 <= decode_x_stage1_25;
--          decode_stage2_valid <= decode_stage1_5_valid;
            
            -- STAGE 2
--          decode_y_stage2(0) <= decode_y_stage1_5(0) + decode_y_stage1_5(1);
--          decode_y_stage2(1) <= decode_y_stage1_5(2) + decode_y_stage1_5(3);
--          decode_y_stage2(2) <= decode_y_stage1_5(4) + decode_y_stage1_5(5);
--          decode_y_stage2(3) <= decode_y_stage1_5(6) + decode_y_stage1_5(7);
            
--          decode_x_stage2(0) <= decode_x_stage1_5(0) + decode_x_stage1_5(1);
--          decode_x_stage2(1) <= decode_x_stage1_5(2) + decode_x_stage1_5(3);
--          decode_x_stage2(2) <= decode_x_stage1_5(4) + decode_x_stage1_5(5);
--          decode_x_stage2(3) <= decode_x_stage1_5(6) + decode_x_stage1_5(7);

            decode_stage2(0) <= decode_intermediate(0) + decode_intermediate(1);
            decode_stage2(1) <= decode_intermediate(2) + decode_intermediate(3);
            decode_stage2(2) <= decode_intermediate(4) + decode_intermediate(5);
            decode_stage2(3) <= decode_intermediate(6) + decode_intermediate(7);
            
            decode_stage3_valid <= decode_stage2_valid;
            
            -- STAGE 3
            decode_stage3(0) <= decode_stage2(0) + decode_stage2(1);
            decode_stage3(1) <= decode_stage2(2) + decode_stage2(3);
            
            decode_stage4_valid <= decode_stage3_valid;
            
            -- STAGE LAST
            valid <= decode_stage4_valid;
            if(decode_stage4_valid = '1') then
              decoded_value_reg <= to_slv(decoded_value_tmp);                     
            end if;
        end if;
    end process DECODE;
    
    decoded_value <= to_sfixed(decoded_value_reg, 1,-10);

end Behavioral;
