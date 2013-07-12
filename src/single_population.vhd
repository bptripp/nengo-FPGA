----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/29/2013 03:50:21 PM
-- Design Name: 
-- Module Name: single_population - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
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

library std;
use std.textio.all;

library IEEE_proposed;
use IEEE_proposed.fixed_pkg.all;
use IEEE_proposed.fixed_float_types.all; -- for fixed_truncate

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity single_population is
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           ready: in std_logic;
           next_u : in STD_LOGIC_VECTOR (11 downto 0);
           y : out STD_LOGIC_VECTOR (11 downto 0);
           valid: out std_logic;
           
           pc_prog_sel: in std_logic_vector(2 downto 0);
           pc_prog_addr: in std_logic_vector(9 downto 0);
           pc_prog_we: in std_logic;
           pc_prog_data: in std_logic_vector(11 downto 0)
           );
end single_population;

architecture Behavioral of single_population is
    signal u_sgn: sfixed(1 downto -10);

    signal x: sfixed(1 downto -10) := X"000";
    signal y_fixed: sfixed(1 downto -10);
    
    constant wx1: sfixed(1 downto -10) := to_sfixed(1.0, 1,-10);
    constant wu1: sfixed(1 downto -10) := to_sfixed(0.1, 1,-10);
    
    signal x_int: sfixed(3 downto -20);
    signal u_int: sfixed(3 downto -20);
    signal ready_stall: std_logic := '0';    
    
    constant BYPASS_H1: integer := 0;
    signal h1_input: sfixed(1 downto -10);
    signal h1_input_valid: std_logic;
    component multiplexed_1filter Port ( 
               clk : in STD_LOGIC;
               rst: in std_logic;
               x : in sfixed (1 downto -10);
               u : in sfixed (1 downto -10);
               valid : in STD_LOGIC;
               x1 : out sfixed (1 downto -10);
               y : out sfixed (1 downto -10);
               ready : out STD_LOGIC; -- connect this to next stage data-valid
               ack: in std_logic; -- connect this to next stage data-acknowledge
               we: out std_logic -- connect this to FIFO acknowledge (assuming FWFT) and FIFO write-enable
               ); end component;
    signal h1_x: std_logic_vector(11 downto 0);
    signal h1_x_sfixed : sfixed(1 downto -10);
    signal h1_x1: sfixed(1 downto -10);
    signal h1_we: std_logic;
    component filter_state_fifo  PORT (
                 clk : IN STD_LOGIC;
                 rst : IN STD_LOGIC;
                 din : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
                 wr_en : IN STD_LOGIC;
                 rd_en : IN STD_LOGIC;
                 dout : OUT STD_LOGIC_VECTOR(11 DOWNTO 0);
                 full : OUT STD_LOGIC;
                 empty : OUT STD_LOGIC
               ); end component;
    signal h1_output: sfixed(1 downto -10);
    signal h1_output_ready: std_logic := '0';
    
    signal bf_addr: std_logic_vector(9 downto 0);
    signal bf_output_ready: std_logic := '0';   
    
    component principal_component_1d
    generic (
        loadfile: string
    );
    Port ( clk : in STD_LOGIC;
           addr : in STD_LOGIC_VECTOR (9 downto 0);
           data : out STD_LOGIC_VECTOR (11 downto 0);
                      
           prog_addr: in std_logic_vector(9 downto 0);
           prog_cs: in std_logic;
           prog_we: in std_logic;
           prog_data: in std_logic_vector(11 downto 0)
    ); end component principal_component_1d;
    signal pc0_cs: std_logic;
    signal pc1_cs: std_logic;
    signal pc2_cs: std_logic;
    signal pc3_cs: std_logic;
    signal pc4_cs: std_logic;
    signal pc5_cs: std_logic;
    signal pc6_cs: std_logic;
        
    signal bf0_data: std_logic_vector(11 downto 0);    
    signal bf1_data: std_logic_vector(11 downto 0);    
    signal bf2_data: std_logic_vector(11 downto 0);    
    signal bf3_data: std_logic_vector(11 downto 0);   
    signal bf4_data: std_logic_vector(11 downto 0);    
    signal bf5_data: std_logic_vector(11 downto 0);
    signal bf6_data: std_logic_vector(11 downto 0);
    
    component normal Port ( 
           clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           q : out signed (15 downto 0)); end component;
    signal normal_out: signed(15 downto 0);
    signal normal_out_sfixed : sfixed(1 downto -14);
    
    component bandpass Port ( 
           clk : in  STD_LOGIC;
           valid : in  STD_LOGIC;
           u : in  SFIXED (1 downto -14);
           y : out  SFIXED (1 downto -14)); end component;    
    signal h3_output: sfixed(1 downto -14);
    
    component fixed_coefficient_decoder generic (
        shift: integer := 0;
        v0: sfixed(1 downto -10);
        v1: sfixed(1 downto -10);
        v2: sfixed(1 downto -10);
        v3: sfixed(1 downto -10);
        v4: sfixed(1 downto -10);
        v5: sfixed(1 downto -10);
        v6: sfixed(1 downto -10);
        vr: sfixed(1 downto -10) -- for Gaussian noise
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
           
           valid: out std_logic;
           decoded_value: out sfixed(1 downto -10)                   
           ); end component fixed_coefficient_decoder;
    
--    constant vx0: sfixed(1 downto -10) := to_sfixed(0.16025, 1,-10);
--    constant vx1: sfixed(1 downto -10) := to_sfixed(-1.87415, 1,-10);
--    constant vx2: sfixed(1 downto -10) := to_sfixed(0.01965, 1,-10);
--    constant vx3: sfixed(1 downto -10) := to_sfixed(-0.06815, 1,-10);
--    constant vx4: sfixed(1 downto -10) := to_sfixed(-0.0181, 1,-10);
--    constant vx5: sfixed(1 downto -10) := to_sfixed(-0.02425, 1,-10);
--    constant vx6: sfixed(1 downto -10) := to_sfixed(-0.00245, 1,-10);
--    constant vxr: sfixed(1 downto -10) := to_sfixed(0.02, 1,-10); -- for Gaussian noise    

--    constant vy0: sfixed(1 downto -10) := to_sfixed(0.16025, 1,-10);
--    constant vy1: sfixed(1 downto -10) := to_sfixed(-1.87415, 1,-10);
--    constant vy2: sfixed(1 downto -10) := to_sfixed(0.01965, 1,-10);
--    constant vy3: sfixed(1 downto -10) := to_sfixed(-0.06815, 1,-10);
--    constant vy4: sfixed(1 downto -10) := to_sfixed(-0.0181, 1,-10);
--    constant vy5: sfixed(1 downto -10) := to_sfixed(-0.02425, 1,-10);
--    constant vy6: sfixed(1 downto -10) := to_sfixed(-0.00245, 1,-10);
--    constant vyr: sfixed(1 downto -10) := to_sfixed(0.02, 1,-10); -- for Gaussian noise
    
begin

    u_sgn <= to_sfixed(next_u, 1,-10);
             
    H1_REG: process(clk, x, u_sgn, ready, x_int, u_int, ready_stall)
        -- sfixed(a downto b) * sfixed(c downto d) = sfixed(a+c+1 downto b+d)
        variable x_tmp: sfixed(3 downto -20);
        variable u_tmp: sfixed(3 downto -20);
        -- sfixed(a downto b) + sfixed(c downto d) = sfixed(max(a,c) + 1 downto min(b,d))
        variable h1_tmp: sfixed(4 downto -20);
    begin
        --h1_tmp := resize(x * wx1 + u_sgn * wu1, h1_tmp);
        x_tmp := x * wx1;
        u_tmp := u_sgn * wu1;
        h1_tmp := x_int + u_int;
        if(rising_edge(clk)) then
            x_int <= x_tmp;
            u_int <= u_tmp;
            ready_stall <= ready;
            --h1_input <= resize(h1_tmp, h1_input);
            h1_input <= h1_tmp(1 downto -10); -- FIXME should be resized
            h1_input_valid <= ready_stall;
        end if;
    end process;
    
USE_H1: if (BYPASS_H1 = 0) generate
    h1_x_sfixed <= to_sfixed(h1_x, 1,-10);
    H1: multiplexed_1filter port map (
        clk => clk,
        rst => rst,
--        x => to_sfixed(h1_x, 1,-10),
        x => h1_x_sfixed,
        u => h1_input,
        valid => h1_input_valid,
        x1 => h1_x1,
        y => h1_output,
        ready => h1_output_ready,
        ack => h1_output_ready, -- automatic acknowledge for now.
        we => h1_we
    );
    H1_X_FIFO: filter_state_fifo port map (
        clk => clk,
        rst => rst,
        din => std_logic_vector(h1_x1),
        wr_en => h1_we,
        rd_en => h1_we,
        dout => h1_x,
        full => open,
        empty => open
    );
end generate USE_H1;

DONT_USE_H1: if (BYPASS_H1 = 1) generate
    h1_output <= h1_input;
    DELAY_H1_READY: process(clk, h1_input_valid)
    begin
        if(rising_edge(clk)) then            
            h1_output_ready <= h1_input_valid;
        end if;
    end process;
end generate DONT_USE_H1;
    
    BF_READY: process(clk, h1_output_ready)
    begin
        if(rising_edge(clk)) then
            bf_output_ready <= h1_output_ready; -- single-cycle read
        end if;
    end process;
    
    
    bf_addr <= to_slv(resize(h1_output, 1,-8, fixed_saturate, fixed_truncate)); -- drop lowest two bits
    
    -- decode chip-select signal for each principal component
    pc0_cs <= '1' when pc_prog_sel = "000" else '0';
    pc1_cs <= '1' when pc_prog_sel = "001" else '0';
    pc2_cs <= '1' when pc_prog_sel = "010" else '0';
    pc3_cs <= '1' when pc_prog_sel = "011" else '0';
    pc4_cs <= '1' when pc_prog_sel = "100" else '0';
    pc5_cs <= '1' when pc_prog_sel = "101" else '0';
    pc6_cs <= '1' when pc_prog_sel = "110" else '0';
        
    PC0: principal_component_1d generic map (
        loadfile => "bf0.rom"
    ) port map (
        clk => clk,
        addr => bf_addr,
        data => bf0_data,
        
        prog_addr => pc_prog_addr,
        prog_cs => pc0_cs,
        prog_we => pc_prog_we,
        prog_data => pc_prog_data
    );
    PC1: principal_component_1d generic map (
        loadfile => "bf1.rom"
    ) port map (
        clk => clk,
        addr => bf_addr,
        data => bf1_data,
                
        prog_addr => pc_prog_addr,
        prog_cs => pc1_cs,
        prog_we => pc_prog_we,
        prog_data => pc_prog_data
    );
    PC2: principal_component_1d generic map (
        loadfile => "bf2.rom"
    ) port map (
        clk => clk,
        addr => bf_addr,
        data => bf2_data,
                
        prog_addr => pc_prog_addr,
        prog_cs => pc2_cs,
        prog_we => pc_prog_we,
        prog_data => pc_prog_data
    );
    PC3: principal_component_1d generic map (
        loadfile => "bf3.rom"
    ) port map (
        clk => clk,
        addr => bf_addr,
        data => bf3_data,
                
        prog_addr => pc_prog_addr,
        prog_cs => pc3_cs,
        prog_we => pc_prog_we,
        prog_data => pc_prog_data
    );
    PC4: principal_component_1d generic map (
        loadfile => "bf4.rom"
    ) port map (
        clk => clk,
        addr => bf_addr,
        data => bf4_data,
                
        prog_addr => pc_prog_addr,
        prog_cs => pc4_cs,
        prog_we => pc_prog_we,
        prog_data => pc_prog_data
    );
    PC5: principal_component_1d generic map (
        loadfile => "bf5.rom"
    ) port map (
        clk => clk,
        addr => bf_addr,
        data => bf5_data,
                
        prog_addr => pc_prog_addr,
        prog_cs => pc5_cs,
        prog_we => pc_prog_we,
        prog_data => pc_prog_data
    );
    PC6: principal_component_1d generic map (
        loadfile => "bf6.rom"
    ) port map (
        clk => clk,
        addr => bf_addr,
        data => bf6_data,
                
        prog_addr => pc_prog_addr,
        prog_cs => pc6_cs,
        prog_we => pc_prog_we,
        prog_data => pc_prog_data
    );
    
    NORMAL_GEN: normal port map (
        clk => clk,
        rst => rst,
        q => normal_out
    );
    
    -- triggering the bandpass filter on the same cycle as the basis function inputs become valid
    -- will produce valid data on the next cycle, at the same time as the basis function outputs are ready
    normal_out_sfixed <= to_sfixed(std_logic_vector(normal_out), 1,-14);
    H3: bandpass port map (
        clk => clk,
        valid => h1_output_ready,
--        u => to_sfixed(normal_out, 1,-14),
        u => normal_out_sfixed,
        y => h3_output
    );
    
--    DECODE: process(clk, bf_output_ready, bf0_data, bf1_data, bf2_data, bf3_data, bf4_data, bf5_data, bf6_data, h3_output)
--        variable y_tmp: sfixed(1 downto -10);
--        variable x_tmp: sfixed(1 downto -10);
--    begin
--        y_tmp := resize(
--            to_sfixed(bf0_data, 1,-10) * vy0 +
--            to_sfixed(bf1_data, 1,-10) * vy1 +
--            to_sfixed(bf2_data, 1,-10) * vy2 +
--            to_sfixed(bf3_data, 1,-10) * vy3 +
--            to_sfixed(bf4_data, 1,-10) * vy4 +
--            to_sfixed(bf5_data, 1,-10) * vy5 +
--            to_sfixed(bf6_data, 1,-10) * vy6 +
--            h3_output * vyr
--        , y_tmp);
--        x_tmp := resize(
--                to_sfixed(bf0_data, 1,-10) * vx0 +
--                to_sfixed(bf1_data, 1,-10) * vx1 +
--                to_sfixed(bf2_data, 1,-10) * vx2 +
--                to_sfixed(bf3_data, 1,-10) * vx3 +
--                to_sfixed(bf4_data, 1,-10) * vx4 +
--                to_sfixed(bf5_data, 1,-10) * vx5 +
--                to_sfixed(bf6_data, 1,-10) * vx6 +
--                h3_output * vxr
--            , x_tmp);
--        if(rising_edge(clk)) then
--            valid <= bf_output_ready;
--            if(bf_output_ready = '1') then
--                y <= std_logic_vector(y_tmp);
--                x <= x_tmp;
--            end if;            
--        end if;
--    end process DECODE;

    DECODER_X: fixed_coefficient_decoder generic map (
        shift => 0,
        v0 => to_sfixed(0.16025, 1,-10),
        v1 => to_sfixed(-1.87415, 1,-10),
        v2 => to_sfixed(0.01965, 1,-10),
        v3 => to_sfixed(-0.06815, 1,-10),
        v4 => to_sfixed(-0.0181, 1,-10),
        v5 => to_sfixed(-0.02425, 1,-10),
        v6 => to_sfixed(-0.00245, 1,-10),
        vr => to_sfixed(0.02, 1,-10)              
    ) port map (
        clk => clk,
        pc_output_ready => bf_output_ready,
        pc0_data => bf0_data,
        pc1_data => bf1_data,
        pc2_data => bf2_data,
        pc3_data => bf3_data,
        pc4_data => bf4_data,
        pc5_data => bf5_data,
        pc6_data => bf6_data,
        normal_data => std_logic_vector(h3_output(1 downto -10)), -- FIXME
        valid => open,
        decoded_value => x
    );
    
    DECODER_Y: fixed_coefficient_decoder generic map (
        shift => 0,
        v0 => to_sfixed(0.16025, 1,-10),
        v1 => to_sfixed(-1.87415, 1,-10),
        v2 => to_sfixed(0.01965, 1,-10),
        v3 => to_sfixed(-0.06815, 1,-10),
        v4 => to_sfixed(-0.0181, 1,-10),
        v5 => to_sfixed(-0.02425, 1,-10),
        v6 => to_sfixed(-0.00245, 1,-10),
        vr => to_sfixed(0.02, 1,-10)              
    ) port map (
        clk => clk,
        pc_output_ready => bf_output_ready,
        pc0_data => bf0_data,
        pc1_data => bf1_data,
        pc2_data => bf2_data,
        pc3_data => bf3_data,
        pc4_data => bf4_data,
        pc5_data => bf5_data,
        pc6_data => bf6_data,
        normal_data => std_logic_vector(h3_output(1 downto -10)), -- FIXME
        valid => valid,
        decoded_value => y_fixed
    );
    
    y <= to_slv(y_fixed);

end Behavioral;
