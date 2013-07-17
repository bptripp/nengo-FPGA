----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/09/2013 03:15:23 PM
-- Design Name: 
-- Module Name: vanderpol - Behavioral
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

entity vanderpol is
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           x1_out : out STD_LOGIC_VECTOR (11 downto 0);
           x2_out : out STD_LOGIC_VECTOR (11 downto 0);
           valid: out std_logic;
           
           pc_prog_sel: in std_logic_vector(3 downto 0);
           pc_prog_addr: in std_logic_vector(9 downto 0);
           pc_prog_we: in std_logic;
           pc_prog_data: in std_logic_vector(11 downto 0)
    );
end vanderpol;

architecture Behavioral of vanderpol is
    signal x1: sfixed(1 downto -10);
    signal x2: sfixed(1 downto -10);
    
  
  constant BYPASS_H1: integer := 0;
    
  component multiplexed_1filter Port ( 
        clk : in STD_LOGIC;
        rst: in std_logic;
        x : in sfixed (1 downto -16);
        u : in sfixed (1 downto -10);
        valid : in STD_LOGIC;
       x1 : out sfixed (1 downto -16);
       y : out sfixed (1 downto -10);
       ready : out STD_LOGIC; -- connect this to next stage data-valid
       ack: in std_logic; -- connect this to next stage data-acknowledge
       we: out std_logic -- connect this to FIFO acknowledge (assuming FWFT) and FIFO write-enable
   ); end component;
  component filter_state_fifo  PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din: IN STD_LOGIC_VECTOR(17 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(17 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC
  ); end component;
  signal h1_x1_sfixed: sfixed(1 downto -16);
  signal h1_x1_input_valid: std_logic;
  signal h1_x1_x: std_logic_vector(17 downto 0);
  signal h1_x1_xnext: sfixed(1 downto -16);
  signal h1_x1_output: sfixed(1 downto -10);
  signal h1_x1_output_ready: std_logic;
  signal h1_x1_we: std_logic;
  signal h1_x1_stall: std_logic;
  signal h1_x1_valid: std_logic;
  
  signal h1_x2_sfixed: sfixed(1 downto -16);
  signal h1_x2_input_valid: std_logic;
  signal h1_x2_x: std_logic_vector(17 downto 0);
  signal h1_x2_xnext: sfixed(1 downto -16);
  signal h1_x2_output: sfixed(1 downto -10);
  signal h1_x2_output_ready: std_logic;
  signal h1_x2_we: std_logic;
  signal h1_x2_stall: std_logic;
  signal h1_x2_valid: std_logic;
  
  signal h1_ready: std_logic;
  
    component principal_component_2d generic (
      loadfile: string
    ); Port ( 
        clk : in STD_LOGIC;
        x_in : in STD_LOGIC_VECTOR (11 downto 0);
         y_in: in std_logic_vector(11 downto 0);
         data : out STD_LOGIC_VECTOR (11 downto 0);
         ready : in STD_LOGIC;
         valid : out STD_LOGIC;
         
         prog_addr: in std_logic_vector(9 downto 0);
         prog_cs: in std_logic;
         prog_we: in std_logic;
         prog_data: in std_logic_vector(11 downto 0)
         ); end component;
    signal pc_prog_cs: std_logic_vector(14 downto 0);
  
  type pc_data_type is array(0 to 14) of std_logic_vector(11 downto 0);
  signal pc_data: pc_data_type;
  signal pc_valid: std_logic_vector(14 downto 0);
    
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
  
  component fixed_coefficient_decoder
  generic (
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
  ); end component;
  
  signal decode_x1_top_valid: std_logic;
  signal decode_x1_bottom_valid: std_logic;
  signal decode_x2_top_valid: std_logic;
  signal decode_x2_bottom_valid: std_logic;
  
  signal decode_x1_top_dv: sfixed(1 downto -10);
  signal decode_x1_bottom_dv: sfixed(1 downto -10);
  signal decode_x2_top_dv: sfixed(1 downto -10);
  signal decode_x2_bottom_dv: sfixed(1 downto -10);
  
  signal output_valid: std_logic;
  
begin

--USE_H1: if (BYPASS_H1 = 0) generate

    -- FIXME: CAREFUL WITH THIS!! The FIFOs will be run at 1024 elements all the time "for real", meaning they can be legitimately full and still continue to operate.
    -- However FIFO Generator reports 1026 elements in a nomimally 1024-deep FIFO so this may be okay.
    -- We know ahead of time that we can't write to the FIFO for three write clocks after a reset so we can just count.
    h1_x1_valid <= '1' when (h1_x1_input_valid = '1' and h1_x1_stall = '0') else '0';
    h1_x2_valid <= '1' when (h1_x2_input_valid = '1' and h1_x2_stall = '0') else '0';

    h1_x1_sfixed <= to_sfixed(h1_x1_x, 1,-16);
    H1X1: multiplexed_1filter port map (
        clk => clk,
        rst => rst,
        x => h1_x1_sfixed,
        u => x1,
        valid => h1_x1_valid,
        x1 => h1_x1_xnext,
        y => h1_x1_output,
        ready => h1_x1_output_ready,
        ack => h1_x1_output_ready, -- automatic acknowledge for now.
        we => h1_x1_we
    );
    H1_X1_FIFO: filter_state_fifo port map (
        clk => clk,
        rst => rst,
        din => std_logic_vector(h1_x1_xnext),
        wr_en => h1_x1_we,
        rd_en => h1_x1_we,
        dout => h1_x1_x,
        full => h1_x1_stall,
        empty => open
    );
    
    h1_x2_sfixed <= to_sfixed(h1_x2_x, 1,-16);
    H1X2: multiplexed_1filter port map (
        clk => clk,
        rst => rst,
        x => h1_x2_sfixed,
        u => x2,
        valid => h1_x2_valid,
        x1 => h1_x2_xnext,
        y => h1_x2_output,
        ready => h1_x2_output_ready,
        ack => h1_x2_output_ready, -- automatic acknowledge for now.
        we => h1_x2_we
    );
    H1_X2_FIFO: filter_state_fifo port map (
        clk => clk,
        rst => rst,
        din => std_logic_vector(h1_x2_xnext),
        wr_en => h1_x2_we,
        rd_en => h1_x2_we,
        dout => h1_x2_x,
        full => h1_x2_stall,
        empty => open
    );
    h1_ready <= '1' when (h1_x1_output_ready = '1' and h1_x2_output_ready = '1') else '0';
--end generate;

DONT_USE_H1: if (BYPASS_H1 = 1) generate
    h1_x1_output <= x1;
    h1_x2_output <= x2;
    DELAY_H1_READY: process(clk, h1_x1_input_valid, h1_x2_input_valid)        
    begin
        if(rising_edge(clk)) then
            if (h1_x1_input_valid = '1' and h1_x2_input_valid = '1') then
                h1_ready <= '1';
            else
                h1_ready <= '0';
            end if;
        end if;
    end process;
end generate;    
    
COMPONENTS: for I in 0 to 14 generate
    pc_prog_cs(I) <= '1' when (pc_prog_sel = std_logic_vector(to_unsigned(I,4))) else '0';
    PC: principal_component_2d generic map (
        loadfile => "vanderpol_pc" & integer'image(I) & ".rom" 
    ) port map (
        clk => clk,
        x_in => std_logic_vector(h1_x1_output),
        y_in => std_logic_vector(h1_x2_output),
        ready => h1_ready,
        data => pc_data(I),
        valid => pc_valid(I),
        prog_addr => pc_prog_addr,
        prog_cs => pc_prog_cs(I),
        prog_we => pc_prog_we,
        prog_data => pc_prog_data
    );
end generate;

NORMAL_GEN: normal port map (
    clk => clk,
    rst => rst,
    q => normal_out
);

normal_out_sfixed <= to_sfixed(std_logic_vector(normal_out), 1,-14);
H3: bandpass port map (
    clk => clk,
    valid => h1_ready,
--        u => to_sfixed(normal_out, 1,-14),
    u => normal_out_sfixed,
    y => h3_output
);

DECODE_X1_TOP: fixed_coefficient_decoder generic map (
    shift => 1,
    v0 => to_sfixed(-0.0165, 1,-10),
    v1 => to_sfixed(-0.7992, 1,-10),
    v2 => to_sfixed(-1.9982, 1,-10),
    v3 => to_sfixed(-0.0219, 1,-10),
    v4 => to_sfixed(0.0007, 1,-10),
    v5 => to_sfixed(0.0084, 1,-10),
    v6 => to_sfixed(-0.0176, 1,-10),
    vr => to_sfixed(-0.0209, 1,-10)
) port map (
    clk => clk,
    pc_output_ready => pc_valid(0),
    pc0_data => pc_data(0),
    pc1_data => pc_data(1),
    pc2_data => pc_data(2),
    pc3_data => pc_data(3),
    pc4_data => pc_data(4),
    pc5_data => pc_data(5),
    pc6_data => pc_data(6),
    normal_data => pc_data(7), -- cheating
    valid => decode_x1_top_valid,
    decoded_value => decode_x1_top_dv
);
DECODE_X1_BOTTOM: fixed_coefficient_decoder generic map (
    shift => 1,
    v0 => to_sfixed(-0.1651, 1,-10),
    v1 => to_sfixed(0.1128, 1,-10),
    v2 => to_sfixed(0.0520, 1,-10),
    v3 => to_sfixed(-0.0185, 1,-10),
    v4 => to_sfixed(0.0265, 1,-10),
    v5 => to_sfixed(-0.0273, 1,-10),
    v6 => to_sfixed(0.0151, 1,-10),
    vr => to_sfixed(0.01, 1,-10)
) port map (
    clk => clk,
    pc_output_ready => pc_valid(8),
    pc0_data => pc_data(8),
    pc1_data => pc_data(9),
    pc2_data => pc_data(10),
    pc3_data => pc_data(11),
    pc4_data => pc_data(12),
    pc5_data => pc_data(13),
    pc6_data => pc_data(14),
    normal_data => std_logic_vector(h3_output(1 downto -10)),
    valid => decode_x1_bottom_valid,
    decoded_value => decode_x1_bottom_dv
);

DECODE_X2_TOP: fixed_coefficient_decoder generic map (
    shift => 1,
    v0 => to_sfixed(-0.0071, 1,-10),
    v1 => to_sfixed(-1.8654, 1,-10),
    v2 => to_sfixed(0.6473, 1,-10),
    v3 => to_sfixed(-0.0178, 1,-10),
    v4 => to_sfixed(-0.0068, 1,-10),
    v5 => to_sfixed(0.0057, 1,-10),
    v6 => to_sfixed(0.0665, 1,-10),
    vr => to_sfixed(-0.0212, 1,-10)
) port map (
    clk => clk,
    pc_output_ready => pc_valid(0),
    pc0_data => pc_data(0),
    pc1_data => pc_data(1),
    pc2_data => pc_data(2),
    pc3_data => pc_data(3),
    pc4_data => pc_data(4),
    pc5_data => pc_data(5),
    pc6_data => pc_data(6),
    normal_data => pc_data(7), -- cheating
    valid => decode_x2_top_valid,
    decoded_value => decode_x2_top_dv
);
DECODE_X2_BOTTOM: fixed_coefficient_decoder generic map (
    shift => 1,
    v0 => to_sfixed(0.0020, 1,-10),
    v1 => to_sfixed(-0.0470, 1,-10),
    v2 => to_sfixed(0.0189, 1,-10),
    v3 => to_sfixed(0.0138, 1,-10),
    v4 => to_sfixed(0.0064, 1,-10),
    v5 => to_sfixed(0.0071, 1,-10),
    v6 => to_sfixed(0.0065, 1,-10),
    vr => to_sfixed(0.01, 1,-10)
) port map (
    clk => clk,
    pc_output_ready => pc_valid(8),
    pc0_data => pc_data(8),
    pc1_data => pc_data(9),
    pc2_data => pc_data(10),
    pc3_data => pc_data(11),
    pc4_data => pc_data(12),
    pc5_data => pc_data(13),
    pc6_data => pc_data(14),
    normal_data => std_logic_vector(h3_output(1 downto -10)),
    valid => decode_x2_bottom_valid,
    decoded_value => decode_x2_bottom_dv
);

REGISTER_DVS: process(clk, rst, decode_x1_top_dv, decode_x1_bottom_dv, decode_x2_top_dv, decode_x2_bottom_dv,
                      decode_x1_top_valid, decode_x1_bottom_valid, decode_x2_top_valid, decode_x2_bottom_valid)
    variable x1_next: sfixed(1 downto -10);
    variable x2_next: sfixed(1 downto -10);           
begin
    x1_next := resize(decode_x1_top_dv + decode_x1_bottom_dv, 1,-10, fixed_saturate, fixed_truncate);
    x2_next := resize(decode_x2_top_dv + decode_x2_bottom_dv, 1,-10, fixed_saturate, fixed_truncate);
    if(rising_edge(clk)) then
        if(rst = '1') then
            x1 <= to_sfixed(0.1, 1,-10);
            x2 <= to_sfixed(0.1, 1,-10);
            output_valid <= '0';
        elsif(decode_x1_top_valid = '1' and decode_x1_bottom_valid = '1' and decode_x2_top_valid = '1' and decode_x2_bottom_valid = '1') then
            x1 <= x1_next;
            x2 <= x2_next;
            output_valid <= '1';
        else
            output_valid <= '0';
        end if;
    end if;
end process;

VALIDATE_INPUT: process(clk, rst, output_valid, h1_x1_input_valid, h1_x2_input_valid, h1_x1_valid, h1_x2_valid)
    variable input_valid: std_logic;
    variable x_valid: std_logic;
begin
    if(h1_x1_input_valid = '1' and h1_x2_input_valid = '1') then
        input_valid := '1';
    else
        input_valid := '0';
    end if;
    if(h1_x1_valid = '1' and h1_x2_valid = '1') then
        x_valid := '1';
    else
        x_valid := '0';
    end if;    
    if(rising_edge(clk)) then
        if(rst = '1') then
            -- data is valid at initial conditions
            input_valid := '1';
        elsif(input_valid = '1') then
            -- if x_valid = 1 then our data has just been accepted into H1.
            if(x_valid = '1') then
                input_valid := '0';
            end if;
        else
            -- if output_valid = 1 then data has just become valid
            if(output_valid = '1') then
                input_valid := '1';
            end if;
        end if;
        h1_x1_input_valid <= input_valid;
        h1_x2_input_valid <= input_valid;
    end if;
end process;

x1_out <= std_logic_vector(x1);
x2_out <= std_logic_vector(x2);
valid <= output_valid;

end Behavioral;
