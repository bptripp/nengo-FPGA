library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity programmable_decoder_1d is
generic (
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
); end entity programmable_decoder_1d;

architecture rtl of programmable_decoder_1d is
    signal ack: std_logic;
    type ack_state_type is (state_wait_for_valid, state_wait_for_deassertion, state_wait_for_shreg, state_wait_for_pc);
    signal ack_state: ack_state_type := state_wait_for_valid;
    
    component variable_coefficient_decoder generic (
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
           ); end component variable_coefficient_decoder;
    
    type register_bank_type is array(0 to 7) of std_logic_vector(11 downto 0);
    signal pc_bank: register_bank_type;
    signal v_bank: register_bank_type;
    type sfixed_bank_type is array(0 to 7) of sfixed(1 downto -10);
    signal v_sfixed_bank: sfixed_bank_type;
    signal bank_valid: std_logic := '0';
    
    signal decoder_valid: std_logic;
    signal decoded_value: sfixed(1 downto -10);
    
    signal write_ctrl_stall: std_logic := '0'; -- waiting for next timestep
    signal write_ctrl_counter: unsigned(9 downto 0) := (others=>'0');
    signal write_ctrl_done: std_logic := '0';
    
begin

    data_ack <= ack;

    ACK_CTRL: process(clk, rst, ack_state, pc_ready, shreg_ready)
        variable ack_v: std_logic;
    begin
        ack_v := '0';
        if(rising_edge(clk)) then
            if(rst = '1') then
                ack_v := '0';
                ack_state <= state_wait_for_valid;
            else
                case ack_state is
                when state_wait_for_valid =>
                    if(pc_ready = '1' and shreg_ready = '1') then
                        ack_v := '1';
                        ack_state <= state_wait_for_deassertion;
                    end if;
                when state_wait_for_deassertion =>
                    if(pc_ready = '0' and shreg_ready = '0') then
                        ack_state <= state_wait_for_valid;
                    elsif(pc_ready = '1' and shreg_ready = '0') then
                        ack_state <= state_wait_for_pc;
                    elsif(pc_ready = '0' and shreg_ready = '1') then
                        ack_state <= state_wait_for_shreg;
                    end if;
                when state_wait_for_shreg =>
                    if(shreg_ready = '0') then
                        ack_state <= state_wait_for_valid;
                    end if;
                when state_wait_for_pc =>
                    if(pc_ready = '0') then
                        ack_state <= state_wait_for_valid;
                    end if;
                end case;
            end if;
            ack <= ack_v;
        end if;        
    end process ACK_CTRL;
    
    -- register stage for inputs
    BANK_REG: process(clk, 
        pc0_data, pc1_data, pc2_data, pc3_data, pc4_data, pc5_data, pc6_data, pc7_data,
        v0, v1, v2, v3, v4, v5, v6, v7,
        ack)
    begin
        if(rising_edge(clk)) then
            pc_bank(0) <= pc0_data;
            pc_bank(1) <= pc1_data;
            pc_bank(2) <= pc2_data;
            pc_bank(3) <= pc3_data;
            pc_bank(4) <= pc4_data;
            pc_bank(5) <= pc5_data;
            pc_bank(6) <= pc6_data;
            pc_bank(7) <= pc7_data;
            v_bank(0) <= v0;
            v_bank(1) <= v1;
            v_bank(2) <= v2;
            v_bank(3) <= v3;
            v_bank(4) <= v4;
            v_bank(5) <= v5;
            v_bank(6) <= v6;
            v_bank(7) <= v7;
            bank_valid <= ack;
        end if;
    end process BANK_REG;
    
    V_TO_SFIXED: for I in 0 to 7 generate
        v_sfixed_bank(I) <= to_sfixed(v_bank(I), 1,-10);
    end generate;
    
    DECODER: variable_coefficient_decoder generic map (shift => shift) port map (
        clk => clk,
        pc_output_ready => ack,        
        pc0_data => pc_bank(0),
        pc1_data => pc_bank(1),
        pc2_data => pc_bank(2),
        pc3_data => pc_bank(3),
        pc4_data => pc_bank(4),
        pc5_data => pc_bank(5),
        pc6_data => pc_bank(6),
        normal_data => pc_bank(7),
        v0 => v_sfixed_bank(0),
        v1 => v_sfixed_bank(1),
        v2 => v_sfixed_bank(2),
        v3 => v_sfixed_bank(3),
        v4 => v_sfixed_bank(4),
        v5 => v_sfixed_bank(5),
        v6 => v_sfixed_bank(6),
        vr => v_sfixed_bank(7),
        
        decoded_value => decoded_value,
        valid => decoder_valid
    );
        
    all_done <= write_ctrl_done;
    WRITE_CTRL: process(clk, rst, timestep, decoder_valid, decoded_value, write_ctrl_counter, write_ctrl_stall)
        variable write_enable: std_logic;
        variable next_counter: unsigned(9 downto 0);
    begin
        write_enable := '0';
        next_counter := write_ctrl_counter;
        if(rising_edge(clk)) then
            if(rst = '1') then
                next_counter := "0000000000";
                write_ctrl_stall <= '0';
            elsif(write_ctrl_stall = '1') then
                if(timestep = '1') then
                    next_counter := "0000000000";
                    write_ctrl_stall <= '0';
                end if;
            else
                if(decoder_valid = '1') then
                    dv_addr <= std_logic_vector(write_ctrl_counter);
                    write_enable := '1';
                    dv_data <= to_slv(decoded_value);
                    -- register decoder output and address on dv interface
                    next_counter := write_ctrl_counter + X"1";
                    -- check if this is the last decoded value in this timestep                    
                    if(write_ctrl_counter = "1111111111") then
                        write_ctrl_stall <= '1';
                    end if;
                end if;
            end if;
            dv_we <= write_enable;
            write_ctrl_counter <= next_counter;
        end if;
    end process WRITE_CTRL;

end architecture rtl;