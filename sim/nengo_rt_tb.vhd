library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

library std;
use std.textio.all;

entity nengo_rt_tb is
end entity;

architecture sim of nengo_rt_tb is
signal clk_125: std_logic;
constant CLOCK125_PERIOD: time := 8 ns;

component nengo_rt_tl generic (
    SIMULATION: string := "FALSE"
); port (
    clk_125: in std_logic;
    rst: in std_logic;    
    -- 3 MSBs are target type:
    -- 0x0: Decoded Value buffers
    -- 0x1: Encoder instruction lists
    -- 0x2: PC filter characteristics
    -- 0x3: PC LFSRs
    -- 0x4: Principal Component sample space
    -- 0x5: Decoder memory (DDR3)
    -- 0x6: not used
    -- 0x7: not used
    -- Addressing LSBs vary by target:
    -- Decoded Value buffers use 19: the highest 8 address an individual DV buffer,
    -- and the lowest 11 address within the buffer.
    -- Encoder instruction lists use 9: the 7 highest address a population unit, and the lowest 2 address
    -- one of (up to) four individual encoders.
    -- PC filter characteristics use 11: the 7 highest address a population unit, the next 2 bits address
    -- one of the four first-order filters,
    -- and the lowest 2 bits address the A,B,C,D coefficients.
    -- PC LFSRs use 9: the 7 highest address a population unit, and the lowest 2 bits address one of the four LFSRs.
    -- Principal components use 21: the 7 highest address all PCs in one population unit, the next 4 address
    -- an individual PC, and the lowest 10 address within the PC.
    -- Decoder memory uses 13: the 7 highest address a population unit,
    -- the next 2 choose which of the 4 DVs is being decoded, 
    -- and the lowest 4 address one of the 16 DV decoder circular buffers.
    prog_addr: in std_logic_vector(23 downto 0); 
    prog_we: in std_logic; -- active HIGH pulse; ignored while prog_ok is LOW.
    -- Again, the important bits in this field vary depending on what's being programmed:
    -- Decoded value buffers use the lowest 12.
    -- Encoder instruction lists use all 40.
    -- PC filter characteristics use the lowest 12.
    -- PC LFSRs use the lowest 32.
    -- Principal components use the lowest 12.
    -- Decoder memory uses the lowest 12.
    prog_data: in std_logic_vector(39 downto 0);
    prog_ack: out std_logic;
    prog_nyet: out std_logic;
    prog_error: out std_logic;
    prog_ok: out std_logic; -- HIGH when programming is allowed, i.e. after system reset and before run start
    start: in std_logic; -- Pulse HIGH to begin execution. Ignored while prog_ok is LOW.
    pause: in std_logic; -- Pulse HIGH to pause execution after current timestep. If start also asserted
                         -- on same timestep, single-step the simulation.
    running: out std_logic;
    timestep_overflow: out std_logic -- Strobed HIGH when a timeout has occurred.
    ); end component;

signal rst: std_logic;
signal prog_addr: std_logic_vector(23 downto 0);
signal prog_we: std_logic;
signal prog_data: std_logic_vector(39 downto 0);
signal prog_ack: std_logic;
signal prog_nyet: std_logic;
signal prog_error: std_logic;
signal prog_ok: std_logic;
signal start: std_logic;
signal pause: std_logic;
signal running: std_logic;
signal timestep_overflow: std_logic;

procedure PROGRAM_WITH_HANDSHAKE
(
    signal clk: in std_logic;
    next_prog_addr: in std_logic_vector(23 downto 0);
    next_prog_data: in std_logic_vector(39 downto 0);
    signal port_prog_addr: out std_logic_vector(23 downto 0);
    signal port_prog_we: out std_logic;
    signal port_prog_data: out std_logic_vector(39 downto 0);
    signal prog_ack: in std_logic;
    signal prog_nyet: in std_logic;
    signal prog_error: in std_logic
) is
begin
    port_prog_we <= '0';
    wait until falling_edge(clk);
    port_prog_addr <= next_prog_addr;
    port_prog_we <= '1';
    port_prog_data <= next_prog_data;
    wait until falling_edge(clk);
    port_prog_we <= '0';
    if(prog_ack = '0' and prog_nyet = '0' and prog_error = '0') then
        wait until (prog_ack = '1' or prog_nyet = '1' or prog_error = '1');
    end if;
    assert(prog_nyet = '0') report "Attempt to program got response NYET" severity failure;
    assert(prog_error = '0') report "Attempt to program got response ERROR" severity failure;
    wait until prog_ack = '0';
end procedure PROGRAM_WITH_HANDSHAKE;

procedure PROGRAM_DV_BUFFER
(
    which_dv: integer;
    which_addr: integer;
    which_data: std_logic_vector(11 downto 0);
    
    signal clk: in std_logic;
    signal port_prog_addr: out std_logic_vector(23 downto 0);
    signal port_prog_we: out std_logic;
    signal port_prog_data: out std_logic_vector(39 downto 0);
    signal prog_ack: in std_logic;
    signal prog_nyet: in std_logic;
    signal prog_error: in std_logic 
) is
    variable dv_u: unsigned(7 downto 0) := to_unsigned(which_dv, 8);
    variable addr_u: unsigned(10 downto 0) := to_unsigned(which_addr, 11);
    variable next_prog_addr: std_logic_vector(23 downto 0);
    variable next_prog_data: std_logic_vector(39 downto 0);
begin
    next_prog_addr(23 downto 21) := "000";
    next_prog_addr(20 downto 19) := "00";
    next_prog_addr(18 downto 11) := std_logic_vector(dv_u);
    next_prog_addr(10 downto 0) := std_logic_vector(addr_u);
    next_prog_data(39 downto 12) := (others=>'0');
    next_prog_data(11 downto 0) := which_data;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);
end procedure PROGRAM_DV_BUFFER;

procedure PROGRAM_ENCODER
(
    which_population: integer;
    which_encoder: integer;
    which_insn: std_logic_vector(39 downto 0);
    
    signal clk: in std_logic;
    signal port_prog_addr: out std_logic_vector(23 downto 0);
    signal port_prog_we: out std_logic;
    signal port_prog_data: out std_logic_vector(39 downto 0);
    signal prog_ack: in std_logic;
    signal prog_nyet: in std_logic;
    signal prog_error: in std_logic 
) is
    variable pop_u: unsigned(6 downto 0) := to_unsigned(which_population, 7);
    variable encoder_u: unsigned(1 downto 0) := to_unsigned(which_encoder, 2);
    variable next_prog_addr: std_logic_vector(23 downto 0);
    variable next_prog_data: std_logic_vector(39 downto 0);
begin
    next_prog_addr(23 downto 21) := "001";
    next_prog_addr(20 downto 9) := (others=>'0');
    next_prog_addr(8 downto 2) := std_logic_vector(pop_u);
    next_prog_addr(1 downto 0) := std_logic_vector(encoder_u);
    next_prog_data := which_insn;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);
end procedure PROGRAM_ENCODER;

procedure PROGRAM_FILTER
(
    which_population: integer;
    which_filter: integer;
    A: std_logic_vector(11 downto 0);
    B: std_logic_vector(11 downto 0);
    C: std_logic_vector(11 downto 0);
    D: std_logic_vector(11 downto 0);    
    
    signal clk: in std_logic;
    signal port_prog_addr: out std_logic_vector(23 downto 0);
    signal port_prog_we: out std_logic;
    signal port_prog_data: out std_logic_vector(39 downto 0);
    signal prog_ack: in std_logic;
    signal prog_nyet: in std_logic;
    signal prog_error: in std_logic 
) is
    variable pop_u: unsigned(6 downto 0) := to_unsigned(which_population, 7);
    variable filter_u: unsigned(1 downto 0) := to_unsigned(which_filter, 2);
    variable next_prog_addr: std_logic_vector(23 downto 0);
    variable next_prog_data: std_logic_vector(39 downto 0);
begin
    next_prog_addr(23 downto 21) := "010";
    next_prog_addr(20 downto 11) := (others=>'0');
    next_prog_addr(10 downto 4) := std_logic_vector(pop_u);
    next_prog_addr(3 downto 2) := std_logic_vector(filter_u);
    next_prog_data(39 downto 12) := (others=>'0');
    
    next_prog_addr(1 downto 0) := "00";    
    next_prog_data(11 downto 0) := A;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);
        
    next_prog_addr(1 downto 0) := "01";
    next_prog_data(11 downto 0) := B;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);
            
    next_prog_addr(1 downto 0) := "10";
    next_prog_data(11 downto 0) := C;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);
                
    next_prog_addr(1 downto 0) := "11";
    next_prog_data(11 downto 0) := D;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);
end procedure PROGRAM_FILTER;

procedure PROGRAM_LFSR
(
    which_population: integer;
    A: std_logic_vector(31 downto 0);
    B: std_logic_vector(31 downto 0);
    C: std_logic_vector(31 downto 0);
    D: std_logic_vector(31 downto 0);
    
    signal clk: in std_logic;
    signal port_prog_addr: out std_logic_vector(23 downto 0);
    signal port_prog_we: out std_logic;
    signal port_prog_data: out std_logic_vector(39 downto 0);
    signal prog_ack: in std_logic;
    signal prog_nyet: in std_logic;
    signal prog_error: in std_logic 
) is
    variable pop_u: unsigned(6 downto 0) := to_unsigned(which_population, 7);    
    variable next_prog_addr: std_logic_vector(23 downto 0);
    variable next_prog_data: std_logic_vector(39 downto 0);
begin
    next_prog_addr(23 downto 21) := "011";
    next_prog_addr(20 downto 9) := (others=>'0');
    next_prog_addr(8 downto 2) := std_logic_vector(pop_u);
    next_prog_data(39 downto 32) := (others=>'0');
    
    next_prog_addr(1 downto 0) := "00";    
    next_prog_data(31 downto 0) := A;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);
        
    next_prog_addr(1 downto 0) := "01";    
    next_prog_data(31 downto 0) := B;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);
            
    next_prog_addr(1 downto 0) := "10";    
    next_prog_data(31 downto 0) := C;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);
            
    next_prog_addr(1 downto 0) := "11";    
    next_prog_data(31 downto 0) := D;
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);                                            
end procedure PROGRAM_LFSR;    

type PrincipalComponentMemoryType is array(0 to 1023) of std_logic_vector(11 downto 0);
impure function InitPCFromFile (FileName: in string) return PrincipalComponentMemoryType is
    FILE ROMFile : text is in FileName;
    variable ROMFileLine : line;
    variable ROM: PrincipalComponentMemoryType;
    variable tmp: bit_vector(11 downto 0);
begin
    for I in PrincipalComponentMemoryType'range loop
        readline(ROMFile, ROMFileLine);
        read(ROMFileLine, tmp);
        ROM(I) := to_stdlogicvector(tmp);
    end loop;
    return ROM;
end function;
procedure PROGRAM_PRINCIPAL_COMPONENT
(
    which_population: integer;
    which_pc: integer;
    filename: string;

    signal clk: in std_logic;
    signal port_prog_addr: out std_logic_vector(23 downto 0);
    signal port_prog_we: out std_logic;
    signal port_prog_data: out std_logic_vector(39 downto 0);
    signal prog_ack: in std_logic;
    signal prog_nyet: in std_logic;
    signal prog_error: in std_logic 
) is
    variable pop_u: unsigned(6 downto 0) := to_unsigned(which_population, 7);
    variable pc_u: unsigned(3 downto 0) := to_unsigneD(which_pc, 4);
    variable ROM: PrincipalComponentMemoryType := InitPCFromFile(filename);
    variable next_prog_addr: std_logic_vector(23 downto 0);
    variable next_prog_data: std_logic_vector(39 downto 0);
begin
    next_prog_addr(23 downto 21) := "100";
    next_prog_addr(20 downto 14) := std_logic_vector(pop_u);
    next_prog_addr(13 downto 10) := std_logic_vector(pc_u);
    next_prog_data(39 downto 12) := (others=>'0');
    for I in 0 to 1023 loop
        next_prog_addr(9 downto 0) := std_logic_vector(to_unsigned(I, 10));
        next_prog_data(11 downto 0) := ROM(I);
        PROGRAM_WITH_HANDSHAKE(clk,
            next_prog_addr, next_prog_data,
            port_prog_addr, port_prog_we, port_prog_data,
            prog_ack, prog_nyet, prog_error);  
    end loop;
end procedure PROGRAM_PRINCIPAL_COMPONENT;


procedure PROGRAM_DECODER
(
    which_population: integer;
    which_dv: integer;
    which_decoder: integer;
    which_data: std_logic_vector(11 downto 0);

    signal clk: in std_logic;
    signal port_prog_addr: out std_logic_vector(23 downto 0);
    signal port_prog_we: out std_logic;
    signal port_prog_data: out std_logic_vector(39 downto 0);
    signal prog_ack: in std_logic;
    signal prog_nyet: in std_logic;
    signal prog_error: in std_logic 
) is
    variable pop_u: unsigned(6 downto 0) := to_unsigned(which_population, 7);
    variable dv_u: unsigned(1 downto 0) := to_unsigned(which_dv, 2);
    variable decoder_u: unsigned(3 downto 0) := to_unsigned(which_decoder, 4);
    variable next_prog_addr: std_logic_vector(23 downto 0);
    variable next_prog_data: std_logic_vector(39 downto 0);
begin
    next_prog_addr(23 downto 21) := "101";
    next_prog_addr(20 downto 13) := (others=>'0');
    next_prog_addr(12 downto 6) := std_logic_vector(pop_u);
    next_prog_addr(5 downto 4) := std_logic_vector(dv_u);
    next_prog_addr(3 downto 0) := std_logic_vector(decoder_u);
    next_prog_data(39 downto 12) := (others=>'0');
    next_prog_data(11 downto 0) := which_data;

    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);  
end procedure PROGRAM_DECODER;

begin

CLK125GEN: process
begin
    clk_125 <= '0';
    loop
        clk_125 <= '0';
        wait for CLOCK125_PERIOD/2;
        clk_125 <= '1';
        wait for CLOCK125_PERIOD/2;
    end loop;
end process CLK125GEN;

uut: nengo_rt_tl generic map ( SIMULATION => "TRUE") port map (
    clk_125 => clk_125,
    rst => rst,
    prog_addr => prog_addr,
    prog_we => prog_we,
    prog_data => prog_data,
    prog_ack => prog_ack,
    prog_nyet => prog_nyet,
    prog_error => prog_error,
    prog_ok => prog_ok,
    start => start,
    pause => pause,
    running => running,
    timestep_overflow => timestep_overflow
);

tb: process
    variable tmp_dv_data: std_logic_vector(11 downto 0);
    variable addr_counter: unsigned(9 downto 0);
begin
    rst <= '1';
    prog_addr <= (others=>'0');
    prog_we <= '0';
    prog_data <= (others=>'0');
    start <= '0';
    pause <= '0';
    
    wait for CLOCK125_PERIOD*3;
    rst <= '0';
    wait until prog_ok = '1';
    
    -- six things to program:
    -- "000" - Decoded Value buffers (just to clear them)
    -- "001" - Encoder instruction lists
    -- "010" - Principal Component filter characteristics
    -- "011" - Principal Component LFSRs
    -- "100" - Principal Component sample space
    -- "101" - Decoder memory circular buffers
    
    -- program DV#0: addresses 0 to 1023 get X"000" (feedback/output range), addresses 1024 to 2047 get to_sfixed(0.5, 1,-10) (input range)
    for I in 0 to 2047 loop 
        if(I > 1023) then
            tmp_dv_data := to_slv(to_sfixed(0.5, 1,-10));
        else
            tmp_dv_data := X"000";
        end if;
        PROGRAM_DV_BUFFER(0, I, tmp_dv_data,
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    end loop;
    
    -- program encoder#0: 1.0*Xi + 0.1*Ui
    for I in 0 to 1023 loop
        addr_counter := to_unsigned(I, 10);
        PROGRAM_ENCODER(0, 0,
            "0" & "0000000" & "0" & "00000000" & "0" & std_logic_vector(addr_counter) & to_slv(to_sfixed(1.0, 1,-10)),
            clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
        PROGRAM_ENCODER(0, 0,
            "1" & "0000000" & "0" & "00000000" & "1" & std_logic_vector(addr_counter) & to_slv(to_sfixed(0.1, 1,-10)),
            clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    end loop;
    
    -- program filter#0: A=0.99005, B=0.00995, C=0.99005, D=0.00995
    PROGRAM_FILTER(0, 0,
        to_slv(to_sfixed(0.99005, 1,-10)), to_slv(to_sfixed(0.00995, 1,-10)),         
        to_slv(to_sfixed(0.99005, 1,-10)), to_slv(to_sfixed(0.00995, 1,-10)),         
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
        
    -- program LFSR
    PROGRAM_LFSR(0, X"DD77F900", X"0CA6B31D", X"20A15FD0", X"A316C1EF",
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    
    -- program principal components 0 through 6
    PROGRAM_PRINCIPAL_COMPONENT(0, 0, "integrator0.rom",
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_PRINCIPAL_COMPONENT(0, 1, "integrator1.rom",
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_PRINCIPAL_COMPONENT(0, 2, "integrator2.rom",
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_PRINCIPAL_COMPONENT(0, 3, "integrator3.rom",
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_PRINCIPAL_COMPONENT(0, 4, "integrator4.rom",
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_PRINCIPAL_COMPONENT(0, 5, "integrator5.rom",
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_PRINCIPAL_COMPONENT(0, 6, "integrator6.rom",
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    -- program DV#0 decoders 0 through 7       
    PROGRAM_DECODER(0, 0, 0, to_slv(to_sfixed(0.16025, 1,-10)),
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_DECODER(0, 0, 1, to_slv(to_sfixed(-1.87415, 1,-10)),
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_DECODER(0, 0, 2, to_slv(to_sfixed(0.01965, 1,-10)),
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_DECODER(0, 0, 3, to_slv(to_sfixed(-0.06815, 1,-10)),
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_DECODER(0, 0, 4, to_slv(to_sfixed(-0.0181, 1,-10)),
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_DECODER(0, 0, 5, to_slv(to_sfixed(-0.02425, 1,-10)),
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_DECODER(0, 0, 6, to_slv(to_sfixed(-0.00245, 1,-10)),
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
    PROGRAM_DECODER(0, 0, 7, to_slv(to_sfixed(0.02, 1,-10)),
        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
        
    -- begin simulation
    wait for CLOCK125_PERIOD*3;
    wait until falling_edge(clk_125);
    start <= '1';
    wait for CLOCK125_PERIOD;
    start <= '0';    
    wait;
end process tb;

end architecture sim;