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
    prog_addr: in std_logic_vector(23 downto 0); 
    prog_we: in std_logic; -- active HIGH pulse; ignored while prog_ok is LOW.
    prog_data: in std_logic_vector(39 downto 0);
    prog_ack: out std_logic;
    prog_nyet: out std_logic;
    prog_error: out std_logic;
    prog_ok: out std_logic; -- HIGH when programming is allowed, i.e. after system reset and before run start
    page_block_addr: in std_logic_vector(5 downto 0);
    page_word_addr: in std_logic_vector(10 downto 0);
    page_we: in std_logic;
    page_lock: in std_logic;
    page_data: in std_logic_vector(11 downto 0);
    start: in std_logic; -- Pulse HIGH to begin execution. Ignored while prog_ok is LOW.
    pause: in std_logic; -- Pulse HIGH to pause execution after current timestep. If start also asserted
                         -- on same timestep, single-step the simulation.
    running: out std_logic;
    timestep_overflow: out std_logic; -- Strobed HIGH when a timeout has occurred.
	 
	 output0_data: out std_logic_vector(11 downto 0);
	 output0_we: out std_logic;
	 output0_done: out std_logic
    ); end component;

signal rst: std_logic;
signal prog_addr: std_logic_vector(23 downto 0);
signal prog_we: std_logic;
signal prog_data: std_logic_vector(39 downto 0);
signal prog_ack: std_logic;
signal prog_nyet: std_logic;
signal prog_error: std_logic;
signal prog_ok: std_logic;
signal page_block_addr: std_logic_vector(5 downto 0);
signal page_word_addr: std_logic_vector(10 downto 0);
signal page_we: std_logic;
signal page_lock: std_logic;
signal page_data: std_logic_vector(11 downto 0);
signal start: std_logic;
signal pause: std_logic;
signal running: std_logic;
signal timestep_overflow: std_logic;
signal output0_data: std_logic_vector(11 downto 0);
signal output0_we: std_logic;
signal output0_done: std_logic;

  function to_string(sv: Std_Logic_Vector) return string is
    variable bv: bit_vector(sv'range) := to_bitvector(sv);
    variable lp: line;
  begin
    write(lp, bv);
    return lp.all;
  end;
  
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
    report "PROGRAM address = " & to_string(next_prog_addr) & " data = " & to_string(next_prog_data);
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

procedure PROGRAM_OUTPUT_CHANNEL
(
	which_channel: integer;
	which_insn: std_logic_vector(35 downto 0);
	
	signal clk: in std_logic;
   signal port_prog_addr: out std_logic_vector(23 downto 0);
   signal port_prog_we: out std_logic;
   signal port_prog_data: out std_logic_vector(39 downto 0);
   signal prog_ack: in std_logic;
   signal prog_nyet: in std_logic;
   signal prog_error: in std_logic 
) is
	variable chan_u: unsigned(6 downto 0) := to_unsigned(which_channel, 7);
	variable next_prog_addr: std_logic_vector(23 downto 0);
	variable next_prog_data: std_logic_vector(39 downto 0);
begin
	next_prog_addr(23 downto 21) := "110";
	next_prog_addr(20 downto 7) := (others=>'0');
	next_prog_addr(6 downto 0) := std_logic_vector(chan_u);
	next_prog_data(39 downto 36) := (others=>'0');
	next_prog_data(35 downto 0) := which_insn;
	
    PROGRAM_WITH_HANDSHAKE(clk,
        next_prog_addr, next_prog_data,
        port_prog_addr, port_prog_we, port_prog_data,
        prog_ack, prog_nyet, prog_error);  	
end procedure PROGRAM_OUTPUT_CHANNEL;


function string_to_std_logic(c: character) return std_logic is 
    variable sl: std_logic;
    begin
      case c is
        when 'U' => 
           sl := 'U'; 
        when 'X' =>
           sl := 'X';
        when '0' => 
           sl := '0';
        when '1' => 
           sl := '1';
        when 'Z' => 
           sl := 'Z';
        when 'W' => 
           sl := 'W';
        when 'L' => 
           sl := 'L';
        when 'H' => 
           sl := 'H';
        when '-' => 
           sl := '-';
        when others =>
           sl := 'X'; 
    end case;
   return sl;
  end string_to_std_logic;

function string_to_std_logic_vector(s: string) return std_logic_vector is 
  variable slv: std_logic_vector(s'high-s'low downto 0);
  variable k: integer;
begin
   k := s'high-s'low;
  for i in s'range loop
     slv(k) := string_to_std_logic(s(i));
     k      := k - 1;
  end loop;
  return slv;
end string_to_std_logic_vector;                                       
                             

file loadfile: TEXT open read_mode is "nengo_rt_tb.nengo-rt";

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
    page_block_addr => page_block_addr,
    page_word_addr => page_word_addr,
    page_we => page_we,
    page_lock => page_lock,
    page_data => page_data,
    start => start,
    pause => pause,
    running => running,
    timestep_overflow => timestep_overflow,
	 output0_data => output0_data,
	 output0_we => output0_we,
	 output0_done => output0_done
);

tb: process
    variable tmp_dv_data: std_logic_vector(11 downto 0);
    variable addr_counter: unsigned(9 downto 0);
	 
	 variable next_prog_addr: std_logic_vector(23 downto 0);
    variable next_prog_data: std_logic_vector(39 downto 0);
	 
	 variable l: line;
	 variable s: string(1 to 65);	 
begin
    rst <= '1';
    prog_addr <= (others=>'0');
    prog_we <= '0';
    prog_data <= (others=>'0');
    page_block_addr <= (others=>'0');
    page_word_addr <= (others=>'0');
    page_we <= '0';
    page_lock <= '0';
    page_data <= (others=>'0');
    start <= '0';
    pause <= '0';
    
    wait for CLOCK125_PERIOD*3;
    rst <= '0';
    wait until prog_ok = '1';
    
	 -- program the board from the specified Nengo-RT loadfile
	 while not endfile(loadfile) loop
		readline(loadfile, l);
		read(l, s);
		next_prog_addr := string_to_std_logic_vector(s(1 to 24));
		next_prog_data := string_to_std_logic_vector(s(26 to 65));
		PROGRAM_WITH_HANDSHAKE( clk_125,
			next_prog_addr, next_prog_data,
			prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error
		);
	 end loop;
	 
    -- six things to program:
    -- "000" - Decoded Value buffers (just to clear them)
    -- "001" - Encoder instruction lists
    -- "010" - Principal Component filter characteristics
    -- "011" - Principal Component LFSRs
    -- "100" - Principal Component sample space
    -- "101" - Decoder memory circular buffers
    
    -- program DV#0: addresses 0 to 1023 get X"000" (feedback/output range)
    --for I in 0 to 1023 loop 
    --    tmp_dv_data := X"000";
--        PROGRAM_DV_BUFFER(0, I, tmp_dv_data,
--            clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    end loop;
--    -- program DV#192: addresses 0 to 1023 get to_sfixed(0.5, 1,-10) (input range)
--    for I in 0 to 1023 loop
--        tmp_dv_data := to_slv(to_sfixed(0.5, 1,-10));                
--        PROGRAM_DV_BUFFER(192, I, tmp_dv_data,
--            clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    end loop;
--    
--    -- program encoder#0: 1.0*Xi + 0.1*Ui
--    for I in 0 to 1023 loop
--        addr_counter := to_unsigned(I, 10);
--        PROGRAM_ENCODER(0, 0,
--            "0" & "0000000" & "0" & "00000000" & "0" & std_logic_vector(addr_counter) & to_slv(to_sfixed(1.0, 1,-10)),
--            clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--        PROGRAM_ENCODER(0, 0,
--            "1" & "0000000" & "0" & "11000000" & "0" & std_logic_vector(addr_counter) & to_slv(to_sfixed(0.1, 1,-10)),
--            clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    end loop;
--    
--    -- program filter#0: A=0.99005, B=0.00995, C=0.99005, D=0.00995
--    PROGRAM_FILTER(0, 0,
--        to_slv(to_sfixed(0.99005, 1,-10)), to_slv(to_sfixed(0.00995, 1,-10)),         
--        to_slv(to_sfixed(0.99005, 1,-10)), to_slv(to_sfixed(0.00995, 1,-10)),         
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--        
--    -- program LFSR
--    PROGRAM_LFSR(0, X"DD77F900", X"0CA6B31D", X"20A15FD0", X"A316C1EF",
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    
--    -- program principal components 0 through 6
--    PROGRAM_PRINCIPAL_COMPONENT(0, 0, "integrator0.rom",
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_PRINCIPAL_COMPONENT(0, 1, "integrator1.rom",
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_PRINCIPAL_COMPONENT(0, 2, "integrator2.rom",
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_PRINCIPAL_COMPONENT(0, 3, "integrator3.rom",
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_PRINCIPAL_COMPONENT(0, 4, "integrator4.rom",
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_PRINCIPAL_COMPONENT(0, 5, "integrator5.rom",
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_PRINCIPAL_COMPONENT(0, 6, "integrator6.rom",
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--		  
--    -- program DV#0 decoders 0 through 7
--    -- FIXME technically we have to program 1024 decoders but they're all the same so this isn't wrong for now       
--    PROGRAM_DECODER(0, 0, 0, to_slv(to_sfixed(0.16025, 1,-10)),
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_DECODER(0, 0, 1, to_slv(to_sfixed(-1.87415, 1,-10)),
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_DECODER(0, 0, 2, to_slv(to_sfixed(0.01965, 1,-10)),
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_DECODER(0, 0, 3, to_slv(to_sfixed(-0.06815, 1,-10)),
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_DECODER(0, 0, 4, to_slv(to_sfixed(-0.0181, 1,-10)),
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_DECODER(0, 0, 5, to_slv(to_sfixed(-0.02425, 1,-10)),
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_DECODER(0, 0, 6, to_slv(to_sfixed(-0.00245, 1,-10)),
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--    PROGRAM_DECODER(0, 0, 7, to_slv(to_sfixed(0.02, 1,-10)),
--        clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);
--
--	 -- program output channel #0
--	 -- read DV#0 port#0 addresses 0-1023 (feedback/output range), no delay
--	 PROGRAM_OUTPUT_CHANNEL(0, "1" & "0" & "00000000" & "00000000000" & "00111111111" & "0000",
--		clk_125, prog_addr, prog_we, prog_data, prog_ack, prog_nyet, prog_error);   
	 
    -- begin simulation
    wait for CLOCK125_PERIOD*3;
    wait until falling_edge(clk_125);
    start <= '1';
    wait for CLOCK125_PERIOD;
    start <= '0';    
    wait;
end process tb;

end architecture sim;