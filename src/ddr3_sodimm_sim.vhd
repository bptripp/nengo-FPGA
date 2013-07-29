library ieee;
use ieee.std_logic_1164.all;

entity ddr3_sodimm_sim is port (
    rst_n: in std_logic;
    ck: in std_logic;
    ck_n: in std_logic;
    cke: in std_logic;
    cs_n: in std_logic;
    ras_n: in std_logic;
    cas_n: in std_logic;
    we_n: in std_logic;
    dm: in std_logic_vector(7 downto 0);
    ba: in std_logic_vector(2 downto 0);
    addr: in std_logic_vector(13 downto 0);
    dq: inout std_logic_vector(63 downto 0);
    dqs: inout std_logic_vector(7 downto 0);
    dqs_n: inout std_logic_vector(7 downto 0);
    odt: in std_logic;
    init_calib_complete: in std_logic
); end entity ddr3_sodimm_sim;

architecture Simulation of ddr3_sodimm_sim is

constant NUM_COMP: integer := 8;
constant MEMORY_WIDTH: integer := 8;
constant DQ_WIDTH: integer := 64;
constant DQS_WIDTH: integer := 8;

signal ddr3_reset_n: std_logic;
signal ddr3_dq_sdram: std_logic_vector(63 downto 0);
signal ddr3_addr_sdram: std_logic_vector(13 downto 0);
signal ddr3_ba_sdram: std_logic_vector(2 downto 0);
signal ddr3_ras_n_sdram: std_logic;
signal ddr3_cas_n_sdram: std_logic;
signal ddr3_we_n_sdram: std_logic;
signal ddr3_cs_n_sdram: std_logic;
signal ddr3_odt_sdram: std_logic;
signal ddr3_cke_sdram: std_logic;
signal ddr3_dm_sdram: std_logic_vector(7 downto 0);
signal ddr3_dqs_p_sdram: std_logic_vector(7 downto 0);
signal ddr3_dqs_n_sdram: std_logic_vector(7 downto 0);
signal ddr3_ck_p_sdram: std_logic;
signal ddr3_ck_n_sdram: std_logic;

component ddr3_model port (
    rst_n: in std_logic;
    ck: in std_logic;
    ck_n: in std_logic;
    cke: in std_logic;
    cs_n: in std_logic;
    ras_n: in std_logic;
    cas_n: in std_logic;
    we_n: in std_logic;
    dm_tdqs: inout std_logic_vector(0 downto 0);
    ba: in std_logic_vector(2 downto 0);
    addr: in std_logic_vector(13 downto 0);
    dq: inout std_logic_vector(7 downto 0);
    dqs: inout std_logic_vector(0 downto 0);
    dqs_n: inout std_logic_vector(0 downto 0);
    tdqs_n: out std_logic_vector(0 downto 0);
    odt: in std_logic
); end component ddr3_model;

component WireDelay generic (
    Delay_g: time := 0 ps;
    Delay_rd: time := 0 ps;
    ERR_INSERT: string := "OFF"
); port (
    A: inout std_logic;
    B: inout std_logic;
    reset: in std_logic;
    phy_init_done: in std_logic
); end component WireDelay;

begin

ddr3_reset_n <= rst_n;
ddr3_ck_p_sdram <= ck;
ddr3_ck_n_sdram <= ck_n;
ddr3_addr_sdram <= addr;
ddr3_ba_sdram <= ba;
ddr3_ras_n_sdram <= ras_n;
ddr3_cas_n_sdram <= cas_n;
ddr3_we_n_sdram <= we_n;
ddr3_cke_sdram <= cke;
ddr3_cs_n_sdram <= cs_n;
ddr3_dm_sdram <= dm;
ddr3_odt_sdram <= odt;

SODIMM: for I in 0 to NUM_COMP-1 generate
    COMP: ddr3_model port map (
        rst_n => ddr3_reset_n,
        ck => ddr3_ck_p_sdram,
        ck_n => ddr3_ck_n_sdram,
        cke => ddr3_cke_sdram,
        cs_n => ddr3_cs_n_sdram,
        ras_n => ddr3_ras_n_sdram,
        cas_n => ddr3_cas_n_sdram,
        we_n => ddr3_we_n_sdram,
        dm_tdqs(0) => ddr3_dm_sdram(I),
        ba => ddr3_ba_sdram,
        addr => ddr3_addr_sdram,
        dq => ddr3_dq_sdram(MEMORY_WIDTH*(I+1)-1 downto MEMORY_WIDTH*(I)),
        dqs(0) => ddr3_dqs_p_sdram(I),
        dqs_n(0) => ddr3_dqs_n_sdram(I),
        tdqs_n => open,
        odt => ddr3_odt_sdram
    );
end generate;

-- bi-directional bus control
DQWDS: for DQWD in 0 to DQ_WIDTH-1 generate
    DLY: WireDelay generic map (
        Delay_g => 0 ps,
        Delay_rd => 0 ps,
        ERR_INSERT => "OFF"
    ) port map (
        A => dq(DQWD),
        B => ddr3_dq_sdram(DQWD),
        reset => rst_n,
        phy_init_done => init_calib_complete
    );
end generate;

DQSWDS: for DQSWD in 0 to DQS_WIDTH-1 generate
    DLY_P: WireDelay generic map (
        Delay_g => 0 ps,
        Delay_rd => 0 ps,
        ERR_INSERT => "OFF"
    ) port map (
        A => dqs(DQSWD),
        B => ddr3_dqs_p_sdram(DQSWD),
        reset => rst_n,
        phy_init_done => init_calib_complete
    );
    DLY_N: WireDelay generic map (
        Delay_g => 0 ps,
        Delay_rd => 0 ps,
        ERR_INSERT => "OFF"
    ) port map (
        A => dqs_n(DQSWD),
        B => ddr3_dqs_n_sdram(DQSWD),
        reset => rst_n,
        phy_init_done => init_calib_complete
    );
end generate;

end architecture Simulation;
