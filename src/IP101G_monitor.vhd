library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

-- The IP101G utilizes a MII Management Interface used for monitoring the PHY's status.
-- The IP101G follows IEEE 802.3 which outlines the behavior of MDC and MDIO for the Management Interface.

-- Management Interface message format:

-- Write 32 bits total:
--> Start of Frame (2 bits) = "01"
--> Write Op Code (2 bits) = "01"
--> PHY Address (5 bits)
--> Reg Address (5 bits)
--> Turn-Around (2 bits) = "10"
--> Reg Data (16 bits)

-- Read 32 bits total:
--> Start of Frame (2 bits) = "01"
--> Read Op Code (2 bits) = "10"
--> PHY Address (5 bits)
--> Reg Address (5 bits)
--> Turn-Around (2 bits)
--> Reg Data (16 bits)


entity IP101G_monitor is 
  generic(
    PHY_ADDRESS : STD_LOGIC_VECTOR(4 DOWNTO 0) := "00000"
  );
  port(
    clk : IN STD_LOGIC;
    rst_n : IN STD_LOGIC;

    phy_link_ok : out STD_LOGIC;

    mdc : OUT STD_LOGIC; -- 25MHz output CLK supply
    mdio : INOUT STD_LOGIC -- Bi-directional serial data port
  );
end IP101G_monitor;

architecture Behavioral of IP101G_monitor is

  constant START_OF_FRAME : STD_LOGIC_VECTOR(1 DOWNTO 0) := "01";
  constant WRITE_OP_CODE : STD_LOGIC_VECTOR(1 DOWNTO 0) := "01";
  constant READ_OP_CODE : STD_LOGIC_VECTOR(1 DOWNTO 0) := "10";
  constant TURN_AROUND : STD_LOGIC_VECTOR(1 DOWNTO 0) := "10";
 
  constant CNTRL_REG_ADDR : STD_LOGIC_VECTOR(4 DOWNTO 0) := "00000";
  constant AUTO_NEG_RESTART_BIT : integer := 9; -- 1 : RESTART, 0 : NORMAL
  constant DUPLEX_MODE_BIT : integer := 8; -- 1 : FULL, 0 : HALF

  constant STATUS_REG_ADDR : STD_LOGIC_VECTOR(4 DOWNTO 0) := "00001";
  constant LINK_STATUS_BIT : integer := 2; -- 1 : VALID LINK, 0 : NO LINK
  constant AUTO_NEG_DONE_BIT : integer := 5; -- 1 : AUTO-NEG DONE, 0 : AUTO-NEG INCOMPLETE

  constant AUTO_NEG_REG : STD_LOGIC_VECTOR(4 DOWNTO 0) := "00110";
  constant AUTO_NEG_SUP : integer := 0; -- 1 : AUTO-NEG SUPPORTED, 0 : AUTO-NEG NOT SUPPORTED

  signal mdio_buff_in : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others => '0');
  signal mdio_buff_out : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others => '0');

  -- FSM Signals

  type MII_MON_ST_TYPE is (IDLE, ST_READ, ST_WRITE, ST_TA, HALT);
  signal mii_mon_st : MII_MON_ST_TYPE := IDLE;

  signal DATA_COUNTER : integer range 0 to 31; 

  -- Output 
  signal r_mdio : STD_LOGIC := '0';
  signal r_phy_link_ok : STD_LOGIC := '0';

  -- Auto-Negotiation Advertisement Link Mode
  constant ENABLE : STD_LOGIC := '1';
  constant DISABLE : STD_LOGIC := '0';

  signal BASE_TX_100_FULL : STD_LOGIC := DISABLE;
  signal BASE_TX_100 : STD_LOGIC := DISABLE;
  signal BASE_T_10_FULL : STD_LOGIC := ENABLE;
  signal BASE_T_10 : STD_LOGIC := DISABLE;

  -- Auto-Negotiation advertisement register value.
  signal AUTO_NEG_REG_SET : STD_LOGIC_VECTOR(15 DOWNTO 0) := "0000000" & BASE_TX_100_FULL & BASE_TX_100 
                                                                       & BASE_T_10_FULL & BASE_T_10 & "00000";

begin 

  mdc <= clk; 
  mdio <= r_mdio;
  phy_link_ok <= r_phy_link_ok;

process (clk) begin
  if(rising_edge(clk)) then 
    if (rst_n = '0') then 
      mii_mon_st <= IDLE;
      mdio <= '1';

    else
      case(mii_mon_st) is 

        when (IDLE) =>
          mdio <= '1';
          DATA_COUNTER <= 0;
          mdio_buff_in <= (others => '0');
          mdio_buff_out <= (others => '0');
          mii_mon_st <= ST_WRITE;

        when (ST_READ) =>

        when (ST_WRITE) =>

        when (ST_TA) =>

        when (HALT) =>

      end case;
    end if;
  end if;
end process;

end Behavioral;