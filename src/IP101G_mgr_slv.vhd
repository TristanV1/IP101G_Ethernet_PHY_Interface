library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

-- This module is purely for testbench only. 
-- It will act as a slave device for the IP101G_master MDC/MDIO interface.

entity IP101G_mgr_slv is
  port (
    rst_n : in std_logic;
    mdc   : in std_logic;
    mdio_I : IN STD_LOGIC;
    mdio_O : OUT STD_LOGIC;
    mdio_T : IN STD_LOGIC
  );
end IP101G_mgr_slv;
architecture TB_Behavioral of IP101G_mgr_slv is
  signal mdio_wr_counter : integer range 0 to 15;
  signal mdio_rd_counter : integer range 0 to 15;

  signal mdio_in_buff  : std_logic_vector(13 downto 0) := (others => '0');
  signal mdio_wr_buff : std_logic_vector(15 downto 0) := (others => '0');

  signal mdio_read_detect  : std_logic := '0';
  signal mdio_write_detect : std_logic := '0';

  signal phy_addr    : std_logic_vector(4 downto 0)  := (others => '0');
  signal reg_addr    : std_logic_vector(4 downto 0)  := (others => '0');
  signal turn_around : std_logic_vector(1 downto 0)  := (others => '0');
  signal reg_data    : std_logic_vector(15 downto 0) := (others => '0');

  -- Constants for readability 
  constant START_OF_FRAME_CODE : std_logic_vector(1 downto 0) := "01";
  constant WRITE_OP_CODE       : std_logic_vector(1 downto 0) := "01";
  constant READ_OP_CODE        : std_logic_vector(1 downto 0) := "10";

  -- FSM signals
  type MGR_SLV_ST_TYPE is (INIT, IDLE, ST_READ, ST_WRITE, ST_TA);
  signal mgr_slv_st : MGR_SLV_ST_TYPE := INIT;

  type TA_ST_DELAY is array(1 downto 0) of MGR_SLV_ST_TYPE;
  signal ta_delay : TA_ST_DELAY := (INIT,INIT);

  -- TB RAM
  type mgr_RAM_entry is array (integer range <>) of STD_LOGIC_VECTOR(15 downto 0);
  signal mgr_RAM : mgr_RAM_entry(31 downto 0) := (others => x"BEEF");

  -- Latch 
  signal data_latch_TB : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others => '0');

  signal mdio_test : std_logic;

  
  -- Tri-state control

begin

  
  sof_detect_proc : process (mdc) is begin
    if (rising_edge(mdc)) then
      if (mgr_slv_st = IDLE) then
        if ((mdio_in_buff(13 downto 12) = START_OF_FRAME_CODE) and (mdio_in_buff(11 downto 10) = WRITE_OP_CODE)) then
          mdio_in_buff      <= (others => '0');
          phy_addr          <= mdio_in_buff(9 downto 5);
          reg_addr          <= mdio_in_buff(4 downto 0);
          --mdio_write_detect <= '1';

        elsif ((mdio_in_buff(13 downto 12) = START_OF_FRAME_CODE) and (mdio_in_buff(11 downto 10) = READ_OP_CODE)) then
          mdio_in_buff     <= (others => '0');
          phy_addr         <= mdio_in_buff(9 downto 5);
          reg_addr         <= mdio_in_buff(4 downto 0);
          --mdio_read_detect <= '1';
        else
          mdio_in_buff(13 downto 0) <= mdio_in_buff(12 downto 0) & mdio_I;
        end if;
      else
        --mdio_write_detect <= '0';
        --mdio_read_detect  <= '0';
      end if;
    end if;
  end process;

  mdio_read_detect <= '1' when ((mdio_in_buff(13 downto 12) = START_OF_FRAME_CODE) 
                          and  (mdio_in_buff(11 downto 10) = READ_OP_CODE) and mgr_slv_st = IDLE) else '0';

  mdio_write_detect <= '1' when ((mdio_in_buff(13 downto 12) = START_OF_FRAME_CODE) 
                          and  (mdio_in_buff(11 downto 10) = WRITE_OP_CODE) and mgr_slv_st = IDLE) else '0';

  mgr_slv_proc : process (mdc) is begin
    if (rising_edge(mdc)) then
      if (rst_n = '0') then
        mdio_wr_counter <= 15;
        mdio_rd_counter <= 14;
        ta_delay <= (INIT,INIT);
        mdio_wr_buff <= (others => '0');
        mgr_slv_st   <= INIT;
        --mdio <= 'Z';
      else
        case (mgr_slv_st) is
          when(INIT) =>
            mgr_slv_st <= IDLE;

          when(IDLE) =>
            if(mdio_write_detect = '1') then 
              mgr_slv_st <= ST_TA;
            elsif(mdio_read_detect = '1') then 
              mgr_slv_st <= ST_READ;
              mdio_O <= mgr_RAM(to_integer(unsigned(reg_addr)))(15); -- Preemptively output the first bit to meet TA delay
            end if;
            mgr_RAM(to_integer(unsigned(reg_addr))) <= mdio_wr_buff;

          when(ST_TA) => -- 2-bit Turn around delay
            mgr_slv_st <= ST_WRITE;
            
          when(ST_READ) =>
            if(mdio_rd_counter = 0) then
              mdio_rd_counter <= 14;
              mgr_slv_st <= IDLE;
            else 
              mdio_rd_counter <= mdio_rd_counter - 1;
            end if;
            mdio_O <= mgr_RAM(to_integer(unsigned(reg_addr)))(mdio_rd_counter);
            data_latch_TB <= mgr_RAM(to_integer(unsigned(reg_addr)));

          when(ST_WRITE) =>
            if(mdio_wr_counter = 0) then 
              mgr_RAM(to_integer(unsigned(reg_addr))) <= mdio_wr_buff;
              mdio_wr_counter <= 15;
              mgr_slv_st <= IDLE;
            else 
              mdio_wr_counter <= mdio_wr_counter - 1;
            end if;
            mdio_wr_buff(mdio_wr_counter) <= mdio_I;

        end case;
      end if;
    end if;
  end process;
end TB_Behavioral;