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

entity IP101G_master is 
  port(
    clk_25m : IN STD_LOGIC; -- 25MHz clock
    rst_n : IN STD_LOGIC; -- Should be connected to system global reset_n

    -- Interface signals
    phy_addr : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
    reg_addr : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
    write_data : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    read_data : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);

    -- Latch data in
    write_valid : IN STD_LOGIC;
    read_addr_valid : IN STD_LOGIC;
    read_valid : OUT STD_LOGIC;

    -- Ready to write/read
    busy : OUT STD_LOGIC;

    -- MII Management Interface Signals
    mdc : OUT STD_LOGIC; -- 25MHz output clk_25m supply
    --mdio : INOUT STD_LOGIC -- Bi-directional serial data port
    mdio_I : OUT STD_LOGIC;
    mdio_O : IN STD_LOGIC;
    mdio_T : OUT STD_LOGIC
  );
end IP101G_master;

architecture Behavioral of IP101G_master is

  -- Constants for readability
  constant START_OF_FRAME : STD_LOGIC_VECTOR(1 DOWNTO 0) := "01";
  constant WRITE_OP_CODE : STD_LOGIC_VECTOR(1 DOWNTO 0) := "01";
  constant READ_OP_CODE : STD_LOGIC_VECTOR(1 DOWNTO 0) := "10";
  constant TURN_AROUND : STD_LOGIC_VECTOR(1 DOWNTO 0) := "10";

  -- Input/output data buffers
  signal mdio_buff_in : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others => '0');
  signal mdio_buff_in_valid : STD_LOGIC := '0';
  signal mdio_buff_out : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');
  signal mdio_write_done : STD_LOGIC := '0';

  -- Convert control signals to pulse
  signal prev_write_valid : STD_LOGIC;
  signal prev_read_addr_valid : STD_LOGIC;

  signal i_write_valid : STD_LOGIC;
  signal i_read_addr_valid : STD_LOGIC;

  -- MII Management Interface FSM Signals
  type MII_MANAGER_ST_TYPE is (IDLE, ST_READ, ST_TA, ST_READ_LATCH, ST_WRITE, HALT);
  signal mii_man_st : MII_MANAGER_ST_TYPE := IDLE;
  signal wr_data_counter : integer range 0 to 31; -- 32 bits of output data for write
  signal rd_data_counter : integer range 0 to 13; -- 14 bits of address data for read
  signal rd_data_latch_counter : integer range 0 to 15; -- 16 bits of input data for read
  signal ta_delay : integer range 0 to 1; -- Counts for 2-bit Turn around delay
  signal manager_rw_ready : STD_LOGIC := '0'; 

  -- MII Management Interface Controller FSM Signals
  type MII_MANAGER_CNTRL_ST_TYPE is (INIT, IDLE, CNTRL_READ, CNTRL_WRITE);
  signal mii_man_cntrl_st : MII_MANAGER_CNTRL_ST_TYPE := INIT;

  -- MII Management Interface Read/Write control
  type MII_CONTROL_REQ is (NONE, READ_REQ, WRITE_REQ);
  signal control_request : MII_CONTROL_REQ := NONE;
  constant ZERO_PAD : STD_LOGIC_VECTOR(17 DOWNTO 0) := (others => '0'); -- Read transactions do not use lower 18 bits of output buffer

  -- Tri-state control
  --signal mdio_I : std_logic;
  --signal mdio_O : std_logic; 
  signal i_mdio_T : std_logic := '0'; 

begin 

  mdc <= clk_25m; 
  mdio_T <= i_mdio_T;

conv_cntrl_to_pulse : process(clk_25m) begin
  if(rising_edge(clk_25m)) then 
    prev_write_valid <= write_valid;
    prev_read_addr_valid <= read_addr_valid;
  end if;
end process; 

i_write_valid <= '1' when ((write_valid = '1') and (prev_write_valid = '0')) else '0';
i_read_addr_valid <= '1' when ((read_addr_valid = '1') and (prev_read_addr_valid = '0')) else '0';

mdio_controller_proc : process (clk_25m) begin -- Arbitrates Read/Write priority
  if(rising_edge(clk_25m)) then 
    if (rst_n = '0') then 
      busy <= '0';
      read_valid <= '0';
      mii_man_cntrl_st <= INIT;
    else
      case(mii_man_cntrl_st) is 
        when(INIT) =>
          busy <= '0';
          read_valid <= '0';
          mii_man_cntrl_st <= IDLE;

        when(IDLE) =>
          read_valid <= '0';
          if(manager_rw_ready = '1') then 
            if(i_write_valid = '1') then -- Latch data to output buffer
              busy <= '1';
              mdio_buff_out <= START_OF_FRAME & WRITE_OP_CODE & phy_addr & 
                               reg_addr & TURN_AROUND & write_data;
              control_request <= WRITE_REQ;                 
              mii_man_cntrl_st <= CNTRL_WRITE; 
              
            elsif(i_read_addr_valid = '1') then 
              busy <= '1';
              mdio_buff_out <= START_OF_FRAME & READ_OP_CODE & phy_addr & 
                               reg_addr & ZERO_PAD;
              control_request <= READ_REQ;
              mii_man_cntrl_st <= CNTRL_READ; 
            end if;
          end if;

        when(CNTRL_READ) =>
          if(mdio_buff_in_valid = '1') then 
            busy <= '0';
            read_data <= mdio_buff_in;
            read_valid <= '1';
            mii_man_cntrl_st <= IDLE;
          end if;
          control_request <= NONE;

        when(CNTRL_WRITE) =>
          if(mdio_write_done = '1') then 
            busy <= '0';
            mii_man_cntrl_st <= IDLE;
          end if;
          control_request <= NONE;

      end case;
    end if;
  end if;
end process;

mdio_interface_proc : process (clk_25m) begin -- Handles MII Management Interface Transactions
    if(rising_edge(clk_25m)) then 
      if (rst_n = '0') then 
        mii_man_st <= IDLE;
        --mdio <= 'Z';
        wr_data_counter <= 31;
        rd_data_counter <= 31;
        rd_data_latch_counter <= 15;
        ta_delay <= 1;
        mdio_buff_in <= (others => '0');
        mdio_buff_in_valid <= '0';
        manager_rw_ready <= '0';
        mdio_write_done <= '0';

      else
        case(mii_man_st) is 
          when(IDLE) => -- Wait for a read/write request
            --mdio <= 'Z';
            i_mdio_T <= '1';
            wr_data_counter <= 31;
            rd_data_counter <= 31;
            rd_data_latch_counter <= 15;
            ta_delay <= 1;
            mdio_buff_in <= (others => '0');
            mdio_buff_in_valid <= '0';
            mdio_write_done <= '0';

            if (control_request = WRITE_REQ) then 
              mii_man_st <= ST_WRITE;
            elsif (control_request = READ_REQ) then 
              mii_man_st <= ST_READ;
            else
              manager_rw_ready <= '1';
            end if;
            
          when(ST_WRITE) => -- Write 32 bits to MDIO
            if(wr_data_counter = 0) then 
              wr_data_counter <= 31;
              mii_man_st <= IDLE;
              mdio_write_done <= '1';
              i_mdio_T <= '1';
            else 
              wr_data_counter <= wr_data_counter - 1;
            end if;
            mdio_I <= mdio_buff_out(wr_data_counter);
            i_mdio_T <= '0';
            --mdio <= mdio_buff_out(wr_data_counter);
            

          when(ST_READ) => -- Write ST, Op Code, PHY Addr, and Reg Addr. Then wait for TA delay
            if(rd_data_counter = 18) then 
              rd_data_counter <= 31;
              mii_man_st <= ST_TA;
              i_mdio_T <= '1';
            else 
              rd_data_counter <= rd_data_counter - 1;
            end if;
            --mdio <= mdio_buff_out(rd_data_counter);
            mdio_I <= mdio_buff_out(rd_data_counter);
            i_mdio_T <= '0';
            mdio_buff_in_valid <= '0';

          when(ST_TA) => -- 2-bit Turn-around delay
            if(ta_delay = 0) then
              mii_man_st <= ST_READ_LATCH;
              ta_delay <= 1;
            else
              ta_delay <= 0;
            end if;
            --mdio <= 'Z';
            i_mdio_T <= '1';
            mdio_buff_in_valid <= '0';

          when(ST_READ_LATCH) => -- Latch 16-bits of data in
            if(rd_data_latch_counter = 0) then 
              rd_data_latch_counter <= 15;
              mii_man_st <= IDLE;
              mdio_buff_in_valid <= '1';
            else 
              rd_data_latch_counter <= rd_data_latch_counter - 1;
            end if;
            --mdio <= 'Z';
            mdio_buff_in(rd_data_latch_counter) <= mdio_O;
            i_mdio_T <= '1';
            
          when others =>
            mii_man_st <= IDLE;

        end case; 

      end if;
    end if;
end process;

end Behavioral;