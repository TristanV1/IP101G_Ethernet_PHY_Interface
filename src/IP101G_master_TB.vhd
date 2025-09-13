library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.env.all;

entity IP101G_master_tb is
end;

architecture bench of IP101G_master_tb is

  constant CLK_FREQ : integer := 25E6;
  constant CLK_PERIOD : time := (1 sec)/CLK_FREQ;
  constant HALF_CLK_PERIOD : time := CLK_PERIOD/2;

  signal clk_25m : STD_LOGIC := '0';
  signal rst_n : STD_LOGIC := '0';
  signal phy_addr : STD_LOGIC_VECTOR(4 DOWNTO 0) := (others => '0');
  signal reg_addr : STD_LOGIC_VECTOR(4 DOWNTO 0) := (others => '0');
  signal write_data : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others => '0');
  signal read_data : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others => '0');
  signal write_valid : STD_LOGIC := '0';
  signal read_addr_valid : STD_LOGIC := '0';
  signal read_valid : STD_LOGIC := '0';
  signal busy : STD_LOGIC := '0';
  signal mdc : STD_LOGIC := '0';

  signal mdio_I : STD_LOGIC;
  signal mdio_O : STD_LOGIC;
  signal mdio_T : STD_LOGIC;

  signal mdio_SIM : STD_LOGIC;

  signal read_data_latch_TB : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others => '0');
  signal write_data_latch_TB : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others => '0');

  signal op_code_rd_TB : STD_LOGIC_VECTOR(1 downto 0) := (others => '0');
  signal reg_addr_rd_TB : STD_LOGIC_VECTOR(4 downto 0) := (others => '0');
  signal phy_addr_rd_TB : STD_LOGIC_VECTOR(4 downto 0) := (others => '0');

  --type sim_RAM_entry is array (integer range <>) of STD_LOGIC_VECTOR(15 downto 0);
  --signal sim_RAM : sim_RAM_entry(31 downto 0) := (others => x"BEEF");

  type TEST_TYP is (PASS,FAIL);
  signal test_result : TEST_TYP := PASS;
  signal test_result_latch : TEST_TYP := PASS;

  signal test_data : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
  

begin

  IP101G_master_inst : entity work.IP101G_master
  port map (
    clk_25m => clk_25m,
    rst_n => rst_n,
    phy_addr => phy_addr,
    reg_addr => reg_addr,
    write_data => write_data,
    read_data => read_data,
    write_valid => write_valid,
    read_addr_valid => read_addr_valid,
    read_valid => read_valid,
    busy => busy,
    mdc => mdc,
    mdio_I => mdio_I,
    mdio_O => mdio_O,
    mdio_T => mdio_T
  );

  IP101G_mgr_slv_inst : entity work.IP101G_mgr_slv
  port map (
    rst_n => rst_n,
    mdc => clk_25m,
    mdio_I => mdio_I,
    mdio_O => mdio_O,
    mdio_T => mdio_T
  );

  CLK_GEN_PROC : process begin
    clk_25m <= '0';
    wait for HALF_CLK_PERIOD;
    clk_25m <= '1';
    wait for HALF_CLK_PERIOD;
  end process; 

  TB_RESULT_LATCH : process(test_result) begin 
    if(test_result = FAIL) then 
      test_result_latch <= FAIL;
    end if;
  end process; 

  TB_STIM : process 
    procedure hold(clks_to_wait : integer) is 
    begin
      wait for clks_to_wait * CLK_PERIOD;
      wait until rising_edge(clk_25m);
    end procedure;
    
    procedure mii_mgr_read(phy_addr_rd : STD_LOGIC_VECTOR(4 downto 0);
                           reg_addr_rd : STD_LOGIC_VECTOR(4 downto 0)
                          ) is 

      constant to_read : STD_LOGIC_VECTOR(15 downto 0) := x"BEEF";
    begin 
      wait until rising_edge(clk_25m);
      phy_addr <= phy_addr_rd;
      reg_addr <= reg_addr_rd;
      wait until rising_edge(clk_25m);

      read_addr_valid <= '1';
      wait until rising_edge(clk_25m);
      read_addr_valid <= '0';
      wait until rising_edge(clk_25m);

      wait until (busy = '0');
    end procedure; 

    procedure mii_mgr_write(phy_addr_wr : STD_LOGIC_VECTOR(4 downto 0);
                            reg_addr_wr : STD_LOGIC_VECTOR(4 downto 0);
                            reg_data_wr : STD_LOGIC_VECTOR(15 downto 0)
                           ) is 
    begin 
      wait until rising_edge(clk_25m);
      phy_addr <= phy_addr_wr;
      reg_addr <= reg_addr_wr;
      write_data <= reg_data_wr;
      wait until rising_edge(clk_25m);

      write_valid <= '1';
      wait until rising_edge(clk_25m);
      write_valid <= '0';
      wait until rising_edge(clk_25m);

      wait until (busy = '0');
    end procedure; 

  begin
    rst_n <= '1';
    hold(10);
    rst_n <= '0';
    hold(10);
    rst_n <= '1';
    
    hold(20);


    DATA_VERIFY : for k in 0 to 15 loop
      test_data <= STD_LOGIC_VECTOR(to_unsigned(2 ** k,16));
      hold(1);

      RD_WR_VERIFY : for i in 0 to 31 loop
        mii_mgr_write(phy_addr_wr => "00000",
                      reg_addr_wr => STD_LOGIC_VECTOR(to_unsigned(i,5)),
                      reg_data_wr => test_data);

        hold(10);

        mii_mgr_read(phy_addr_rd => "00000",
                    reg_addr_rd => STD_LOGIC_VECTOR(to_unsigned(i,5)));

        hold(10);

        if(read_data /= test_data) then 
          test_result <= FAIL;
        else 
          test_result <= PASS;
        end if;

      end loop;

      hold(10);
    end loop;

    hold(50);

    stop;

  end process;

end;