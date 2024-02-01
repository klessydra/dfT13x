----------------------------------------------------------------------------------------------------------------
--  Stage ID - (Instruction decode and registerfile read)                                                     --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                          --
--                                                                                                            --
--  Date Modified: 07-04-2020                                                                                 --
----------------------------------------------------------------------------------------------------------------
--  Registerfiles of the incoming hart are read in this stage in parallel with the decoding                   --
--  Two types of registerfiles can be generaated for XILINX FPGAs LUTAM based or FF based dpeneding on the    --
--  setting of the generic variaabble chosen                                                                  --
--  The scratchpad memory mapper also exists in this stage, which maps the address to the corresponding SPM   --
--  This pipeline stage always takes one cycle latency                                                        --
----------------------------------------------------------------------------------------------------------------

-- ieee packages ------------
library ieee;
use ieee.math_real.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;
--use work.klessydra_parameters.all;

-- pipeline  pinout --------------------
entity REGISTERFILE is
  generic(
    THREAD_POOL_SIZE           : natural;
    LUTRAM_RF                  : natural;
    morph_en                   : natural;
    fetch_stage_en             : natural;
    accl_en                    : natural;
    SPM_NUM                    : natural;
    Addr_Width                 : natural;
    SPM_STRT_ADDR              : std_logic_vector(31 downto 0);
    RF_SIZE                    : natural;
    RF_CEIL                    : natural;
    SPM_ADDR_WID               : natural
    );
  port (
    -- clock, reset active low
    clk_i                        : in  std_logic;
    rst_ni                       : in  std_logic;
    -- dtmr sygnals     
    dest_valid                   : in std_logic;
    LS_WB_wrong                  : out std_logic;
    LS_WB_wrong_EXEC             : out std_logic;
    LS_WB_wrong_EXEC_lat         : out std_logic;
    LS_WB_wrong_EXEC_wire        : out std_logic;
    LS_WB_wrong_END              : out std_logic;
    load_op_buf_wire             : in std_logic_vector(2 downto 0);  
    load_op_buf_lat              : in std_logic_vector(2 downto 0);  
    LS_is_running_wire           : in std_logic;
    LS_is_running                : in std_logic;
    edge_fault                   : in std_logic;
    restore_fault_PC_wire        : in std_logic;
    restore_fault_PC             : in std_logic;
    restore_fault_LSU_wire       : in std_logic;
    restore_fault_LSU            : in std_logic;
    restore_fault_RF_wire        : out std_logic;
    restore_fault_RF             : out std_logic;
    restore_fault                : in std_logic;
    restore_store_active         : in std_logic;
    restore_stall                : in std_logic;
    pc_voted                     : in std_logic_vector(31 downto 0);
    WB_no_done                   : out std_logic;
  -- lsu address generation
    add_out_load                 : out std_logic_vector(31 downto 0);
    add_out_store                : out std_logic_vector(31 downto 0);

  -- Branch Control Signals
    harc_IF                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_ID                 : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_EXEC               : in  natural range THREAD_POOL_SIZE-1 downto 0;    
    harc_WB                 : out  natural range THREAD_POOL_SIZE-1 downto 0;
    pc_IF                   : in  std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    pc_ID                   : in  std_logic_vector(31 downto 0);  -- pc_ID is PC entering ID stage
    pc_IE                   : in  std_logic_vector(31 downto 0);
    pc_WB                   : in  std_logic_vector(31 downto 0);
    data_dependency         : in  std_logic;
    bypass_rs1              : in  std_logic;
    bypass_rs2              : in  std_logic;
    bypass_rd_read          : in  std_logic;
    jalr_stall              : in  std_logic;
    branch_stall            : in  std_logic;
    core_busy_IE            : in  std_logic;
    core_busy_LS            : in  std_logic;
    core_busy_LS_lat        : in  std_logic;
    busy_ID                 : in  std_logic;  
    ls_parallel_exec        : in  std_logic;
    dsp_parallel_exec       : in  std_logic;
    dsp_to_jump_wire        : in  std_logic;
    instr_rvalid_ID         : in  std_logic;
    instr_rvalid_ID_int     : in  std_logic;
    instr_word_ID           : in  std_logic_vector(31 downto 0);
    instr_word_IE           : in  std_logic_vector(31 downto 0);
    LS_WB_EN                : in  std_logic;
    IE_WB_EN                : in  std_logic;
    MUL_WB_EN               : in  std_logic;
    IE_WB                   : in  std_logic_vector(31 downto 0);
    MUL_WB                  : in  std_logic_vector(31 downto 0);
    LS_WB                   : in  std_logic_vector(31 downto 0);
    instr_word_LS_WB        : in  std_logic_vector(31 downto 0);
    instr_word_IE_WB        : in  std_logic_vector(31 downto 0);
    harc_LS_WB              : in  natural range THREAD_POOL_SIZE-1 downto 0;
    harc_IE_WB              : in  natural range THREAD_POOL_SIZE-1 downto 0;
    zero_rs1                : out std_logic;
    zero_rs2                : out std_logic;
    pass_BEQ                : out std_logic;
    pass_BNE                : out std_logic;
    pass_BLT                : out std_logic;
    pass_BLTU               : out std_logic;
    pass_BGE                : out std_logic;
    pass_BGEU               : out std_logic;
    RS1_Data_IE             : out std_logic_vector(31 downto 0);
    RS2_Data_IE             : out std_logic_vector(31 downto 0);
    RD_Data_IE              : out std_logic_vector(31 downto 0);
    return_address          : out array_2d(THREAD_POOL_SIZE-1 downto 0)(31 downto 0);
    rs1_to_sc               : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rs2_to_sc               : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rd_to_sc                : out std_logic_vector(SPM_ADDR_WID-1 downto 0);
    data_addr_internal_IE   : out std_logic_vector(31 downto 0);
    regfile                 : out array_3d(THREAD_POOL_SIZE-1 downto 0)(RF_SIZE-1 downto 0)(31 downto 0)
    );
end entity;  ------------------------------------------


-- Klessydra dfT1m (4 stages) pipeline implementation -----------------------
architecture RF of REGISTERFILE is

  subtype harc_range is natural range THREAD_POOL_SIZE - 1 downto 0;

  signal regfile_lutram_rs1 : array_2d((THREAD_POOL_SIZE*RF_SIZE)-1 downto 0)(31 downto 0);
  signal regfile_lutram_rs2 : array_2d((THREAD_POOL_SIZE*RF_SIZE)-1 downto 0)(31 downto 0);
  signal regfile_lutram_rd  : array_2d((THREAD_POOL_SIZE*RF_SIZE)-1 downto 0)(31 downto 0);

  -- signals for dTMR voting
  signal regfile_voted_wire     : array_2d(RF_SIZE-1 downto 0)(31 downto 0);
  signal WB_RD_voted       : std_logic_vector(31 downto 0);
  signal WB_RD_voted_wire   : std_logic_vector(31 downto 0);
  signal WB_RD_buf_wire    : array_2d(harc_range)(31 downto 0);
  signal WB_RD_buf    : array_2d(harc_range)(31 downto 0);
  signal WB_EN_buf_wire    : std_logic_vector(2 downto 0);
  signal WB_EN_buf   : std_logic_vector(2 downto 0);
  signal RD_WB_buf     : array_2d_int(harc_range);
  signal RD_WB_buf_wire     : array_2d_int(harc_range);
  signal RD_WB    : integer;
  signal WB_ready     : std_logic;
  signal rs1_bypass_wire   : std_logic;
  signal rs2_bypass_wire   : std_logic;
  signal rs1_bypass_lat   : std_logic;
  signal rs2_bypass_lat   : std_logic;
  signal rs1_bypass   : std_logic;
  signal rs2_bypass   : std_logic;
  signal harc_WB_lat  : harc_range;
  signal fault_RF     : std_logic;  
  signal RS1_Data_IE_buffer  : std_logic_vector(31 downto 0);
  signal RS2_Data_IE_buffer  : std_logic_vector(31 downto 0);
  signal WB_done_PC             : std_logic_vector(31 downto 0);   

  --FAULT TOLERANT VOTER 
--  signal parity1      : std_logic;
--  signal parity_voted : std_logic;

  --segnali dft1m
  signal restore_fault_WB : std_logic;
  signal WB_done_wire     : std_logic;
  signal WB_done          : std_logic;

  signal WB_fault : std_logic;

  -- shared signals
  signal harc_loss  : harc_range;

--  alias pc_IF_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Pipe.pc_IF : std_logic_vector(31 downto 0) >>;
--  alias instr_word_ID_lat_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Pipe.FETCH.instr_word_ID_lat : std_logic_vector(31 downto 0) >>;
--  alias dest_valid_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Pipe.DECODE.dest_valid : std_logic >>;
--  alias edge_fault_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Prg_Ctr.edge_fault : std_logic >>;
--  alias harc_EXEC_alias is << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.harc_EXEC : harc_range >>; 
--  alias harc_IF_alias is << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.harc_IF : harc_range >>; 
--  alias LS_is_running_lat_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Pipe.LSU.LS_is_running_lat : std_logic >>;
--  alias load_op_buf_lat_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Pipe.LSU.load_op_buf_lat : std_logic_vector(2 downto 0) >>;
--  alias load_op_buf_wire_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Pipe.LSU.load_op_buf_wire : std_logic_vector(2 downto 0) >>;
--  alias LS_is_running_wire_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Pipe.LSU.LS_is_running_wire : std_logic >>;
--  alias LS_is_running_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Pipe.LSU.LS_is_running : std_logic >>;


  attribute ram_style : string;
  attribute ram_style of RS1_Data_IE        : signal is "reg"; -- AAA i think these are unecessary and need to be removed
  attribute ram_style of RS2_Data_IE        : signal is "reg"; -- AAA i think these are unecessary and need to be removed
  attribute ram_style of RD_Data_IE         : signal is "reg"; -- AAA i think these are unecessary and need to be removed
  attribute ram_style of regfile_lutram_rs1 : signal is "distributed";
  attribute ram_style of regfile_lutram_rs2 : signal is "distributed";
  attribute ram_style of regfile_lutram_rd  : signal is "distributed";

  signal ID_rs1_to_sc           : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal ID_rs2_to_sc           : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal ID_rd_to_sc            : std_logic_vector(SPM_ADDR_WID-1 downto 0);

  signal RS1_Data_IE_wire       : std_logic_vector(31 downto 0);
  signal RS2_Data_IE_wire       : std_logic_vector(31 downto 0);
  signal RD_Data_IE_wire        : std_logic_vector(31 downto 0);

  -- instruction operands
  signal RS1_Addr_IE            : std_logic_vector(4 downto 0);   -- debugging signals
  signal RS2_Addr_IE            : std_logic_vector(4 downto 0);   -- debugging signals
  signal RD_Addr_IE             : std_logic_vector(4 downto 0);   -- debugging signals
  signal RD_EN                  : std_logic;
  signal WB_RD                  : std_logic_vector(31 downto 0);
  signal WB_EN                  : std_logic;
  signal instr_word_WB          : std_logic_vector(31 downto 0);

  function rs1 (signal instr : in std_logic_vector(31 downto 0)) return integer is
  begin
    return to_integer(unsigned(instr(15+(RF_CEIL-1) downto 15)));
  end;

  function rs2 (signal instr : in std_logic_vector(31 downto 0)) return integer is
  begin
    return to_integer(unsigned(instr(20+(RF_CEIL-1) downto 20)));
  end;

  function rd (signal instr : in std_logic_vector(31 downto 0)) return integer is
  begin
    return to_integer(unsigned(instr(7+(RF_CEIL-1) downto 7)));
  end;

  -- This function increments all the bits in a std_logic_vector
  function add_vect_bits(v: std_logic_vector) return natural is
    variable h: natural;
  begin
    h := 0;
    for i in v'range loop
      if v(i) = '1' then
        h := h + 1;
      end if;
    end loop;
    return h;
  end function add_vect_bits;

begin

  RF_FF : if LUTRAM_RF = 0 generate

  RF_ACCESS : process(clk_i, rst_ni, instr_word_ID)  -- synch single state process
  begin
    if rst_ni = '0' then
      for h in harc_range loop
        for i in  RF_SIZE-1 downto 0  loop 
          regfile(h)(i) <= (others => '0');
        end loop;
      end loop;
      RS1_Data_IE <= (others => '0');
      RS2_Data_IE <= (others => '0');
      RD_Data_IE <= (others => '0');
      WB_fault <= '0';
    elsif rising_edge(clk_i) then

      if core_busy_IE = '1' or core_busy_LS = '1' or ls_parallel_exec = '0'  or dsp_parallel_exec = '0' or LS_WB_wrong_EXEC = '1' or ( restore_fault_PC = '1' and harc_ID /= 0 ) then -- the instruction pipeline is halted
      elsif instr_rvalid_ID_int = '0' then -- wait for a valid instruction
      else  -- process the incoming instruction 

        ------------------------------------------------------------
        --  ██████╗ ███████╗ ██████╗ ███████╗██╗██╗     ███████╗  --
        --  ██╔══██╗██╔════╝██╔════╝ ██╔════╝██║██║     ██╔════╝  --
        --  ██████╔╝█████╗  ██║  ███╗█████╗  ██║██║     █████╗    --
        --  ██╔══██╗██╔══╝  ██║   ██║██╔══╝  ██║██║     ██╔══╝    --
        --  ██║  ██║███████╗╚██████╔╝██║     ██║███████╗███████╗  --
        --  ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝╚══════╝  --
        ------------------------------------------------------------

--        data_addr_internal_IE <= std_logic_vector(signed(regfile(harc_ID)(rs1(instr_word_ID))) + signed(S_immediate(instr_word_ID)));
        ----- REGISTERFILE READ IS DONE HERE --------------------------------------------------------------------------------------------------------------
        RS1_Data_IE <=  RS1_Data_IE_wire; 
        RS2_Data_IE <=  RS2_Data_IE_wire; 
      
        if accl_en = 1 then
          if dsp_to_jump_wire = '0' then
            RD_Data_IE <= regfile(harc_ID)(rd(instr_word_ID)); -- only the DSP unit reads the accelerator
          else
            RD_Data_IE <= regfile(harc_ID)(0);
          end if;
        end if;

        -- dft1m start --
        data_addr_internal_IE <= std_logic_vector(signed(regfile_voted_wire(rs1(instr_word_ID))) + signed(S_immediate(instr_word_ID)));

        -- pragma translate_on
        RD_Data_IE  <= regfile_voted_wire(rd(instr_word_ID)) when  morph_en = 0 or bypass_rd_read = '0' or harc_ID /= harc_WB else WB_RD; -- reading the 'rd' data here is only for debugging purposes if the acclerator is disabled
        RS1_Addr_IE <= std_logic_vector(to_unsigned(rs1(instr_word_ID), 5)); -- debugging signals
        RS2_Addr_IE <= std_logic_vector(to_unsigned(rs2(instr_word_ID), 5)); -- debugging signals
        RD_Addr_IE  <= std_logic_vector(to_unsigned(rd(instr_word_ID), 5)); -- debugging signals
        -- pragma translate_on
        -- dft1m end --
       ----------------------------------------------------------------------------------------------------------------------------------------------------
      end if;  -- instr. conditions

      -- dft1m start --
      -- performs the RF write back from a voted value only in case of WB_EN (from buffers), in case of the address RD_WB different from zeroes, and when you are
      -- in the harc_WB = 1 or harc_WB = 0 (but only when the wb_done logic has confirmed that another wb wasn't already performed)
      if  RD_WB /= 0 and ( add_vect_bits(WB_EN_buf_wire) > 1 or (add_vect_bits(WB_EN_buf_wire) >= 1 and add_vect_bits(WB_EN_buf) >= 1)) and 
         ( harc_WB = 1 or ( harc_WB = 0 and WB_done = '0') or ( harc_WB = 0 and WB_done = '1' and (WB_done_PC /= pc_IE) and restore_fault = '1') )   then --  ( WB_EN_lat = '1' or LS_PE_WB_EN = '1' )then
        for h in harc_range loop
          regfile(h)(RD_WB) <= WB_RD_voted_wire;    
        end loop;
        WB_fault <= '1';
      else 
        WB_fault <= '0';
      end if;     
      -- dft1m end --

    end if;  -- clk
  end process;

  -- dt1m start --
  -- regfile reading for the operands. Take the buffered result in case of bypass.
  RS1_Data_IE_wire <= regfile_voted_wire(rs1(instr_word_ID)) when morph_en = 0 or rs1_bypass_wire = '0'  else WB_RD_buf_wire(harc_ID); --or harc_ID /= harc_WB else WB_RD_buf_wire(harc_ID); 
  RS2_Data_IE_wire <= regfile_voted_wire(rs2(instr_word_ID)) when morph_en = 0 or rs2_bypass_wire = '0'  else WB_RD_buf_wire(harc_ID); --or harc_ID /= harc_WB else WB_RD_buf_wire(harc_ID);
  RD_Data_IE_wire  <= regfile_voted_wire(rd(instr_word_ID))  when morph_en = 0 or bypass_rd_read  = '0' else WB_RD_buf_wire(harc_ID); --or harc_ID /= harc_WB else WB_RD_buf_wire(harc_ID);
  -- dft1m end --
  end generate; -- LUTRAM_RF = 0




  RF_LUTRAM : if LUTRAM_RF = 1 generate

  RF_RD_EN : process(all)  -- synch single state process
  begin
    RD_EN <= '0';
    if core_busy_IE = '1' or core_busy_LS = '1' or ls_parallel_exec = '0'  or dsp_parallel_exec = '0' or data_dependency = '1' then -- the instruction pipeline is halted
    elsif instr_rvalid_ID_int = '0' then -- wait for a valid instruction
    else  -- process the incoming instruction 
      RD_EN <= '1';
    end if;  -- instr. conditions
    if morph_en = 1 then
      if rs1(instr_word_ID) /= 0 then
        RS1_Data_IE_wire <= regfile_lutram_rs1(32*harc_ID+rs1(instr_word_ID)) when bypass_rs1 = '0' or harc_ID /= harc_WB else WB_RD; 
      else
        RS1_Data_IE_wire <= (others => '0');
      end if;
      if rs2(instr_word_ID) /= 0 then
        RS2_Data_IE_wire <= regfile_lutram_rs2(32*harc_ID+rs2(instr_word_ID)) when bypass_rs2 = '0' or harc_ID /= harc_WB else WB_RD;
      else
        RS2_Data_IE_wire <= (others => '0');
      end if;
      if accl_en = 1 then
        if rs1(instr_word_ID) /= 0 then
          RD_Data_IE_wire <= regfile_lutram_rd(32*harc_ID+rd(instr_word_ID)) when morph_en = 0 or bypass_rd_read = '0' or harc_ID /= harc_WB else WB_RD; -- only the DSP unit reads the accelerator
        else
          RD_Data_IE_wire <= (others => '0');
        end if;
      end if;
    elsif morph_en = 0 then
      if rs1(instr_word_ID) /= 0 then
        RS1_Data_IE_wire <= regfile_lutram_rs1(32*harc_ID+rs1(instr_word_ID)); 
      else
        RS1_Data_IE_wire <= (others => '0');
      end if;
      if rs2(instr_word_ID) /= 0 then
        RS2_Data_IE_wire <= regfile_lutram_rs2(32*harc_ID+rs2(instr_word_ID));
      else
        RS2_Data_IE_wire <= (others => '0');
      end if;
    end if;

  end process;

  RS1_ACCESS : process(clk_i)  -- synch single state process
  begin
    if rising_edge(clk_i) then
      if RD_EN = '1' then 
        data_addr_internal_IE <= std_logic_vector(signed(regfile_lutram_rs1(32*harc_ID+rs1(instr_word_ID))) + signed(S_immediate(instr_word_ID)));
        RS1_Data_IE <= RS1_Data_IE_wire;
      -- pragma translate_off
       RD_Data_IE  <= regfile_lutram_rs1(32*harc_ID+rd(instr_word_ID)); -- reading the 'rd' data here is only for debugging purposes when the acclerator is disabled
       RS1_Addr_IE <= std_logic_vector(to_unsigned(rs1(instr_word_ID), 5)); -- debugging signals
       RS2_Addr_IE <= std_logic_vector(to_unsigned(rs2(instr_word_ID), 5)); -- debugging signals
       RD_Addr_IE  <= std_logic_vector(to_unsigned(rd(instr_word_ID), 5)); -- debugging signals
      -- pragma translate_on
      end if;  -- instr. conditions
      if WB_EN = '1' then
        regfile_lutram_rs1(32*harc_WB+rd(instr_word_WB)) <= WB_RD;
      end if;
    end if;  -- clk
  end process;

  RS2_ACCESS : process(clk_i)  -- synch single state process
  begin
    if rising_edge(clk_i) then
      if RD_EN = '1' then 
        RS2_Data_IE <= RS2_Data_IE_wire;
      end if;  -- instr. conditions
      if WB_EN = '1' then
        regfile_lutram_rs2(32*harc_WB+rd(instr_word_WB)) <= WB_RD;
      end if;
    end if;  -- clk
  end process;

  RD_LUTRAM : if accl_en = 1 generate
  RD_ACCESS : process(clk_i)  -- synch single state process
  begin
    if rising_edge(clk_i) then
      if accl_en = 1 then
        if RD_EN = '1' then
          RD_Data_IE <= RD_Data_IE_wire;
        end if;  -- instr. conditions
      end if;
      if WB_EN = '1' then
        regfile_lutram_rd(32*harc_WB+rd(instr_word_WB)) <= WB_RD;
      end if;
    end if;  -- clk
  end process;
  end generate; -- accl_en = 1

  end generate; -- LUTRAM_RF = 1

  RETURN_ADDR_REG : if fetch_stage_en = 1 generate
  RA_ACCESS : process(clk_i) begin
    if rising_edge(clk_i) then
      if WB_EN = '1' then
        if rd(instr_word_WB) = 1 then
          return_address(harc_WB) <= WB_RD;
        end if;
      end if; 
    end if;
  end process;
  end generate;

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ██████╗ ████████╗███╗   ███╗██████╗     ██████╗ ███████╗     ██████╗ ██████╗ ███╗   ██╗████████╗██████╗  ██████╗ ██╗         ██╗      ██████╗  ██████╗ ██╗ ██████╗--
-- ██╔══██╗╚══██╔══╝████╗ ████║██╔══██╗    ██╔══██╗██╔════╝    ██╔════╝██╔═══██╗████╗  ██║╚══██╔══╝██╔══██╗██╔═══██╗██║         ██║     ██╔═══██╗██╔════╝ ██║██╔════╝--
-- ██║  ██║   ██║   ██╔████╔██║██████╔╝    ██████╔╝█████╗      ██║     ██║   ██║██╔██╗ ██║   ██║   ██████╔╝██║   ██║██║         ██║     ██║   ██║██║  ███╗██║██║     --
-- ██║  ██║   ██║   ██║╚██╔╝██║██╔══██╗    ██╔══██╗██╔══╝      ██║     ██║   ██║██║╚██╗██║   ██║   ██╔══██╗██║   ██║██║         ██║     ██║   ██║██║   ██║██║██║     --
-- ██████╔╝   ██║   ██║ ╚═╝ ██║██║  ██║    ██║  ██║██║         ╚██████╗╚██████╔╝██║ ╚████║   ██║   ██║  ██║╚██████╔╝███████╗    ███████╗╚██████╔╝╚██████╔╝██║╚██████╗--
-- ╚═════╝    ╚═╝   ╚═╝     ╚═╝╚═╝  ╚═╝    ╚═╝  ╚═╝╚═╝          ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚══════╝    ╚══════╝ ╚═════╝  ╚═════╝ ╚═╝ ╚═════╝--
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                                                                                                                  
-- the combinational voting for dTMR
  dTMR_RF_COMB : process(all)  
  begin

  WB_RD_buf_wire        <= WB_RD_buf;
  WB_RD_voted_wire      <= WB_RD_voted;
  rs1_bypass_wire       <= rs1_bypass_lat;
  rs2_bypass_wire       <= rs2_bypass_lat;
  restore_fault_RF_wire <= restore_fault_RF;
  WB_no_done <= '0';

  -- FAULT TOLERANT VOTER
  --parity1      <= '0';
  --parity_voted <= '0';

  if rst_ni = '0' then
    for h in harc_range loop
      WB_RD_buf_wire(h) <= (others => '0');
    end loop;
    WB_RD_voted_wire <= (others => '0');
    rs1_bypass_wire <= '0';
    rs2_bypass_wire <= '0';
  end if; 


  -- Check the dependencies between the operands of adjacent instructions, if the rs1 or rs2 are equal
  -- to the destination register of the previous instruction (i.e. the one in WB) then I don't take the values
  -- from the RF (because they won't be written yet), but I get them from the buffers.
  if RD_WB /= 0 and (  harc_EXEC = 2 or harc_EXEC = 1 or ( harc_EXEC = 0 and harc_ID = 2 and LS_is_running = '1') ) then
    if ( rs1(instr_word_ID) = RD_WB ) and ( rs2(instr_word_ID) = RD_WB )  then
      rs1_bypass_wire <= '1';
      rs2_bypass_wire <= '1';
    elsif rs1(instr_word_ID) = RD_WB then
      rs1_bypass_wire <= '1';
      rs2_bypass_wire <= '0';
    elsif rs2(instr_word_ID) = RD_WB then
      rs1_bypass_wire <= '0';
      rs2_bypass_wire <= '1';
    else
      rs1_bypass_wire <= '0';
      rs2_bypass_wire <= '0';
    end if;
  elsif RD_WB = 0 and ( harc_EXEC = 1 or harc_EXEC = 0 ) then 
      rs1_bypass_wire <= '0';
      rs2_bypass_wire <= '0';  
  end if;

  -- Check if the bypass is still valid for thread 1, in some cases due to faults the bypass can be activated or deactivated
  -- for this you need to double check on thread 1, based on what is done for thread 2 (previous). Basically the two threads
  -- must have a separate bypass control.
  if harc_EXEC = 2  and rs1_bypass_lat = '1' then
    if ( rs1(instr_word_ID) = RD_WB ) and ( rs2(instr_word_ID) = RD_WB )  then
      rs1_bypass_wire <= '1';
    elsif rs1(instr_word_ID) = RD_WB then
      rs1_bypass_wire <= '1';
    elsif rs2(instr_word_ID) = RD_WB then
      rs1_bypass_wire <= '0';
    else
      rs1_bypass_wire <= '0';
      rs2_bypass_wire <= '0';  
    end if;
  end if;    

  if harc_EXEC = 2  and rs2_bypass_lat = '1' then
    if ( rs1(instr_word_ID) = RD_WB ) and ( rs2(instr_word_ID) = RD_WB )  then
      rs2_bypass_wire <= '1';
    elsif rs1(instr_word_ID) = RD_WB then
      rs2_bypass_wire <= '0';
    elsif rs2(instr_word_ID) = RD_WB then
      rs2_bypass_wire <= '1';
    else
      rs1_bypass_wire <= '0';
      rs2_bypass_wire <= '0';  
    end if;
  end if;    

  if restore_fault_PC = '1' and ( harc_EXEC /= 0 or (harc_EXEC = 0 and WB_done = '1' )) then
      rs1_bypass_wire <= '0';
      rs2_bypass_wire <= '0';
  end if;    



  -- Always vote among the RFs so as to always have the right result on the wire which will always be read during operands reading
  for i in  RF_SIZE-1 downto 0  loop  
  --  regfile_voted_wire(i) <= ( regfile(2)(i) and regfile(1)(i) ) or ( regfile(2)(i) and regfile(0)(i) ) or ( regfile(1)(i) and regfile(0)(i) );  
    if ( regfile(2)(i) xor regfile(1)(i) ) = ( 0 to 31 => '0' ) then
      regfile_voted_wire(i) <= regfile(1)(i); 
    elsif ( regfile(2)(i) xor regfile(1)(i) ) = ( 0 to 31 => '1' ) then
      regfile_voted_wire(i) <= regfile(0)(i); 
    end if;        
  end loop;  




  -- WB_RD buffer filling and fault control 
  if WB_EN = '1' then  

    -- Fill the WB_buffers both in case of restore (thread 0) or not (thread 1)
    WB_RD_buf_wire(harc_WB) <= WB_RD;    
    if  harc_WB = 1 and restore_fault_PC = '0' then 
    -- If there is a load since there is only one I take the value directly without doing voting, this can be a problem when harc_WB is set to zero 
    -- as the default condition. However even if WB_RD_voted_wire is dirty, this value should never be written into the RF WB_EN_buf is not set.
      if add_vect_bits(load_op_buf_lat) >= 1 then
        if restore_fault_WB = '0' then
          WB_RD_voted_wire    <= WB_RD;
          WB_RD_buf_wire(2)   <= WB_RD;
          WB_RD_buf_wire(1)   <= WB_RD;
          --FAULT TOLERANT VOTER 
--          parity1 <= xor WB_RD_buf_wire(2);
--          parity_voted <= xor WB_RD_voted_wire;
--          restore_fault_RF_wire <= parity1 xor parity_voted;
        else
          restore_fault_RF_wire <= '1';
        end if;  
      else
        -- Vote between the writeback buffers, if they are the same ok, otherwise I start the restore_fault_RF procedure.
        if WB_RD_buf(2) = WB_RD and restore_fault_WB = '0' then
          WB_RD_voted_wire    <= WB_RD_buf_wire(2);
          --FAULT TOLERANT VOTER 
--          parity1 <= xor WB_RD_buf_wire(2);
--          parity_voted <= xor WB_RD_voted_wire;
--          restore_fault_RF_wire <= parity1 xor parity_voted;
        else
          restore_fault_RF_wire <= '1';
        end if;  
      end if;      

    elsif harc_WB = 0 and harc_IF = 2 then
        WB_RD_voted_wire    <= WB_RD;
        WB_RD_buf_wire(2)   <= WB_RD;
        WB_RD_buf_wire(1)   <= WB_RD;
        restore_fault_RF_wire <= '0';
    end if;  



    -- Checks if a WB was not performed due to faults. If a fault avoid the WB of the instruction, a restore procedure should start
    if ( restore_fault_PC = '0' and restore_fault_WB = '0' and LS_is_running = '0' and harc_ID = 2 and WB_EN_buf = "010" ) or      
       ( restore_fault_PC = '0' and restore_fault_WB = '0' and LS_is_running = '0' and harc_ID = 1 and harc_EXEC = 2 and harc_WB = 1 and WB_EN_buf = "110" ) or
       ( restore_fault_PC = '0' and restore_fault_WB = '0' and LS_is_running = '0' and harc_ID = 1 and harc_EXEC = 2 and harc_WB = 1 and add_vect_bits(WB_EN_buf) = 0 ) then    
       restore_fault_RF_wire <= '1';
       WB_no_done <= '1';  --QUESTO FORSE NON CI VA MESSO
    end if;
    if ( harc_ID = 2 and restore_fault_PC = '0' and restore_fault_WB = '1' and LS_is_running = '0' ) then      
      restore_fault_RF_wire <= '1';
      WB_no_done <= '1';
    end if;
 
  -- Checks if a WB was not performed due to faults. If a fault avoid the WB of the instruction, a restore procedure should start
  elsif ( restore_fault_PC = '0' and restore_fault_WB = '0' and harc_ID = 1 and add_vect_bits(WB_EN_buf) = 1 ) or
        ( restore_fault_PC = '0' and restore_fault_WB = '0' and harc_ID = 2 and LS_is_running = '0' and WB_EN_buf = "010" ) or
        ( restore_fault_PC = '0' and restore_fault_WB = '1' and harc_ID = 1 and LS_is_running= '0' ) or
        ( restore_fault_PC = '0' and restore_fault_WB = '0' and harc_ID = 1 and harc_EXEC = 2 and harc_WB = 2 and add_vect_bits(WB_EN_buf) = 2 ) or
        ( restore_fault_PC = '0' and restore_fault_WB = '0' and harc_ID = 1 and harc_EXEC = 1 and harc_WB = 1 and add_vect_bits(WB_EN_buf) = 0 ) then
    WB_no_done <= '1';
    restore_fault_RF_wire <= '1';
  end if;

  -- when Edge faults on WB_EN signals avoid writing back in the RF
  if WB_EN_buf = "110" and WB_fault = '0' and LS_is_running = '0' then
    restore_fault_RF_wire <= '1';
    WB_no_done <= '1';
  end if;   

  -- Reset restore_fault_RF_wire signal after a WB performed by thread 0 (it means the restore operation ends) 
  if harc_WB = 0 and harc_IF = 2 then
    restore_fault_RF_wire <= '0';
  end if;            
  end process;




  dTMR_RF_SYNCH : process(clk_i, rst_ni)  -- synch single state process
  begin
    if rst_ni = '0' then
      for h in harc_range loop
        WB_RD_buf(h) <= (others => '0');
      end loop;
      restore_fault_RF <= '0';
      LS_WB_wrong_EXEC_lat <= '0';
      LS_WB_wrong_END <= '0';
    elsif rising_edge(clk_i) then
      WB_RD_buf            <= WB_RD_buf_wire;
      restore_fault_RF     <= restore_fault_RF_wire;
      LS_WB_wrong_EXEC_lat <= LS_WB_wrong_EXEC_wire;
      LS_WB_wrong          <= LS_WB_wrong_EXEC;
      LS_WB_wrong_END      <= '1' when LS_WB_wrong = '1' and LS_WB_wrong_EXEC_lat = '1' else '0';
    end if;      
  end process;    

                                                                         
------------------------------------------------------------------------------
--██╗    ██╗██████╗ ██╗████████╗███████╗    ██████╗  █████╗  ██████╗██╗  ██╗--
--██║    ██║██╔══██╗██║╚══██╔══╝██╔════╝    ██╔══██╗██╔══██╗██╔════╝██║ ██╔╝--
--██║ █╗ ██║██████╔╝██║   ██║   █████╗█████╗██████╔╝███████║██║     █████╔╝ --
--██║███╗██║██╔══██╗██║   ██║   ██╔══╝╚════╝██╔══██╗██╔══██║██║     ██╔═██╗ --
--╚███╔███╔╝██║  ██║██║   ██║   ███████╗    ██████╔╝██║  ██║╚██████╗██║  ██╗--
-- ╚══╝╚══╝ ╚═╝  ╚═╝╚═╝   ╚═╝   ╚══════╝    ╚═════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝--
------------------------------------------------------------------------------

  -- harc_WB management
  harc_WB <= harc_LS_WB when   LS_WB_EN = '1' or restore_store_active = '1' else 
            harc_IE_WB when ( IE_WB_EN = '1' and LS_is_running_wire = '0') or MUL_WB_EN = '1' else
                0      when harc_IE_WB = 0 and ( harc_EXEC = 0 and harc_IF = 2 )  else 
            harc_LS_WB when harc_WB_lat = 0 and ( harc_EXEC /= 0 )  else 
            harc_WB_lat;

  instr_word_WB <= instr_word_LS_WB when LS_WB_EN = '1' else instr_word_IE_WB;
  WB_EN         <= LS_WB_EN or IE_WB_EN or MUL_WB_EN;
  WB_RD         <= LS_WB when LS_WB_EN = '1' else MUL_WB when MUL_WB_EN = '1' else IE_WB;  

  LS_WB_wrong_EXEC_wire <= '1' when ((add_vect_bits(load_op_buf_wire) >= 1) and core_busy_LS_lat = '1' and harc_EXEC = 1 and (rs1_bypass_wire = '1' or rs2_bypass_wire = '1' ) ) else '0';
  LS_WB_wrong_EXEC <= LS_WB_wrong_EXEC_lat or LS_WB_wrong_EXEC_wire;

------------------------------------------------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                   
--██████╗ ████████╗███╗   ███╗██████╗     ██╗    ██╗██████╗      ██████╗ ██████╗ ███╗   ██╗████████╗██████╗  ██████╗ ██╗         ██╗      ██████╗  ██████╗ ██╗ ██████╗--
--██╔══██╗╚══██╔══╝████╗ ████║██╔══██╗    ██║    ██║██╔══██╗    ██╔════╝██╔═══██╗████╗  ██║╚══██╔══╝██╔══██╗██╔═══██╗██║         ██║     ██╔═══██╗██╔════╝ ██║██╔════╝--
--██║  ██║   ██║   ██╔████╔██║██████╔╝    ██║ █╗ ██║██████╔╝    ██║     ██║   ██║██╔██╗ ██║   ██║   ██████╔╝██║   ██║██║         ██║     ██║   ██║██║  ███╗██║██║     --
--██║  ██║   ██║   ██║╚██╔╝██║██╔══██╗    ██║███╗██║██╔══██╗    ██║     ██║   ██║██║╚██╗██║   ██║   ██╔══██╗██║   ██║██║         ██║     ██║   ██║██║   ██║██║██║     --
--██████╔╝   ██║   ██║ ╚═╝ ██║██║  ██║    ╚███╔███╔╝██████╔╝    ╚██████╗╚██████╔╝██║ ╚████║   ██║   ██║  ██║╚██████╔╝███████╗    ███████╗╚██████╔╝╚██████╔╝██║╚██████╗--
--╚═════╝    ╚═╝   ╚═╝     ╚═╝╚═╝  ╚═╝     ╚══╝╚══╝ ╚═════╝      ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚══════╝    ╚══════╝ ╚═════╝  ╚═════╝ ╚═╝ ╚═════╝--
------------------------------------------------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                 

  dTMR_WB_COMB : process(all)  -- wb combinational control
  begin
    WB_EN_buf_wire <= WB_EN_buf; 
    WB_done_wire   <= WB_done;
    for h in harc_range loop
      RD_WB_buf_wire(h) <= RD_WB_buf(h);
    end loop;

    -- WB_EN_buf_wire is used to collect the WB_ENs of the different threads, and to write to the RF only when at least 2 are 1, i.e. they received a WB_ENs.
    if WB_EN = '1' and ( restore_fault_PC_wire = '0' or harc_WB = 0 )then
      if ((harc_EXEC = 1  or harc_loss = 1 )and harc_WB = 1) then
        WB_EN_buf_wire(2) <= '1';          
        WB_EN_buf_wire(1) <= '1';          
        WB_EN_buf_wire(0) <= '0';          
      elsif (harc_EXEC = 1  or harc_loss = 1 )  then
        WB_EN_buf_wire(2) <= '1';          
        WB_EN_buf_wire(1) <= '0';          
        WB_EN_buf_wire(0) <= '0';          
      elsif ( harc_EXEC = 2  )  then
        WB_EN_buf_wire(1) <= '1';          
        WB_EN_buf_wire(0) <= '0';          
      elsif (harc_EXEC = 0 and  restore_fault_PC = '1' ) then -- potrei scrivere and harc_EXEC = 0
        WB_EN_buf_wire <= (others =>'1');
      end if;      
    elsif  restore_fault_PC_wire = '1' then
      WB_EN_buf_wire <= (others =>'0');
    elsif  ((harc_EXEC = 1 or harc_loss = 1) and harc_WB = 2  and add_vect_bits(WB_EN_buf) = 1 ) then
      WB_EN_buf_wire <= "100";
    elsif  (((harc_EXEC = 1 or harc_loss = 1) and harc_WB = 2 ) or ( harc_EXEC = 2 and harc_WB = 1 and add_vect_bits(WB_EN_buf) = 2 ) or ( harc_EXEC = 0 ) )  then
      WB_EN_buf_wire <= (others =>'0');
    elsif  ((harc_EXEC = 1 or harc_loss = 1) and harc_WB = 1  and add_vect_bits(WB_EN_buf) = 2 ) then
      WB_EN_buf_wire <= (others =>'0');
    elsif  ((harc_EXEC = 2 or harc_loss = 2) and harc_WB = 2  and harc_ID = 2  ) then
      WB_EN_buf_wire <= (others =>'0');
    elsif  ((harc_EXEC = 2 or harc_loss = 2) and harc_WB = 2  and add_vect_bits(WB_EN_buf) = 2 ) then
      WB_EN_buf_wire <= (others =>'0');
    elsif  ((harc_EXEC = 2 or harc_loss = 2) and harc_WB = 2  and add_vect_bits(WB_EN_buf) = 1 ) then
      WB_EN_buf_wire <= "100";
    end if;



    -- RD_WB_buf takes into account the destination registers of the various decoded instructions, cointaining the address in which you have to perform a WB
    if dest_valid = '1' then
      if harc_ID /= 0 and restore_fault_PC = '1' then
        -- I want to rewrite the RD_WB_wire with the correct one to avoid futur activation of restore_fault_WB procedure
        for h in harc_range loop
          RD_WB_buf_wire(h) <= RD_WB;
        end loop;
      else
        RD_WB_buf_wire(harc_ID)  <= rd(instr_word_ID); -- is need to take the operand from RF
      end if;
    else 
        for h in harc_range loop
          RD_WB_buf_wire(h) <= RD_WB;
        end loop;    
    end if;  

    -- I want to rewrite the RD_WB_wire with the correct one to avoid futur activation of restore_fault_WB procedure
    if restore_fault = '1' then
      for h in harc_range loop
        RD_WB_buf_wire(h) <= RD_WB;
      end loop;
    end if;



--FORSE DOVREI METTERLO QUANDO EFFETTIVAMENTE SCRIVE NEL RF
    -- I want to keep trace about the performed WB, to maintain avoid the repetition of alread performed WB on RF 
    if  ( harc_WB = 1 or ( harc_WB =0 and WB_done = '0') ) and add_vect_bits(WB_EN_buf_wire) > 1   then --  ( WB_EN_lat = '1' or LS_PE_WB_EN = '1' )then
      WB_done_wire <= '1';
      if  edge_fault = '1' then
        WB_done_wire <= '0';
      end if;
    elsif ( (restore_fault_PC_wire = '0' or (restore_fault_LSU = '1' and (pc_IF /= WB_done_PC and pc_ID /= WB_done_PC and pc_IE /= WB_done_PC)) ) and busy_ID = '0' and harc_EXEC /= harc_WB ) or edge_fault ='1'  then
      WB_done_wire <= '0';
    end if;        
    -- when Edge faults on WB_EN signals avoid writing back in the RF
    if WB_EN_buf = "110" and WB_fault = '0' then
      WB_done_wire <= '0';
    end if;   

  end process;




  dTMR_WB_SYNCH : process(clk_i, rst_ni)  -- synch single state process
  begin
    if rst_ni = '0' then
      harc_WB_lat      <=  0;
      WB_EN_buf        <=  (others => '0');
      WB_RD_voted      <=  (others => '0');
      RD_WB         <=  0;
      rs1_bypass_lat   <= '0';
      rs2_bypass_lat   <= '0';
      restore_fault_WB <= '0';
      WB_done          <= '0';
      WB_done_PC       <= (others => '0');
      for h in harc_range loop
        RD_WB_buf(h) <= 0;
      end loop;

    elsif rising_edge(clk_i) then
      harc_WB_lat  <= harc_WB;
      WB_RD_voted <= WB_RD_voted_wire;
      WB_EN_buf <= WB_EN_buf_wire;
      WB_done <= WB_done_wire;
      RD_WB_buf <= RD_WB_buf_wire;

      rs1_bypass_lat <= rs1_bypass_wire;
      rs2_bypass_lat <= rs2_bypass_wire;


    -- Vote between the registers that hold the address who has to do wb during harc 0, only if at least 2 out of 3 are the same 
    -- Voting is done asynchronously to avoid that during instructions that last more than 1 cycle, these values are changed
      if harc_ID = 1 and restore_fault_PC = '0' then
          -- if they are all different i maintain the previous value
          if ( RD_WB_buf_wire(2) = RD_WB_buf_wire(1) ) then
            if dest_valid = '1' and busy_ID = '0' then
              RD_WB <= RD_WB_buf_wire(1); 
            else
              RD_WB <= 0;                
            end if;     
          elsif ( RD_WB_buf_wire(2) /= RD_WB_buf_wire(1) ) and restore_fault_PC_wire = '0' then
            restore_fault_WB <= '1';
            RD_WB <= 0;                
          end if;
      elsif harc_ID = 0 and restore_fault_PC = '1' and dest_valid = '1' then
        RD_WB <= RD_WB_buf_wire(0);      
      elsif harc_ID = 0 and restore_fault_PC = '1' and dest_valid = '0' then
        RD_WB <= 0;      
      elsif (harc_ID = 0 or harc_ID = 1 ) and  RD_Addr_IE = "00000" then
        RD_WB <= 0;                
      end if;

      if harc_WB = 0  then
        restore_fault_WB <= '0';
      end if;

      -- I want to keep trace about the performed WB, to maintain avoid the repetition of alread performed WB on RF maintaining the WB Program Counter
--      if  ( harc_WB = 1 or ( harc_WB =0 and WB_done = '0') ) and add_vect_bits(WB_EN_buf_wire) > 1  and LS_is_running_wire = '0' then --  ( WB_EN_lat = '1' or LS_PE_WB_EN = '1' )then
      if  ( harc_WB = 1 or ( harc_WB =0 and WB_done = '0') ) and add_vect_bits(WB_EN_buf_wire) > 1  then --  ( WB_EN_lat = '1' or LS_PE_WB_EN = '1' )then
        WB_done_PC <= pc_WB when WB_done = '0' or (WB_done = '1' and WB_done_wire = '1');
      end if;  -- clk
  

    end if;  -- clk
  end process;

--------------------------------------------------------------------- end of WB Stage ----------------
------------------------------------------------------------------------------------------------------



  ------------------------------------------------------------------------------------------------------
  --   ██████╗ ██████╗ ███╗   ███╗██████╗  █████╗ ██████╗  █████╗ ████████╗ ██████╗ ██████╗ ███████╗  --
  --  ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝  --
  --  ██║     ██║   ██║██╔████╔██║██████╔╝███████║██████╔╝███████║   ██║   ██║   ██║██████╔╝███████╗  --
  --  ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██╔══██║██╔══██╗██╔══██║   ██║   ██║   ██║██╔══██╗╚════██║  --
  --  ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║  ██║██║  ██║██║  ██║   ██║   ╚██████╔╝██║  ██║███████║  --
  --   ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝  --
  ------------------------------------------------------------------------------------------------------

  comparator_enable_comb : process(rst_ni, clk_i)
  begin
    if rst_ni = '0' then
      pass_BEQ  <= '0';
      pass_BNE  <= '0';
      pass_BLT  <= '0';
      pass_BGE  <= '0';
      pass_BLTU <= '0';
      pass_BGEU <= '0';
      zero_rs1  <= '0';
      zero_rs2  <= '0';
    elsif rising_edge(clk_i) then
      if core_busy_IE = '1' or core_busy_LS = '1' or ls_parallel_exec = '0'  or dsp_parallel_exec = '0' then --or data_dependency = '1' then -- the instruction pipeline is halted
      elsif instr_rvalid_ID_int = '0' then -- wait for a valid instruction
      else  -- process the incoming instruction 
        pass_BEQ  <= '0';
        pass_BNE  <= '0';
        pass_BLT  <= '0';
        pass_BGE  <= '0';
        pass_BLTU <= '0';
        pass_BGEU <= '0';
        zero_rs1  <= '0';
        zero_rs2  <= '0';
        if unsigned(RS1_Data_IE_wire) = 0 then
          zero_rs1 <= '1';
        end if;
        if unsigned(RS2_Data_IE_wire) = 0 then
          zero_rs2 <= '1';
        end if;
        if (signed(RS1_Data_IE_wire) = signed(RS2_Data_IE_wire)) then
          pass_BEQ <= '1';
        else
          pass_BNE <= '1';
        end if;
        if (signed(RS1_Data_IE_wire) < signed(RS2_Data_IE_wire)) then
          pass_BLT <= '1';
        else
          pass_BGE <= '1';
        end if;
        if (unsigned(RS1_Data_IE_wire) < unsigned(RS2_Data_IE_wire)) then
          pass_BLTU <= '1';
        else
          pass_BGEU <= '1';
        end if;
      end if;
    end if;
  end process;

------------------------------------------------------------------------------------------
--  ███████╗██████╗ ███╗   ███╗    ███╗   ███╗ █████╗ ██████╗ ██████╗ ███████╗██████╗   --
--  ██╔════╝██╔══██╗████╗ ████║    ████╗ ████║██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔══██╗  --
--  ███████╗██████╔╝██╔████╔██║    ██╔████╔██║███████║██████╔╝██████╔╝█████╗  ██████╔╝  --
--  ╚════██║██╔═══╝ ██║╚██╔╝██║    ██║╚██╔╝██║██╔══██║██╔═══╝ ██╔═══╝ ██╔══╝  ██╔══██╗  --
--  ███████║██║     ██║ ╚═╝ ██║    ██║ ╚═╝ ██║██║  ██║██║     ██║     ███████╗██║  ██║  --
--  ╚══════╝╚═╝     ╚═╝     ╚═╝    ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝     ╚══════╝╚═╝  ╚═╝  --
------------------------------------------------------------------------------------------                                                           

  spm_mapper : if accl_en = 1 generate 
  Spm_Addr_Mapping : process(all)
  begin
  ID_rs1_to_sc <= std_logic_vector(to_unsigned(SPM_NUM, SPM_ADDR_WID));  -- we assign SPM_NUM to rs1_to_sc as a default case which is out of range (0 to SPM_NUM-1)
  ID_rs2_to_sc <= std_logic_vector(to_unsigned(SPM_NUM, SPM_ADDR_WID));  -- we assign SPM_NUM to rs2_to_sc as a default case which is out of range (0 to SPM_NUM-1)
  ID_rd_to_sc  <= std_logic_vector(to_unsigned(SPM_NUM, SPM_ADDR_WID));  -- we assign SPM_NUM to rd_to_sc  as a default case which is out of range (0 to SPM_NUM-1)
    for i in 0 to SPM_NUM-1 loop -- Decode the address and assign and set the scratchpad number (0 to SPM_NUM-1) to the operand
      if RS1_Data_IE_wire(31 downto Addr_Width) >= std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i)) and
         RS1_Data_IE_wire(31 downto Addr_Width) <  std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i+1)) then
        ID_rs1_to_sc <= std_logic_vector(to_unsigned(i, SPM_ADDR_WID));
      end if;
      if RS2_Data_IE_wire(31 downto Addr_Width) >= std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i)) and
         RS2_Data_IE_wire(31 downto Addr_Width) <  std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i+1)) then
        ID_rs2_to_sc <= std_logic_vector(to_unsigned(i, SPM_ADDR_WID));
      end if;
      if RD_Data_IE_wire(31 downto Addr_Width)  >= std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i)) and
         RD_Data_IE_wire(31 downto Addr_Width)  <  std_logic_vector(unsigned(SPM_STRT_ADDR(31 downto Addr_Width)) + (i+1)) then
        ID_rd_to_sc <= std_logic_vector(to_unsigned(i, SPM_ADDR_WID));
      end if;
    end loop;
  end process;

  Spm_Addr_Mapping_Synch : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
    elsif rising_edge(clk_i) then
      rs1_to_sc <= ID_rs1_to_sc;
      rs2_to_sc <= ID_rs2_to_sc;
      rd_to_sc  <= ID_rd_to_sc;
    end if;
  end process;
  
  end generate;




  -----------------------------------------------------------------------------------------------
  --   █████╗ ██████╗ ██████╗ ██████╗ ███████╗███████╗███████╗     ██████╗ ███████╗███╗   ██╗  --
  --  ██╔══██╗██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝    ██╔════╝ ██╔════╝████╗  ██║  --
  --  ███████║██║  ██║██║  ██║██████╔╝█████╗  ███████╗███████╗    ██║  ███╗█████╗  ██╔██╗ ██║  --
  --  ██╔══██║██║  ██║██║  ██║██╔══██╗██╔══╝  ╚════██║╚════██║    ██║   ██║██╔══╝  ██║╚██╗██║  --
  --  ██║  ██║██████╔╝██████╔╝██║  ██║███████╗███████║███████║    ╚██████╔╝███████╗██║ ╚████║  --
  --  ╚═╝  ╚═╝╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝     ╚═════╝ ╚══════╝╚═╝  ╚═══╝  --
  -----------------------------------------------------------------------------------------------


  LSU_Address_Mapper_comb : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
    elsif rising_edge(clk_i) then
        -- Perform the addition ---------------------------------------------------
        add_out_load <= std_logic_vector(signed(RS1_data_IE_wire) + signed(I_immediate(instr_word_ID)));
        add_out_store <= std_logic_vector(signed(RS1_data_IE_wire) + signed(S_immediate(instr_word_ID)));
    ---------------------------------------------------------------------------
    end if;
  end process;



---------------------------------------------------------------------- end of ID stage -----------
--------------------------------------------------------------------------------------------------
end RF;
--------------------------------------------------------------------------------------------------
-- END of ID architecture ------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------