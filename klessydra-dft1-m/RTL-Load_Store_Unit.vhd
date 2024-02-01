--------------------------------------------------------------------------------------------------------------
--  LSU -- (Load-Store Unit)                                                                                --
--  Author(s): Abdallah Cheikh abdallah.cheikh@uniroma1.it (abdallah93.as@gmail.com)                        --
--                                                                                                          --
--  Date Modified: 8-12-2019                                                                                --
--------------------------------------------------------------------------------------------------------------
--  The LSU performs all the operations that access the external memories. Including the amoswap, and the   --
--  custom burst load and store instructions. The LSU can allow superscalar execution with other execution  --
--  units if a store operation is executing, Loaded instructions write either to the regfile or the SPMs    --
--------------------------------------------------------------------------------------------------------------

-- ieee packages ------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

-- local packages ------------
use work.riscv_klessydra.all;
--use work.klessydra_parameters.all;

-- LD-STR pinout --------------------
entity Load_Store_Unit is
  generic(
    THREAD_POOL_SIZE           : natural;
    fetch_stage_en             : natural;
    accl_en                    : natural;
    replicate_accl_en          : natural;
    SIMD                       : natural;
    SPM_NUM                    : natural;  
    Addr_Width                 : natural;
    Data_Width                 : natural;
    SIMD_BITS                  : natural;
    ACCL_NUM                   : natural;
    SPM_ADDR_WID               : natural
    );
  port (
    -- clock, and reset active low
    clk_i, rst_ni              : in  std_logic;
    -- Program Counter Signals
    irq_pending                : in  std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    -- ID_Stage Signals
    RS1_Data_IE                : in  std_logic_vector(31 downto 0);
    RS2_Data_IE                : in  std_logic_vector(31 downto 0);
    RD_Data_IE                 : in  std_logic_vector(31 downto 0);
    instr_word_IE              : in  std_logic_vector(31 downto 0);
    pc_IE                      : in  std_logic_vector(31 downto 0);
    decoded_instruction_LS     : in  std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
    data_be_ID                 : in  std_logic_vector(3 downto 0);
    data_width_ID              : in  std_logic_vector(1 downto 0);
    harc_EXEC                  : in  natural range THREAD_POOL_SIZE-1 downto 0;
    LS_instr_req               : in  std_logic;
    load_op                    : in  std_logic;
    store_op                   : in  std_logic;
    --sw_mip                     : in  std_logic;
    core_busy_LS               : out std_logic;
    core_busy_LS_lat               : out std_logic;
    busy_LS                    : out std_logic;
    -- dtmr sygnals
    LS_WB_wrong_EXEC_wire         : in std_logic;
    LS_WB_wrong                   : in std_logic;
    LS_WB_wrong_END               : in std_logic;
    load_op_buf_wire              : out std_logic_vector(2 downto 0);  
    load_op_buf_lat               : out std_logic_vector(2 downto 0);  
    LS_is_running_wire            : out std_logic;
    LS_is_running                 : out std_logic;
    restore_fault_PC              : in std_logic;
    restore_fault_PC_wire         : in std_logic;
    restore_fault                 : in std_logic;
    restore_fault_lat             : in std_logic;
    restore_fault_LSU_wire        : out std_logic;
    restore_fault_LSU             : out std_logic;
    restore_store_active          : out std_logic;
    restore_stall                 : in std_logic;
    add_out_load                  : in std_logic_vector(31 downto 0);
    add_out_store                 : in std_logic_vector(31 downto 0);
    pippo                         : out std_logic;

    -- Processing Pipeline Signals
    rs1_to_sc                  : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rs2_to_sc                  : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    rd_to_sc                   : in  std_logic_vector(SPM_ADDR_WID-1 downto 0);
    halt_LSU                   : in  std_logic;
    data_addr_internal         : out std_logic_vector(31 downto 0);
    ls_except_data             : out std_logic_vector(31 downto 0);
    ls_except_condition        : out std_logic;
    ls_taken_branch            : out std_logic;
    LSU_flush_hart_FETCH       : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    LSU_flush_hart_ID          : out std_logic_vector(THREAD_POOL_SIZE-1 downto 0);
    amo_load                   : in  std_logic;
    amo_load_skip              : in  std_logic;
    amo_store                  : out std_logic;
    -- CSR Signals
    misaligned_err             : out std_logic;
    -- Scratchpad Interface Signals
    ls_data_gnt_i              : in  std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_wr_gnt              : in  std_logic;
    ls_sc_data_read_wire       : in  std_logic_vector(Data_Width-1 downto 0);
    state_LS                   : out fsm_LS_states;
    harc_LS_wire               : out integer range ACCL_NUM-1 downto 0;
    sc_word_count_wire         : out integer;
    spm_bcast                  : out std_logic;
    kmemld_inflight            : out std_logic_vector(SPM_NUM-1 downto 0);
    kmemstr_inflight           : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_req                 : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sci_we                  : out std_logic_vector(SPM_NUM-1 downto 0);
    ls_sc_read_addr            : out std_logic_vector(Addr_Width-(SIMD_BITS+3) downto 0);
    ls_sc_write_addr           : out std_logic_vector(Addr_Width-(SIMD_BITS+3)downto 0);
    ls_sc_data_write_wire      : out std_logic_vector(Data_Width-1 downto 0);
    -- WB_Stage Signals
    LS_WB_EN                   : out std_logic;
    LS_WB_EN_wire              : out std_logic;
    harc_LS_WB                 : out natural range THREAD_POOL_SIZE-1 downto 0;
    harc_WB                    : in natural range THREAD_POOL_SIZE-1 downto 0;
    instr_word_LS_WB           : out std_logic_vector(31 downto 0);
    LS_WB                      : out std_logic_vector(31 downto 0);
    -- Data memory interface
    data_req_o                 : out std_logic;
    data_gnt_i                 : in  std_logic;
    data_rvalid_i              : in  std_logic;
    data_we_o                  : out std_logic;
    data_be_o                  : out std_logic_vector(3 downto 0);
    data_addr_o                : out std_logic_vector(31 downto 0);
    data_wdata_o               : out std_logic_vector(31 downto 0);
    data_rdata_i               : in  std_logic_vector(31 downto 0);
    data_err_i                 : in  std_logic
	);
end entity;  ------------------------------------------

architecture LSU of Load_Store_Unit is

  subtype harc_range is natural range THREAD_POOL_SIZE-1 downto 0;
  subtype accl_range is integer range ACCL_NUM-1 downto 0; 

  --dTMR signals
--  alias parallel_exec_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Pipe.ls_parallel_exec  : std_logic >>;
--  alias fault_PC_lat_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Prg_Ctr.fault_PC_lat : std_logic >>;
--  alias harc_fault_wire_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Prg_Ctr.harc_fault_wire  : std_logic_vector (2 downto 0) >>;
--  alias harc_fault_wire_lat_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Prg_Ctr.harc_fault_wire_lat  : std_logic_vector (1 downto 0) >>;
--  alias harc_loss_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Prg_Ctr.harc_loss  : integer >>;
--  alias fault_harc_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.Prg_Ctr.fault_harc  : std_logic >>;      

--  alias harc_ID_alias is  << signal .tb.top_i.core_region_i.CORE.RISCV_CORE.harc_ID : harc_range >>;

--  signal  load_op_buf_wire           :      std_logic_vector(2 downto 0);   
--  signal  load_op_buf_lat            :      std_logic_vector(2 downto 0);   
--  signal  LS_is_running_wire         :      std_logic;
--  signal  LS_is_running              :      std_logic;
  signal  LS_is_running_lat          :      std_logic;
  signal  ls_instr_req_buf_wire      :      std_logic_vector(2 downto 0);   
  signal  ls_instr_req_buf           :      std_logic_vector(2 downto 0);   

  signal data_addr_internal_buf_wire       : array_2D(harc_range)(31 downto 0);
  signal data_addr_internal_buf            : array_2D(harc_range)(31 downto 0);
  signal data_wdata_o_buf_wire             : array_2D(harc_range)(31 downto 0);
  signal data_wdata_o_buf             : array_2D(harc_range)(31 downto 0);
  signal data_we_o_buf_wire                : std_logic_vector (2 downto 0);
  signal data_we_o_buf                : std_logic_vector (2 downto 0);
  signal data_req_o_buf_wire               : std_logic_vector (2 downto 0);
  signal data_req_o_buf               : std_logic_vector (2 downto 0);
  signal data_be_internal_buf_wire         : array_2D(harc_range)(3 downto 0);
  signal data_be_internal_buf         : array_2D(harc_range)(3 downto 0);

  signal data_addr_internal_voted         : std_logic_vector(31 downto 0); 
  signal data_addr_internal_voted_wire    : std_logic_vector(31 downto 0); 
  signal data_wdata_o_voted_wire           : std_logic_vector(31 downto 0); 
  signal data_wdata_o_voted                : std_logic_vector(31 downto 0); 
  signal data_we_o_voted_wire              : std_logic;
  signal data_we_o_voted                   : std_logic;
  signal data_req_o_voted_wire             : std_logic;
  signal data_req_o_voted                  : std_logic;
  signal data_be_internal_voted_wire       : std_logic_vector(3 downto 0); 
  signal data_be_internal_voted            : std_logic_vector(3 downto 0); 

  signal pippo_wire : std_logic;
--  signal pippo : std_logic;


--  signal LS_peripheral_counter        : integer;

  signal store_op_buf_wire                 : std_logic_vector(2 downto 0);   --serve per evitare che all'ultimo non venga effettuata la singola store
  signal store_op_buf                 : std_logic_vector(2 downto 0);   --serve per evitare che all'ultimo non venga effettuata la singola store
  signal store_valid                  : std_logic;

  signal load_op_buf                  : std_logic_vector(2 downto 0);   --serve per evitare che all'ultimo non venga effettuata la singola load
--  signal load_op_buf_lat              : std_logic_vector(2 downto 0);   --serve per evitare che all'ultimo non venga effettuata la singola load
--  signal ls_instr_req_buf             : std_logic_vector(2 downto 0);   --serve per evitare che all'ultimo non venga effettuata la singola LS
--  signal LS_peripheral                : std_logic; 
  signal harc_loss                    : integer;
  signal load_valid                   : std_logic;

  signal data_width_ID_buf_wire            : array_2D(harc_range)(1 downto 0);
  signal data_width_ID_buf            : array_2D(harc_range)(1 downto 0);
  signal data_width_ID_voted_wire          : std_logic_vector(1 downto 0);
  signal data_width_ID_voted          : std_logic_vector(1 downto 0);
  signal decoded_instruction_LS_buf_wire   : array_2D(harc_range)(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instruction_LS_buf   : array_2D(harc_range)(LS_UNIT_INSTR_SET_SIZE-1 downto 0);
  signal decoded_instruction_LS_voted_wire : std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0); -- wire per mantenere il risultato del voting
  signal decoded_instruction_LS_voted : std_logic_vector(LS_UNIT_INSTR_SET_SIZE-1 downto 0); -- wire per mantenere il risultato del voting
  signal bits_decoded_LS_fault        : integer;

  signal data_rvalid_i_lat : std_logic;


  -- signals for LSU Comb
  signal data_addr_internal_wires         : std_logic_vector (31 downto 0);
  signal data_wdata_o_wires               : std_logic_vector (31 downto 0);
  signal data_be_internal_wires           : std_logic_vector (3 downto 0);
  signal data_we_o_wires                  : std_logic;
  signal data_req_o_wires                 : std_logic;
  signal ls_except_condition_wires        : std_logic;
  signal ls_taken_branch_wires            : std_logic;
  signal core_busy_LS_wires               : std_logic;
  signal busy_LS_wires                    : std_logic; 
  signal ls_except_condition_internal     : std_logic;
  signal ls_taken_branch_internal         : std_logic;
  signal core_busy_LS_internal            : std_logic;
  signal busy_LS_internal                 : std_logic;

  signal nextstate_LS : fsm_LS_states;
  -- Memory fault signals
  signal data_addr_internal_lat     : std_logic_vector(31 downto 0);
  signal load_err                   : std_logic;
  signal store_err                  : std_logic;
  signal amo_store_lat              : std_logic;
  signal overflow_rs1_sc            : std_logic_vector(Addr_Width downto 0);
  signal overflow_rd_sc             : std_logic_vector(Addr_Width downto 0);
  signal busy_LS_lat                : std_logic;
  signal sc_word_count              : integer;
  signal harc_LS                    : accl_range;
  signal harc_LOAD                  : harc_range;
  signal ls_rs1_to_sc               : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal ls_rd_to_sc                : std_logic_vector(SPM_ADDR_WID-1 downto 0);
  signal ls_sc_data_write           : std_logic_vector(Data_Width-1 downto 0);
  signal data_be_internal           : std_logic_vector(3 downto 0);
  signal RS1_Data_IE_wire_lat       : std_logic_vector(31 downto 0);  -- Wire to used to directly send the result of the address increment
  signal RS2_Data_IE_wire_lat       : std_logic_vector(31 downto 0);  -- Wire to used to directly send the result of the address increment
  signal RD_Data_IE_wire_lat        : std_logic_vector(31 downto 0);  -- Wire to used to directly send the result of the address increment
  signal RS1_Data_IE_lat            : std_logic_vector(31 downto 0);  -- Used to preserve the old data in case we start executing in parallel
  signal RS2_Data_IE_lat            : std_logic_vector(31 downto 0);  -- Used to preserve the old data in case we start executing in parallel
  signal RD_Data_IE_lat             : std_logic_vector(31 downto 0);  -- Used to preserve the old data in case we start executing in parallel

  signal add_op_A                   : std_logic_vector(31 downto 0);
  signal add_op_B                   : std_logic_vector(31 downto 0);
  signal add_out                    : std_logic_vector(31 downto 0);

  signal flush_hart_int_wire        : std_logic_vector(harc_range);
  signal flush_hart_int             : std_logic_vector(harc_range);


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

  -- Memory fault signals
  load_err  <= data_gnt_i and data_err_i and not(data_we_o);
  store_err <= data_gnt_i and data_err_i and data_we_o;

  -- Memory address signal
  data_addr_o <= data_addr_internal(31 downto 2) & "00";
  data_be_o <= to_stdlogicvector(to_bitvector(data_be_internal) sll
                                 to_integer(unsigned(data_addr_internal(1 downto 0))));

------------------------------------------------------------------------
--  ██╗     ███████╗██╗   ██╗    ███████╗██╗   ██╗███╗   ██╗ ██████╗  --
--  ██║     ██╔════╝██║   ██║    ██╔════╝╚██╗ ██╔╝████╗  ██║██╔════╝  --
--  ██║     ███████╗██║   ██║    ███████╗ ╚████╔╝ ██╔██╗ ██║██║       --
--  ██║     ╚════██║██║   ██║    ╚════██║  ╚██╔╝  ██║╚██╗██║██║       --
--  ███████╗███████║╚██████╔╝    ███████║   ██║   ██║ ╚████║╚██████╗  --
--  ╚══════╝╚══════╝ ╚═════╝     ╚══════╝   ╚═╝   ╚═╝  ╚═══╝ ╚═════╝  --
------------------------------------------------------------------------

  LSU_sync : process(clk_i, rst_ni)
  begin
	  
    if rst_ni = '0' then
	    amo_store  <= '0';
	    amo_store_lat  <= '0';
	    LS_WB_EN <= '0';
	    busy_LS_lat <= '0';
	    LS_WB <= (others => '0');
      harc_LS_WB <= THREAD_POOL_SIZE-1;
      misaligned_err <= '0';
      instr_word_LS_WB <= (others => '0');
      if accl_en = 1 then
        ls_rs1_to_sc             <= (others => '0');
        ls_rd_to_sc              <= (others => '0');
      end if;
      ls_except_condition_internal <= '0';
      ls_taken_branch_internal     <= '0';
      core_busy_LS_internal        <= '0';
      busy_LS_internal             <= '0';     

    elsif rising_edge(clk_i) then
      amo_store  <= '0';
      misaligned_err <= '0';
      LS_WB       <= (others => '0');
      busy_LS_lat <= busy_LS or core_busy_LS;
      LS_WB_EN    <= LS_WB_EN_wire;
      if accl_en = 1 then
        RS1_Data_IE_lat <= RS1_Data_IE_wire_lat;
        RS2_Data_IE_lat <= RS2_Data_IE_wire_lat;
        RD_Data_IE_lat  <= RD_Data_IE_wire_lat;
      end if;
      ls_except_condition_internal <= ls_except_condition_wires;
      ls_taken_branch_internal     <= ls_taken_branch_wires    ;
      core_busy_LS_internal        <= core_busy_LS_wires       ;
      busy_LS_internal             <= busy_LS_wires            ;     

      -- don't evaluate any LSU state in case you don't have any LSU operation or in case you in a restore phase
      if ( ls_instr_req = '0'  and busy_LS_lat = '0' and state_LS /= voting ) or ( restore_fault_PC = '1' and harc_EXEC /= 0 ) then
      --   LS_WB_EN <= '0';
      -- if you are processing a LSU instruction 
      elsif ls_instr_req = '1' or add_vect_bits(ls_instr_req_buf_wire) > 0 or busy_LS_lat = '1' then

        if data_rvalid_i = '1' then
          ls_sc_data_write <= data_rdata_i;
        end if;
      --  LS_WB_EN <= '0';
        case state_LS is	
          when normal =>
          --------------------------------------------------------------
          --  ██╗      ██████╗  █████╗ ██████╗      ██████╗ ██████╗   --
          --  ██║     ██╔═══██╗██╔══██╗██╔══██╗    ██╔═══██╗██╔══██╗  --
          --  ██║     ██║   ██║███████║██║  ██║    ██║   ██║██████╔╝  --
          --  ██║     ██║   ██║██╔══██║██║  ██║    ██║   ██║██╔═══╝   --
          --  ███████╗╚██████╔╝██║  ██║██████╔╝    ╚██████╔╝██║       --
          --  ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═════╝      ╚═════╝ ╚═╝       --
          --------------------------------------------------------------
          -- load_op_buf_wire maintain load instruction operation in case of faults
            if load_op = '1' or add_vect_bits(load_op_buf_wire) > 1 then  -- Load Instructions
              if ((data_addr_internal(1 downto 0) = "00" and data_width_ID = "10") or 
                  (data_addr_internal(0)          = '0'  and data_width_ID = "01") or
                                                             data_width_ID = "00") then
                if load_err = '1' then
                  ls_except_data <= LOAD_ERROR_EXCEPT_CODE;
                end if;
                harc_LS_WB <= harc_EXEC;
                instr_word_LS_WB <= instr_word_IE;
              else
                ls_except_data <= LOAD_MISALIGNED_EXCEPT_CODE;
                misaligned_err <= '1';
              end if;
            end if;
          -----------------------------------------------------------------------
          --  ███████╗████████╗ ██████╗ ██████╗ ███████╗     ██████╗ ██████╗   --
          --  ██╔════╝╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝    ██╔═══██╗██╔══██╗  --
          --  ███████╗   ██║   ██║   ██║██████╔╝█████╗      ██║   ██║██████╔╝  --
          --  ╚════██║   ██║   ██║   ██║██╔══██╗██╔══╝      ██║   ██║██╔═══╝   --
          --  ███████║   ██║   ╚██████╔╝██║  ██║███████╗    ╚██████╔╝██║       --
          --  ╚══════╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚═════╝ ╚═╝       --
          -----------------------------------------------------------------------
          -- store_op_buf_wire maintain store instruction operation in case of faults
            if store_op = '1' or add_vect_bits(store_op_buf_wire) > 1 then  
              if ((data_addr_internal(1 downto 0) = "00" and data_width_ID = "10") or 
                  (data_addr_internal(0)          = '0'  and data_width_ID = "01") or
                                                             data_width_ID = "00") then
                RS2_Data_IE_lat <= RS2_Data_IE; 
                if (store_err = '1') then
                  ls_except_data <= STORE_ERROR_EXCEPT_CODE;
                end if;
                harc_LS_WB <= harc_EXEC;
              else
                ls_except_data <= STORE_MISALIGNED_EXCEPT_CODE;
                misaligned_err <= '1';
              end if;
            end if;
            if amo_store = '1' or amo_load_skip = '1' then
              amo_store_lat <= amo_store;
              amo_store <= '0';
            end if;
          ------------------------------------------------------------------------------------------------------
          --  ██████╗ ██╗   ██╗██████╗ ███████╗████████╗    ██╗     ██████╗     ██╗███████╗████████╗██████╗   --
          --  ██╔══██╗██║   ██║██╔══██╗██╔════╝╚══██╔══╝    ██║     ██╔══██╗   ██╔╝██╔════╝╚══██╔══╝██╔══██╗  --
          --  ██████╔╝██║   ██║██████╔╝███████╗   ██║       ██║     ██║  ██║  ██╔╝ ███████╗   ██║   ██████╔╝  --
          --  ██╔══██╗██║   ██║██╔══██╗╚════██║   ██║       ██║     ██║  ██║ ██╔╝  ╚════██║   ██║   ██╔══██╗  --
          --  ██████╔╝╚██████╔╝██║  ██║███████║   ██║       ███████╗██████╔╝██╔╝   ███████║   ██║   ██║  ██║  --
          --  ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚══════╝   ╚═╝       ╚══════╝╚═════╝ ╚═╝    ╚══════╝   ╚═╝   ╚═╝  ╚═╝  --
          ------------------------------------------------------------------------------------------------------

            if accl_en = 1 then
              if decoded_instruction_LS(KMEMLD_bit_position)   = '1' or
                 decoded_instruction_LS(KBCASTLD_bit_position) = '1' then
                overflow_rd_sc <= add_out(Addr_Width downto 0); -- If storing data to SC overflows it's address space
                -- Illegal byte transfer handler, and illegal writeback address handler
                if rd_to_sc = "100" then --  AAA change "100" to make it parametrizable -- Not a scratchpad destination address
                  ls_except_data              <= ILLEGAL_ADDRESS_EXCEPT_CODE;
                elsif RS1_Data_IE(1 downto 0) /= "00" then
                  ls_except_data              <= LOAD_MISALIGNED_EXCEPT_CODE;
                  misaligned_err              <= '1';
                elsif load_err = '1' then  -- AAA move to data_valid_waiting stage
                  ls_except_data                <= LOAD_ERROR_EXCEPT_CODE;
                else
                  RS1_Data_IE_lat <= RS1_Data_IE;
                  RS2_Data_IE_lat <= RS2_Data_IE;
                  RD_Data_IE_lat  <= RD_Data_IE;
                  ls_rd_to_sc <= rd_to_sc;
                end if;
              end if;

              if decoded_instruction_LS(KMEMSTR_bit_position) = '1' then
                overflow_rs1_sc <= add_out(Addr_Width downto 0); -- If loading data from SC overflows it's address space
                -- Illegal byte transfer handler, and illegal writeback address handler
                if rs1_to_sc = "100" then                              --  Not a scratchpad source address
                  ls_except_data              <= ILLEGAL_ADDRESS_EXCEPT_CODE;
                elsif RD_Data_IE(1 downto 0) /= "00" then
                  ls_except_data              <= STORE_MISALIGNED_EXCEPT_CODE;
                  misaligned_err              <= '1';  
                elsif store_err = '1' then
                  ls_except_data              <= STORE_ERROR_EXCEPT_CODE;	
                else
                  RS1_Data_IE_lat <= RS1_Data_IE;
                  RS2_Data_IE_lat <= RS2_Data_IE;
                  RD_Data_IE_lat <= RD_Data_IE;
                  ls_rs1_to_sc <= rs1_to_sc;
                end if;
              end if;
            end if;
			  

          ----------------------------------------------------------------------------------------------------
          --  ██╗   ██╗ █████╗ ██╗     ██╗██████╗     ██╗    ██╗ █████╗ ██╗████████╗██╗███╗   ██╗ ██████╗   --
          --  ██║   ██║██╔══██╗██║     ██║██╔══██╗    ██║    ██║██╔══██╗██║╚══██╔══╝██║████╗  ██║██╔════╝   --
          --  ██║   ██║███████║██║     ██║██║  ██║    ██║ █╗ ██║███████║██║   ██║   ██║██╔██╗ ██║██║  ███╗  --
          --  ╚██╗ ██╔╝██╔══██║██║     ██║██║  ██║    ██║███╗██║██╔══██║██║   ██║   ██║██║╚██╗██║██║   ██║  --
          --   ╚████╔╝ ██║  ██║███████╗██║██████╔╝    ╚███╔███╔╝██║  ██║██║   ██║   ██║██║ ╚████║╚██████╔╝  --
          --    ╚═══╝  ╚═╝  ╚═╝╚══════╝╚═╝╚═════╝      ╚══╝╚══╝ ╚═╝  ╚═╝╚═╝   ╚═╝   ╚═╝╚═╝  ╚═══╝ ╚═════╝   --
          ----------------------------------------------------------------------------------------------------
          -- when data_valid_waiting, you normally wait for a valid data from data_rvalid_i. During thread 2 operations, this value is substituted by load_valid or store_valid signal 
          when data_valid_waiting =>

            if amo_store_lat = '1' or amo_load_skip = '1' then
              if data_rvalid_i = '1'  or load_valid = '1' or store_valid = '1' then
                amo_store_lat <= '0';
              end if;
            end if; 

            if accl_en = 1 then
              if decoded_instruction_LS_voted(KMEMLD_bit_position) = '1' or decoded_instruction_LS_voted(KBCASTLD_bit_position) = '1' then
                if overflow_rd_sc(Addr_Width) = '1' then
                  ls_except_data              <= SCRATCHPAD_OVERFLOW_EXCEPT_CODE;
                end if;
              end if;

              if decoded_instruction_LS_voted(KMEMSTR_bit_position) = '1' then
                if overflow_rs1_sc(Addr_Width) = '1' then
                  ls_except_data              <= SCRATCHPAD_OVERFLOW_EXCEPT_CODE;     
                end if;
              end if;
            end if;

            if decoded_instruction_LS_voted(LW_bit_position) = '1'  or (decoded_instruction_LS_voted(AMOSWAP_bit_position) = '1' and amo_store_lat = '0' and amo_load_skip = '0') then
              if data_rvalid_i = '1'  or load_valid = '1' or store_valid = '1' then
                LS_WB <= data_rdata_i;
                --LS_WB_EN <= '1';
                if decoded_instruction_LS_voted(AMOSWAP_bit_position) = '1' then
                  amo_store <= '1';
                end if;
              end if;
            end if;

            if decoded_instruction_LS_voted(LH_bit_position) = '1' or decoded_instruction_LS_voted(LHU_bit_position) = '1' then 
              if data_rvalid_i = '1'  or load_valid = '1' or store_valid = '1' then
                case data_addr_internal(1) is
                  when '0' =>
                  --LS_WB_EN <= '1';
                    if decoded_instruction_LS_voted(LH_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(signed(data_rdata_i(15 downto 0)), 32));
                    elsif decoded_instruction_LS_voted(LHU_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(unsigned(data_rdata_i(15 downto 0)), 32));
                    end if;
                  when '1' =>
                    --LS_WB_EN <= '1';
                    if decoded_instruction_LS_voted(LH_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(signed(data_rdata_i(31 downto 16)), 32));
                    elsif decoded_instruction_LS_voted(LHU_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(unsigned(data_rdata_i(31 downto 16)), 32));
                    end if;
                  when others =>
                    null;
                end case;
              end if;
            end if;

            if decoded_instruction_LS_voted(LB_bit_position) = '1' or decoded_instruction_LS_voted(LBU_bit_position) = '1' then 
              if data_rvalid_i = '1' or load_valid = '1' or store_valid = '1' then    
                --LS_WB_EN <= '1';
                case data_addr_internal(1 downto 0) is
                  when "00" =>
                    if decoded_instruction_LS_voted(LB_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(signed(data_rdata_i(7 downto 0)), 32));
                    elsif decoded_instruction_LS_voted(LBU_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(unsigned(data_rdata_i(7 downto 0)), 32));
                    end if;
                  when "01" =>
                    if decoded_instruction_LS_voted(LB_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(signed(data_rdata_i(15 downto 8)), 32));
                    elsif decoded_instruction_LS_voted(LBU_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(unsigned(data_rdata_i(15 downto 8)), 32));           
                    end if;
                  when "10" =>
                    if decoded_instruction_LS_voted(LB_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(signed(data_rdata_i(23 downto 16)), 32));
                    elsif decoded_instruction_LS_voted(LBU_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(unsigned(data_rdata_i(23 downto 16)), 32));
                    end if;
                  when "11" =>
                    if decoded_instruction_LS_voted(LB_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(signed(data_rdata_i(31 downto 24)), 32));
                    elsif decoded_instruction_LS_voted(LBU_bit_position) = '1' then
                      LS_WB <= std_logic_vector(resize(unsigned(data_rdata_i(31 downto 24)), 32));
                    end if;
                  when others =>
                    null;               
                end case;
              end if;
            end if;

          -- Since the voting operation is purely combinational, nothing is required by the synch LSU state  
          when voting =>

        end case;
      end if;  
    end if;


  end process;

  spm_bcast <= '1' when  decoded_instruction_LS_voted(KBCASTLD_bit_position) = '1' else '0';


-------------------------------------------------------------------------
--  ██╗     ███████╗██╗   ██╗     ██████╗ ██████╗ ███╗   ███╗██████╗   --
--  ██║     ██╔════╝██║   ██║    ██╔════╝██╔═══██╗████╗ ████║██╔══██╗  --
--  ██║     ███████╗██║   ██║    ██║     ██║   ██║██╔████╔██║██████╔╝  --
--  ██║     ╚════██║██║   ██║    ██║     ██║   ██║██║╚██╔╝██║██╔══██╗  --
--  ███████╗███████║╚██████╔╝    ╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝  --
--  ╚══════╝╚══════╝ ╚═════╝      ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝   --
-------------------------------------------------------------------------

  LSU_comb : process(all)

  begin
    -- dft1m
    bits_decoded_LS_fault <= add_vect_bits(decoded_instruction_LS);

    data_addr_internal_wires         <= std_logic_vector(signed(RS1_Data_IE));  -- The reset value was non-zero in order to keep the switching activity minimal
    nextstate_LS <=  normal;

    data_be_internal_wires           <= (others => '0');
    data_wdata_o_wires               <= (others => '0');
    data_we_o_wires                  <= '0';
    data_req_o_wires                 <= '0';
    ls_except_condition_wires        <= '0';
    ls_taken_branch_wires            <= '0';
    core_busy_LS_wires               <= '0';
    busy_LS_wires                    <= '0';

    restore_fault_LSU_wire                     <= '0' when restore_fault = '1' else restore_fault_LSU;
    LS_is_running_wire                         <= LS_is_running;
--    LS_is_running_wire                         <= '0';
--    LS_WB_EN_wire                    <= LS_WB_EN;

    decoded_instruction_LS_voted_wire <= decoded_instruction_LS_voted;
    data_width_ID_voted_wire          <= data_width_ID_voted;

    if rst_ni = '0' then
      data_width_ID_voted_wire <= (others => '0');
      decoded_instruction_LS_voted_wire  <= (others => '0');
    end if;

    if accl_en = 1 then
      ls_sc_data_write_wire            <= ls_sc_data_write;
      sc_word_count_wire               <= sc_word_count;
      kmemld_inflight                  <= (others => '0');
      kmemstr_inflight                 <= (others => '0');
      ls_sc_write_addr                 <= (others => '0');
	    ls_sc_read_addr                  <= (others => '0');
      --halt_lsu                         <= '0';
      RS1_Data_IE_wire_lat             <= RS1_Data_IE_lat;
      RS2_Data_IE_wire_lat             <= RS2_Data_IE_lat;
      RD_Data_IE_wire_lat              <= RD_Data_IE_lat;
      harc_LS_wire                     <= harc_LS;
      ls_sci_req <= (others => '0');
      ls_sci_we  <= (others => '0');
    end if;

    -- don't evaluate any LSU state in case you don't have any LSU operation or in case you in a restore phase
    if ( ls_instr_req = '0' and busy_LS_lat = '0' and state_LS /= voting ) or ( restore_fault_PC = '1' and harc_EXEC /= 0 ) then
      LS_WB_EN_wire <= '0';
--    elsif LS_instr_req = '1' or busy_LS_lat = '1' then
--    elsif ( add_vect_bits(ls_instr_req_buf_wire) > 0 or busy_LS_lat = '1' )  then 


      data_addr_internal         <= ( others => '0');
      data_wdata_o               <= ( others => '0');
      data_be_internal           <= ( others => '0');
      data_we_o                  <= '0';
      data_req_o                 <= '0';
      ls_except_condition        <= ls_except_condition_wires;
      ls_taken_branch            <= ls_taken_branch_wires;
      core_busy_LS               <= core_busy_LS_wires;
      busy_LS                    <= busy_LS_wires;

      -- if a fault avoid the voting stage, you have to force it to produce the restore state 
      nextstate_LS <= voting when ( harc_EXEC = 1  and state_LS = normal and ls_instr_req_buf = "100" and restore_fault_PC = '0' and LS_is_running = '0' ) else normal ;



--    data_addr_internal         <= data_addr_internal_buf(harc_EXEC);

----          data_addr_internal         <= data_addr_internal_wires;
--          data_wdata_o               <= data_wdata_o_wires;
--          data_be_internal           <= data_be_internal_wires;
--          data_we_o                  <= data_we_o_wires;
--          data_req_o                 <= data_req_o_wires;
--          ls_except_condition        <= ls_except_condition_wires;
--          ls_taken_branch            <= ls_taken_branch_wires;
--          core_busy_LS               <= core_busy_LS_wires;
--          busy_LS                    <= busy_LS_wires;

--      data_width_ID_voted_wire <= data_width_ID; --data_width_ID_buf_wire(2);-- or data_width_ID_buf_wire(1) or data_width_ID_buf_wire(0);--(data_width_ID_buf_wire(2) and data_width_ID_buf_wire(1) ) or ( data_width_ID_buf_wire(2) and data_width_ID_buf_wire(0) ) or ( data_width_ID_buf_wire(1) and data_width_ID_buf_wire(0) );
--      decoded_instruction_LS_voted_wire  <= decoded_instruction_LS; --decoded_instruction_LS_buf_wire(2);-- or decoded_instruction_LS_buf_wire(1) or decoded_instruction_LS_buf_wire(0);--(decoded_instruction_LS_buf_wire(2) and decoded_instruction_LS_buf_wire(1) ) or ( decoded_instruction_LS_buf_wire(2) and decoded_instruction_LS_buf_wire(0) ) or ( decoded_instruction_LS_buf_wire(1) and decoded_instruction_LS_buf_wire(0) );               

    -- if you are processing a LSU instruction 
    elsif ( ls_instr_req = '1' or add_vect_bits(ls_instr_req_buf_wire ) > 0 or add_vect_bits(ls_instr_req_buf ) > 0 or busy_LS_lat = '1' )  then 
      LS_WB_EN_wire <= '0';

      case state_LS is
        -- when normal, you are in the default state. If any load or store operation signal is received, you have to perform the LS operation
        when normal =>

          -- as a default state force voting stage when you are on thread 0 or 1
          nextstate_LS <= voting when ( harc_EXEC = 1 or harc_EXEC = 0) and LS_is_running = '0' else normal;

          --------------------------------------------------------------
          --  ██╗      ██████╗  █████╗ ██████╗      ██████╗ ██████╗   --
          --  ██║     ██╔═══██╗██╔══██╗██╔══██╗    ██╔═══██╗██╔══██╗  --
          --  ██║     ██║   ██║███████║██║  ██║    ██║   ██║██████╔╝  --
          --  ██║     ██║   ██║██╔══██║██║  ██║    ██║   ██║██╔═══╝   --
          --  ███████╗╚██████╔╝██║  ██║██████╔╝    ╚██████╔╝██║       --
          --  ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═════╝      ╚═════╝ ╚═╝       --
          --------------------------------------------------------------
          -- load_op_buf_wire maintain load instruction operation in case of faults
          if load_op = '1' or add_vect_bits(load_op_buf_wire) > 1 then
            if amo_load = '0' then
              data_addr_internal_wires <= add_out;
            else
              data_addr_internal_wires <= RS1_Data_IE;
            end if;
            if ((data_addr_internal(1 downto 0) = "00" and data_width_ID = "10") or 
                (data_addr_internal(0)          = '0'  and data_width_ID = "01") or
                                                                 data_width_ID = "00") then
              data_be_internal_wires <= data_be_ID;
              data_req_o_wires       <= '1';
              if load_err = '1' then
                ls_except_condition_wires <= '1';
                ls_taken_branch_wires     <= '1';
              else
                core_busy_LS_wires <= '1';
--                  nextstate_LS       <= data_valid_waiting;
                nextstate_LS <= voting when ( harc_EXEC = 1 or harc_EXEC = 0) else data_valid_waiting; --force voting stage when you are on thread 0 or 1

              end if;
            else
              ls_except_condition_wires <= '1';
              ls_taken_branch_wires     <= '1';
            end if;
          end if;

          -----------------------------------------------------------------------
          --  ███████╗████████╗ ██████╗ ██████╗ ███████╗     ██████╗ ██████╗   --
          --  ██╔════╝╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝    ██╔═══██╗██╔══██╗  --
          --  ███████╗   ██║   ██║   ██║██████╔╝█████╗      ██║   ██║██████╔╝  --
          --  ╚════██║   ██║   ██║   ██║██╔══██╗██╔══╝      ██║   ██║██╔═══╝   --
          --  ███████║   ██║   ╚██████╔╝██║  ██║███████╗    ╚██████╔╝██║       --
          --  ╚══════╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚═════╝ ╚═╝       --
          -----------------------------------------------------------------------
          -- store_op_buf_wire maintain store instruction operation in case of faults
          if store_op = '1' or add_vect_bits(store_op_buf_wire) > 1 then
            if amo_store = '0' and amo_load_skip = '0'  then
              data_addr_internal_wires <= add_out;
            elsif amo_store = '1' or amo_load_skip = '1' then
              data_addr_internal_wires <= RS1_Data_IE;
            end if;
            data_we_o_wires <= '1';
            if ((data_addr_internal(1 downto 0) = "00" and data_width_ID = "10") or 
                (data_addr_internal(0)          = '0'  and data_width_ID = "01") or
                                                                 data_width_ID = "00") then
              data_req_o_wires       <= '1';
              data_be_internal_wires <= data_be_ID;
              data_wdata_o_wires <= RS2_Data_IE;    
              if store_err = '1' then
                ls_except_condition_wires  <= '1';
                ls_taken_branch_wires      <= '1';
              else
--                  nextstate_LS       <= data_valid_waiting;
                nextstate_LS <= voting when ( harc_EXEC = 1 or harc_EXEC = 0)  else data_valid_waiting; --force voting stage when you are on thread 0 or 1
                busy_LS_wires <= '1';
                if amo_store = '1' or amo_load_skip = '1' then
                  core_busy_LS_wires <= '1';
                end if;
              end if;
            else
              ls_except_condition_wires  <= '1';
              ls_taken_branch_wires      <= '1';
            end if;
            if data_width_ID = "01" then  -- store half word
              case data_addr_internal(1) is
                when '0' =>
                  data_wdata_o_wires <= RS2_Data_IE(31 downto 0);
                  data_we_o_wires        <= '1';  -- is a writing
                  data_be_internal_wires <= data_be_ID;
                when '1' =>
                  data_wdata_o_wires <= RS2_Data_IE(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                  data_we_o_wires        <= '1';  -- is a writing
                  data_be_internal_wires <= data_be_ID;
                when others =>
                  null;
              end case;
            end if;
            if data_width_ID = "00" then  -- store byte
              case data_addr_internal(1 downto 0) is
                when "00" =>
                  data_wdata_o_wires <= RS2_Data_IE(31 downto 0);
                  data_we_o_wires        <= '1';  -- is a writing
                  data_be_internal_wires <= data_be_ID;
                when "01" =>
                  data_wdata_o_wires <= RS2_Data_IE(23 downto 0) & std_logic_vector(to_unsigned(0, 8));
                  data_we_o_wires        <= '1';  -- is a writing
                  data_be_internal_wires <= data_be_ID;
                when "10" =>
                  data_wdata_o_wires <= RS2_Data_IE(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                  data_we_o_wires        <= '1';  -- is a writing
                  data_be_internal_wires <= data_be_ID;
                when "11" =>
                  data_wdata_o_wires <= RS2_Data_IE(7 downto 0) & std_logic_vector(to_unsigned(0, 24));
                  data_we_o_wires        <= '1';  -- is a writing
                  data_be_internal_wires <= data_be_ID;
                when others =>
                  null;
              end case;
            end if;
          end if;
          ------------------------------------------------------------------------------------------------------
          --  ██████╗ ██╗   ██╗██████╗ ███████╗████████╗    ██╗     ██████╗     ██╗███████╗████████╗██████╗   --
          --  ██╔══██╗██║   ██║██╔══██╗██╔════╝╚══██╔══╝    ██║     ██╔══██╗   ██╔╝██╔════╝╚══██╔══╝██╔══██╗  --
          --  ██████╔╝██║   ██║██████╔╝███████╗   ██║       ██║     ██║  ██║  ██╔╝ ███████╗   ██║   ██████╔╝  --
          --  ██╔══██╗██║   ██║██╔══██╗╚════██║   ██║       ██║     ██║  ██║ ██╔╝  ╚════██║   ██║   ██╔══██╗  --
          --  ██████╔╝╚██████╔╝██║  ██║███████║   ██║       ███████╗██████╔╝██╔╝   ███████║   ██║   ██║  ██║  --
          --  ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚══════╝   ╚═╝       ╚══════╝╚═════╝ ╚═╝    ╚══════╝   ╚═╝   ╚═╝  ╚═╝  --
          ------------------------------------------------------------------------------------------------------

            if accl_en = 1 then
              if decoded_instruction_LS(KMEMLD_bit_position)   = '1' or
                 decoded_instruction_LS(KBCASTLD_bit_position) = '1' then
                -- RS2_Data_IE(Addr_Width downto 0) instead of RS2_Data_IE(Addr_Width -1 downto 0) in order to allow reading sizes = MAX_SC_SIZE and not MAX_SC_SIZE - 1 
                if rd_to_sc = "100" then -- AAA change this to support more than 4 spms
                  ls_except_condition_wires  <= '1';
                  ls_taken_branch_wires      <= '1';
                elsif(RS1_Data_IE(1 downto 0) /= "00") then
                  ls_except_condition_wires  <= '1';
                  ls_taken_branch_wires      <= '1';
                elsif load_err = '1' then
                  ls_except_condition_wires  <= '1';
                  ls_taken_branch_wires      <= '1';
                else
--                  nextstate_LS       <= data_valid_waiting;
                nextstate_LS <= voting when ( harc_EXEC = 1 or harc_EXEC = 0) else data_valid_waiting; --force voting stage when you are on thread 0 or 1
                  core_busy_LS_wires <= '1'; -- set the core_busy_LS to '1' untill we are sure there are no exceptions.
                  busy_LS_wires      <= '1';
                  sc_word_count_wire <= to_integer(unsigned(RD_Data_IE(SIMD_BITS+1 downto 1))/2);
                  kmemld_inflight(to_integer(unsigned(rd_to_sc))) <= '1';
                  if replicate_accl_en=1 then
                    harc_LS_wire <= harc_EXEC;
                  elsif replicate_accl_en=0 then
                    harc_LS_wire <= 0;
                  end if;
                end if;
              end if;

              if decoded_instruction_LS(KMEMSTR_bit_position) = '1' then
                -- RS2_Data_IE(Addr_Width downto 0) instead of RS2_Data_IE(Addr_Width -1 downto 0) in order to allow reading sizes = MAX_SC_SIZE and not MAX_SC_SIZE - 1 
                if rs1_to_sc = "100" then
                  ls_except_condition_wires  <= '1';
                  ls_taken_branch_wires      <= '1';
                elsif(RD_Data_IE(1 downto 0) /= "00") then
                  ls_except_condition_wires  <= '1';
                  ls_taken_branch_wires      <= '1';
                elsif store_err = '1' then
                  ls_except_condition_wires  <= '1';
                  ls_taken_branch_wires      <= '1';
                else
--                  nextstate_LS       <= data_valid_waiting;
                  nextstate_LS <= voting when ( harc_EXEC = 1 or harc_EXEC = 0) else data_valid_waiting; --force voting stage when you are on thread 0 or 1
                  core_busy_LS_wires <= '1'; -- set the core_busy_LS to '1' untill we are sure there are no exceptions.
                  busy_LS_wires      <= '1';
                  ls_sci_req(to_integer(unsigned(rs1_to_sc))) <= '1';
                  ls_sc_read_addr <= RS1_Data_IE(Addr_Width - 1 downto SIMD_BITS+2);
                  sc_word_count_wire <= to_integer(unsigned(RS1_Data_IE(SIMD_BITS+1 downto 1))/2);
                  kmemstr_inflight(to_integer(unsigned(rs1_to_sc))) <= '1';
                  if replicate_accl_en=1 then
                    harc_LS_wire    <= harc_EXEC;
                  elsif replicate_accl_en=0 then
                    harc_LS_wire <= 0;
                  end if;
                end if;
              end if;
          end if;

          data_addr_internal         <= ( others => '0');
          data_wdata_o               <= ( others => '0');
          data_be_internal           <= ( others => '0');
          data_we_o                  <= '0';
          data_req_o                 <= '0';
          ls_except_condition        <= ls_except_condition_wires;
          ls_taken_branch            <= ls_taken_branch_wires;
          core_busy_LS               <= core_busy_LS_wires;
          busy_LS                    <= busy_LS_wires;

          data_width_ID_voted_wire <= data_width_ID; --data_width_ID_buf_wire(2);-- or data_width_ID_buf_wire(1) or data_width_ID_buf_wire(0);--(data_width_ID_buf_wire(2) and data_width_ID_buf_wire(1) ) or ( data_width_ID_buf_wire(2) and data_width_ID_buf_wire(0) ) or ( data_width_ID_buf_wire(1) and data_width_ID_buf_wire(0) );
          decoded_instruction_LS_voted_wire  <= decoded_instruction_LS; --decoded_instruction_LS_buf_wire(2);-- or decoded_instruction_LS_buf_wire(1) or decoded_instruction_LS_buf_wire(0);--(decoded_instruction_LS_buf_wire(2) and decoded_instruction_LS_buf_wire(1) ) or ( decoded_instruction_LS_buf_wire(2) and decoded_instruction_LS_buf_wire(0) ) or ( decoded_instruction_LS_buf_wire(1) and decoded_instruction_LS_buf_wire(0) );               


      --------------------------------------------------------
      -- ██╗   ██╗ ██████╗ ████████╗██╗███╗   ██╗ ██████╗   --
      -- ██║   ██║██╔═══██╗╚══██╔══╝██║████╗  ██║██╔════╝   --
      -- ██║   ██║██║   ██║   ██║   ██║██╔██╗ ██║██║  ███╗  --
      -- ╚██╗ ██╔╝██║   ██║   ██║   ██║██║╚██╗██║██║   ██║  --
      --  ╚████╔╝ ╚██████╔╝   ██║   ██║██║ ╚████║╚██████╔╝  --
      --   ╚═══╝   ╚═════╝    ╚═╝   ╚═╝╚═╝  ╚═══╝ ╚═════╝   --
      --------------------------------------------------------                                                 
      when voting =>  

        -- if thread is one and there is a store, send the voted value, else no
        -- LSU voting mechanism, this state appears only when you are on thread 1 or 0 
        if ( add_vect_bits(store_op_buf) >=1  or add_vect_bits(load_op_buf) >=1  ) and LS_is_running = '0' then --la parte amo non serve le amo non sono supportate

          -- when thread 0 is active, it means you are in a restore state and you have to send the LS operation
          if ( harc_EXEC = 0 or harc_loss = 0 ) then
            if restore_fault = '1' then
              restore_fault_LSU_wire <= '0';    
            end if;
            LS_is_running_wire <= '0';
            data_we_o                  <= data_we_o_buf(0);  --data_we_o_wires;
            data_req_o                 <= data_req_o_buf(0); --data_req_o_wires;

            data_addr_internal         <= data_addr_internal_buf(0);
            data_wdata_o               <= data_wdata_o_buf(0);
            data_be_internal           <= data_be_internal_buf(0);

            data_width_ID_voted_wire <= data_width_ID_buf(0);-- or data_width_ID_buf_wire(1) or data_width_ID_buf_wire(0);--(data_width_ID_buf_wire(2) and data_width_ID_buf_wire(1) ) or ( data_width_ID_buf_wire(2) and data_width_ID_buf_wire(0) ) or ( data_width_ID_buf_wire(1) and data_width_ID_buf_wire(0) );
            decoded_instruction_LS_voted_wire  <= decoded_instruction_LS_buf(0);-- or decoded_instruction_LS_buf_wire(1) or decoded_instruction_LS_buf_wire(0);--(decoded_instruction_LS_buf_wire(2) and decoded_instruction_LS_buf_wire(1) ) or ( decoded_instruction_LS_buf_wire(2) and decoded_instruction_LS_buf_wire(0) ) or ( decoded_instruction_LS_buf_wire(1) and decoded_instruction_LS_buf_wire(0) );               
            nextstate_LS <= data_valid_waiting;

          -- when you are on thread 1, vote among the buffers. 
          elsif restore_fault_PC = '0'  then --make the voting

              -- if one of the buffered value is different from the other, active the restore state
              if (((data_addr_internal_buf(2) /= data_addr_internal_buf(1) ) or ( data_wdata_o_buf(2) /= data_wdata_o_buf(1) ) or
                   (data_we_o_buf(2) /= data_we_o_buf(1) ) or (data_req_o_buf(2) /= data_req_o_buf(1) ) or
                   (data_be_internal_buf(2) /= data_be_internal_buf(1) ) or (data_width_ID_buf(2) /= data_width_ID_buf(1) ) or
                   (decoded_instruction_LS_buf(2) /= decoded_instruction_LS_buf(1))) and LS_WB_wrong = '0' ) then

                  data_we_o               <= '0';-- or data_we_o_buf(1) or data_we_o_buf(0);--(data_we_o_buf(2) and data_we_o_buf(1) ) or ( data_we_o_buf(2) and data_we_o_buf(0) ) or ( data_we_o_buf(1) and data_we_o_buf(0) );
                  data_req_o              <= '0';-- or data_req_o_buf(1) or data_req_o_buf(0);--(data_req_o_buf(2) and data_req_o_buf(1) ) or ( data_req_o_buf(2) and data_req_o_buf(0) ) or ( data_req_o_buf(1) and data_req_o_buf(0) );
                  restore_fault_LSU_wire  <= '1';    

                  nextstate_LS <= data_valid_waiting;
              -- if they are equal it means you are on thread 1 with no faults. Perform the LS operation
              elsif (( (data_addr_internal_buf(2) = data_addr_internal_buf(1) ) and ( data_wdata_o_buf(2) = data_wdata_o_buf(1) ) and
                     (data_we_o_buf(2) = data_we_o_buf(1) ) and (data_req_o_buf(2) = data_req_o_buf(1) ) and
                     (data_be_internal_buf(2) = data_be_internal_buf(1) ) and (data_width_ID_buf(2) = data_width_ID_buf(1) ) and
                     (decoded_instruction_LS_buf(2) = decoded_instruction_LS_buf(1)) )  ) and restore_fault_PC = '0' then --potrebbe andare bene anche restore_fault_pc SENZA WIRE

                  data_we_o                  <= data_we_o_buf(2); --data_we_o_wires;
                  data_req_o                 <= data_req_o_buf(2); --data_req_o_wires;
                  LS_is_running_wire         <= '1';

                  nextstate_LS <= data_valid_waiting;
              -- if they are equal but you are on a restore state, don't perform any LS operation since it will be performed by thread 0 in the following cycles    
              elsif (( (data_addr_internal_buf(2) = data_addr_internal_buf(1) ) and ( data_wdata_o_buf(2) = data_wdata_o_buf(1) ) and
                     (data_we_o_buf(2) = data_we_o_buf(1) ) and (data_req_o_buf(2) = data_req_o_buf(1) ) and
                     (data_be_internal_buf(2) = data_be_internal_buf(1) ) and (data_width_ID_buf(2) = data_width_ID_buf(1) ) and
                     (decoded_instruction_LS_buf(2) = decoded_instruction_LS_buf(1)) )  ) and restore_fault_PC = '1' then --potrebbe andare bene anche restore_fault_pc SENZA WIRE

                  data_we_o           <= '0';-- or data_we_o_buf_wire(1) or data_we_o_buf_wire(0);--(data_we_o_buf_wire(2) and data_we_o_buf_wire(1) ) or ( data_we_o_buf_wire(2) and data_we_o_buf_wire(0) ) or ( data_we_o_buf_wire(1) and data_we_o_buf_wire(0) );
                  data_req_o          <= '0';-- or data_req_o_buf_wire(1) or data_req_o_buf_wire(0);--(data_req_o_buf_wire(2) and data_req_o_buf_wire(1) ) or ( data_req_o_buf_wire(2) and data_req_o_buf_wire(0) ) or ( data_req_o_buf_wire(1) and data_req_o_buf_wire(0) );

                  nextstate_LS <= normal;
              -- in any other case, don't perfor any LS operation    
              else
                  data_we_o           <= '0';-- or data_we_o_buf_wire(1) or data_we_o_buf_wire(0);--(data_we_o_buf_wire(2) and data_we_o_buf_wire(1) ) or ( data_we_o_buf_wire(2) and data_we_o_buf_wire(0) ) or ( data_we_o_buf_wire(1) and data_we_o_buf_wire(0) );
                  data_req_o          <= '0';-- or data_req_o_buf_wire(1) or data_req_o_buf_wire(0);--(data_req_o_buf_wire(2) and data_req_o_buf_wire(1) ) or ( data_req_o_buf_wire(2) and data_req_o_buf_wire(0) ) or ( data_req_o_buf_wire(1) and data_req_o_buf_wire(0) );
                  nextstate_LS <= normal;
              end if;   

              data_addr_internal         <= data_addr_internal_buf(harc_EXEC);
              data_wdata_o               <= data_wdata_o_buf(harc_EXEC);
              data_be_internal           <= data_be_internal_buf(harc_EXEC);

              data_width_ID_voted_wire <= data_width_ID_buf(harc_EXEC);-- or data_width_ID_buf_wire(1) or data_width_ID_buf_wire(0);--(data_width_ID_buf_wire(harc_EXEC) and data_width_ID_buf_wire(1) ) or ( data_width_ID_buf_wire(harc_EXEC) and data_width_ID_buf_wire(0) ) or ( data_width_ID_buf_wire(1) and data_width_ID_buf_wire(0) );
              decoded_instruction_LS_voted_wire  <= decoded_instruction_LS_buf(harc_EXEC);-- or decoded_instruction_LS_buf_wire(1) or decoded_instruction_LS_buf_wire(0);--(decoded_instruction_LS_buf_wire(harc_EXEC) and decoded_instruction_LS_buf_wire(1) ) or ( decoded_instruction_LS_buf_wire(harc_EXEC) and decoded_instruction_LS_buf_wire(0) ) or ( decoded_instruction_LS_buf_wire(1) and decoded_instruction_LS_buf_wire(0) );               

          -- in any other case, maintain the previous state and don't perform any LS operation     
          else
            data_we_o                  <= '0';
            data_req_o                 <= '0';
            if restore_fault = '1' then
              restore_fault_LSU_wire <= '0';    
            end if;
            LS_is_running_wire <= '0';
        
            data_addr_internal         <= data_addr_internal_buf(harc_EXEC);
            data_wdata_o               <= data_wdata_o_buf(harc_EXEC);
            data_be_internal           <= data_be_internal_buf(harc_EXEC);

            data_width_ID_voted_wire <= data_width_ID_buf(harc_EXEC);-- or data_width_ID_buf_wire(1) or data_width_ID_buf_wire(0);--(data_width_ID_buf_wire(harc_EXEC) and data_width_ID_buf_wire(1) ) or ( data_width_ID_buf_wire(harc_EXEC) and data_width_ID_buf_wire(0) ) or ( data_width_ID_buf_wire(1) and data_width_ID_buf_wire(0) );
            decoded_instruction_LS_voted_wire  <= decoded_instruction_LS_buf(harc_EXEC);-- or decoded_instruction_LS_buf_wire(1) or decoded_instruction_LS_buf_wire(0);--(decoded_instruction_LS_buf_wire(harc_EXEC) and decoded_instruction_LS_buf_wire(1) ) or ( decoded_instruction_LS_buf_wire(harc_EXEC) and decoded_instruction_LS_buf_wire(0) ) or ( decoded_instruction_LS_buf_wire(1) and decoded_instruction_LS_buf_wire(0) );               

            nextstate_LS <= data_valid_waiting;

          end if;

        -- if you are in the voting stage without having any LS operation, don't perform any LS oepration, and activate the restore signal  
        else
            restore_fault_LSU_wire <= '1';        
          if restore_fault = '1' then
      --          LS_is_running_wire <= '0';
            restore_fault_LSU_wire <= '0';        
          end if;
          LS_is_running_wire <= '0';

            data_addr_internal         <= ( others => '0');
            data_wdata_o               <= ( others => '0');
            data_be_internal           <= ( others => '0');
            data_we_o                  <= '0';
            data_req_o                 <= '0';

      --      data_we_o                  <= data_we_o_wires;
      --      data_req_o                 <= data_req_o_wires;
      --      data_addr_internal         <= data_addr_internal_wires;
      --      data_wdata_o               <= data_wdata_o_wires;
      --      data_be_internal           <= data_be_internal_wires;

          data_width_ID_voted_wire <= data_width_ID_buf(harc_EXEC);-- or data_width_ID_buf_wire(1) or data_width_ID_buf_wire(0);--(data_width_ID_buf_wire(harc_EXEC) and data_width_ID_buf_wire(1) ) or ( data_width_ID_buf_wire(harc_EXEC) and data_width_ID_buf_wire(0) ) or ( data_width_ID_buf_wire(1) and data_width_ID_buf_wire(0) );
          decoded_instruction_LS_voted_wire  <= decoded_instruction_LS_buf(harc_EXEC);-- or decoded_instruction_LS_buf_wire(1) or decoded_instruction_LS_buf_wire(0);--(decoded_instruction_LS_buf_wire(harc_EXEC) and decoded_instruction_LS_buf_wire(1) ) or ( decoded_instruction_LS_buf_wire(harc_EXEC) and decoded_instruction_LS_buf_wire(0) ) or ( decoded_instruction_LS_buf_wire(1) and decoded_instruction_LS_buf_wire(0) );               

          nextstate_LS <= normal;

        end if;  

        ls_except_condition        <= ls_except_condition_internal;
        ls_taken_branch            <= ls_taken_branch_internal    ;
        core_busy_LS               <= core_busy_LS_internal       ;
        busy_LS                    <= busy_LS_internal            ;


        ----------------------------------------------------------------------------------------------------
        --  ██╗   ██╗ █████╗ ██╗     ██╗██████╗     ██╗    ██╗ █████╗ ██╗████████╗██╗███╗   ██╗ ██████╗   --
        --  ██║   ██║██╔══██╗██║     ██║██╔══██╗    ██║    ██║██╔══██╗██║╚══██╔══╝██║████╗  ██║██╔════╝   --
        --  ██║   ██║███████║██║     ██║██║  ██║    ██║ █╗ ██║███████║██║   ██║   ██║██╔██╗ ██║██║  ███╗  --
        --  ╚██╗ ██╔╝██╔══██║██║     ██║██║  ██║    ██║███╗██║██╔══██║██║   ██║   ██║██║╚██╗██║██║   ██║  --
        --   ╚████╔╝ ██║  ██║███████╗██║██████╔╝    ╚███╔███╔╝██║  ██║██║   ██║   ██║██║ ╚████║╚██████╔╝  --
        --    ╚═══╝  ╚═╝  ╚═╝╚══════╝╚═╝╚═════╝      ╚══╝╚══╝ ╚═╝  ╚═╝╚═╝   ╚═╝   ╚═╝╚═╝  ╚═══╝ ╚═════╝   --
        ----------------------------------------------------------------------------------------------------
        -- when data_valid_waiting, you normally wait for a valid data from data_rvalid_i. During thread 2 operations, this value is substituted by load_valid or store_valid signal 
        when data_valid_waiting =>  
          data_addr_internal_wires <= data_addr_internal_lat;

          if decoded_instruction_LS_voted(LW_bit_position) = '1'  or (decoded_instruction_LS_voted(AMOSWAP_bit_position) = '1' and amo_store_lat = '0' and amo_load_skip = '0') then
            if data_rvalid_i = '1' or load_valid = '1' or store_valid = '1' then
              LS_WB_EN_wire <= '1';
            end if;
          end if;

          if decoded_instruction_LS_voted(LH_bit_position) = '1' or decoded_instruction_LS_voted(LHU_bit_position) = '1' then 
            if data_rvalid_i = '1' or load_valid = '1' or store_valid = '1' then
              case data_addr_internal(1) is
                when '0' =>
                  LS_WB_EN_wire <= '1';
                when '1' =>
                  LS_WB_EN_wire <= '1';
                when others =>
                  null;
              end case;
            end if;
          end if;

          if decoded_instruction_LS_voted(LB_bit_position) = '1' or decoded_instruction_LS_voted(LBU_bit_position) = '1' then 
            if data_rvalid_i = '1' or load_valid = '1' or store_valid = '1' then   
              LS_WB_EN_wire <= '1';
            end if;
          end if;

 
          if decoded_instruction_LS_voted(KMEMLD_bit_position)   = '1' or
             decoded_instruction_LS_voted(KBCASTLD_bit_position) = '1' then
            if accl_en = 1 then   
              if data_rvalid_i = '1'  or load_valid = '1' or store_valid = '1' then
                RS1_Data_IE_wire_lat <= std_logic_vector(unsigned(RS1_Data_IE_lat) + "100");
                RD_Data_IE_wire_lat  <= std_logic_vector(unsigned(RD_Data_IE_lat)  + "100");
                if unsigned(RS2_Data_IE_lat(Addr_Width downto 0)) > 4 then --decrement by four bytes
                  RS2_Data_IE_wire_lat <= std_logic_vector(unsigned(RS2_Data_IE_lat) - "100");
                else -- else set the MSB to '1' indicating that no bytes are left
                  RS2_Data_IE_wire_lat(Addr_Width+1) <= '1'; -- singaling that kmemstr is done
                  RS2_Data_IE_wire_lat(2 downto 0)   <= (others => '0');
                end if;
              end if;
              if overflow_rd_sc(Addr_Width) = '1' then
                ls_except_condition_wires  <= '1';
                ls_taken_branch_wires      <= '1';
              elsif RS2_Data_IE_lat(Addr_Width+1) = '0' then
                busy_LS_wires              <= '1';
                data_be_internal_wires     <= "1111";
                data_req_o_wires           <= '1';
                data_addr_internal_wires <= RS1_Data_IE_wire_lat;
                nextstate_LS <= data_valid_waiting;
                ls_sci_we(to_integer(unsigned(ls_rd_to_sc))) <= '1';
                ls_sc_write_addr <= RD_Data_IE_lat(Addr_Width-1 downto SIMD_BITS+2);
                kmemld_inflight(to_integer(unsigned(ls_rd_to_sc))) <= '1';
              end if;
              if data_rvalid_i = '1'  or load_valid = '1' or store_valid = '1' then
                if unsigned(RS2_Data_IE_lat(Addr_Width downto 0)) >= 4 then
                  ls_sc_data_write_wire <= data_rdata_i;
                else
                  ls_sc_data_write_wire(8*to_integer(unsigned(RS2_Data_IE_lat(1 downto 0))) -1 downto 0) <= data_rdata_i(8*to_integer(unsigned(RS2_Data_IE_lat(1 downto 0))) -1 downto 0);
                end if;
                ls_sci_req(to_integer(unsigned(ls_rd_to_sc))) <= '1';
                if sc_word_count = SIMD-1 then
                  sc_word_count_wire <= 0;
                else
                  sc_word_count_wire <= sc_word_count + 1;
                end if;
              end if;
            end if;

          elsif decoded_instruction_LS_voted(KMEMSTR_bit_position) = '1' then
            if accl_en = 1 then
              if data_rvalid_i = '1' or load_valid = '1' or store_valid = '1'  then
                RS1_Data_IE_wire_lat <= std_logic_vector(unsigned(RS1_Data_IE_lat) + "100");
                RD_Data_IE_wire_lat  <= std_logic_vector(unsigned(RD_Data_IE_lat)  + "100");
                if unsigned(RS2_Data_IE_lat(Addr_Width downto 0)) > 4 then
                  RS2_Data_IE_wire_lat <= std_logic_vector(unsigned(RS2_Data_IE_lat) - "100");
                else
                  RS2_Data_IE_wire_lat(Addr_Width+1) <= '1'; -- singaling that kmemstr is done
                  RS2_Data_IE_wire_lat(2 downto 0)   <= (others => '0');
                end if;
              end if;
              if overflow_rs1_sc(Addr_Width) = '1' then
                ls_except_condition_wires  <= '1';
                ls_taken_branch_wires      <= '1';
              elsif RS2_Data_IE_lat(Addr_Width+1) = '0' then -- if that bit "Addr_Width+1" is high, then kmemstr is done
                busy_LS_wires      <= '1';
                nextstate_LS <= data_valid_waiting;
                ls_sc_read_addr <= RS1_Data_IE_wire_lat(Addr_Width - 1 downto SIMD_BITS+2);
                kmemstr_inflight(to_integer(unsigned(ls_rs1_to_sc))) <= '1';
                if to_integer(unsigned(ls_data_gnt_i)) /= 0 then
                  data_be_internal_wires     <= "1111";
                  data_req_o_wires           <= '1';
                  data_we_o_wires            <= '1';
                  data_addr_internal_wires <= RD_Data_IE_lat;
                  data_wdata_o_wires <= ls_sc_data_read_wire;
                end if;
                -- Increments the address of the SC memory every four words for KMEMSTR
                if data_rvalid_i = '1'  or load_valid = '1' or store_valid = '1' then
                  ls_sci_req(to_integer(unsigned(ls_rs1_to_sc))) <= '1';
                  if sc_word_count = SIMD-1 then
                    sc_word_count_wire <= 0;
                  else
                    sc_word_count_wire <= sc_word_count + 1;
                  end if;
                end if;
              end if;
            end if;
    
          elsif data_rvalid_i = '1'  or load_valid = '1' or store_valid = '1' then
            if store_op = '1' or add_vect_bits(store_op_buf_wire) > 1 then -- SW or AMOSWAP data writing
              if decoded_instruction_LS_voted(SW_bit_position) = '1' then  -- SW data writing
                data_wdata_o_wires     <= RS2_Data_IE_lat(31 downto 0);
                data_we_o_wires        <= '1';  -- is a writing
                data_be_internal_wires <= data_be_ID;
              end if;
              if decoded_instruction_LS_voted(SH_bit_position) = '1' then  -- SH data writing
                case data_addr_internal(1) is
                  when '0' =>
                    data_wdata_o_wires <= RS2_Data_IE_lat(31 downto 0);
                    data_we_o_wires        <= '1';  -- is a writing
                    data_be_internal_wires <= data_be_ID;
                  when '1' =>
                    data_wdata_o_wires <= RS2_Data_IE_lat(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                    data_we_o_wires        <= '1';  -- is a writing
                    data_be_internal_wires <= data_be_ID;
                  when others =>
                    null;
                end case;
              end if;
              if decoded_instruction_LS_voted(SB_bit_position) = '1' then  -- SB data writng
                case data_addr_internal(1 downto 0) is
                  when "00" =>
                    data_wdata_o_wires <= RS2_Data_IE_lat(31 downto 0);
                    data_we_o_wires        <= '1';  -- is a writing
                    data_be_internal_wires <= data_be_ID;
                  when "01" =>
                    data_wdata_o_wires <= RS2_Data_IE_lat(23 downto 0) & std_logic_vector(to_unsigned(0, 8));
                    data_we_o_wires        <= '1';  -- is a writing
                    data_be_internal_wires <= data_be_ID;
                  when "10" =>
                    data_wdata_o_wires <= RS2_Data_IE_lat(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                    data_we_o_wires        <= '1';  -- is a writing
                    data_be_internal_wires <= data_be_ID;
                  when "11" =>
                    data_wdata_o_wires <= RS2_Data_IE_lat(7 downto 0) & std_logic_vector(to_unsigned(0, 24));
                    data_we_o_wires        <= '1';  -- is a writing
                    data_be_internal_wires <= data_be_ID;
                  when others =>
                    null;
                end case;
              end if;
            end if;
    
            if load_op = '1'  or add_vect_bits(load_op_buf_wire) > 1  then
              data_be_internal_wires <= data_be_ID;
              if decoded_instruction_LS_voted(AMOSWAP_bit_position) = '1' then
                core_busy_LS_wires <= '1';          
              end if;
            end if;
          else
            nextstate_LS <= data_valid_waiting;
            busy_LS_wires <= '1';
            -- do not change to "if store_op = '0'" since that will disable store superscalar execution, because store_op resets to 0 on the next cycle
            if decoded_instruction_LS_voted(SW_bit_position) = '0' and  decoded_instruction_LS_voted(SH_bit_position) = '0' and decoded_instruction_LS_voted(SB_bit_position)  = '0' then
              core_busy_LS_wires <= '1';
            end if;
          end if;

          data_addr_internal         <= data_addr_internal_buf(harc_EXEC);
          data_wdata_o               <= data_wdata_o_buf(harc_EXEC);
          data_be_internal           <= data_be_internal_buf(harc_EXEC);
--          data_addr_internal         <= ( others => '0');
--          data_wdata_o               <= ( others => '0');
--          data_be_internal           <= ( others => '0');
          data_we_o                  <= '0';
          data_req_o                 <= '0';
          ls_except_condition        <= ls_except_condition_wires;
          ls_taken_branch            <= ls_taken_branch_wires;
          core_busy_LS               <= core_busy_LS_wires;
          busy_LS                    <= busy_LS_wires;

      end case;
	
    

    if fetch_stage_en = 0 then
      flush_hart_int_wire(harc_EXEC) <= ls_except_condition_wires;
      for h in harc_range loop
        LSU_flush_hart_ID(h) <= flush_hart_int_wire(h);
      end loop;
    elsif fetch_stage_en = 1 then
      for h in harc_range loop
        flush_hart_int_wire(h) <= '0';
        if harc_EXEC = h then
          flush_hart_int_wire(harc_EXEC) <= ls_except_condition_wires;
        end if;
        LSU_flush_hart_ID(h)     <= flush_hart_int_wire(h) or flush_hart_int(h); -- flushes two cycles of the current hart, hence its "int_wire or int"
        LSU_flush_hart_FETCH(h)  <= flush_hart_int_wire(h); -- flushes only one cycle of the current hart
      end loop;
    end if;

  end if;  


  -- LS is running, is active when a LS operation is performed except during a parallel execution (store superscalar operation) 
  if ( add_vect_bits(store_op_buf) >=1  or add_vect_bits(load_op_buf) >=1 ) then 
    if ( harc_EXEC = 1 or harc_loss = 1 ) and restore_fault_PC = '0'  then --make the voting
    elsif ( harc_EXEC = 0 or harc_loss = 0 ) then
      LS_is_running_wire <= '0';
    else
      LS_is_running_wire <= '0'; 
    end if;
  else
    LS_is_running_wire <= '0';
  end if;  
  end process;



  fsm_LS_state : process(clk_i, rst_ni) -- also implements some aux signals
  begin
    if rst_ni = '0' then
      state_LS <= normal; 
      if accl_en = 1 then
        sc_word_count <= 0;
        harc_LS <= ACCL_NUM-1;
      end if;
      flush_hart_int <= (others => '0');
      harc_LOAD      <= THREAD_POOL_SIZE-1;
    elsif rising_edge(clk_i) then
      flush_hart_int         <= flush_hart_int_wire;
      state_LS               <= nextstate_LS;
      data_addr_internal_lat <= data_addr_internal_wires;
      if accl_en = 1 then
	    --halt_lsu_lat             <= halt_lsu;
        sc_word_count            <= sc_word_count_wire;
        harc_LS                  <= harc_LS_wire;
      end if;
    end if;
  end process;



  -----------------------------------------------------------------------------------------------------------------------------------------------------
  -- ██████╗ ████████╗███╗   ███╗██████╗      ██████╗ ██████╗ ███╗   ██╗████████╗██████╗  ██████╗ ██╗         ██╗      ██████╗  ██████╗ ██╗ ██████╗  --
  -- ██╔══██╗╚══██╔══╝████╗ ████║██╔══██╗    ██╔════╝██╔═══██╗████╗  ██║╚══██╔══╝██╔══██╗██╔═══██╗██║         ██║     ██╔═══██╗██╔════╝ ██║██╔════╝  --
  -- ██║  ██║   ██║   ██╔████╔██║██████╔╝    ██║     ██║   ██║██╔██╗ ██║   ██║   ██████╔╝██║   ██║██║         ██║     ██║   ██║██║  ███╗██║██║       --
  -- ██║  ██║   ██║   ██║╚██╔╝██║██╔══██╗    ██║     ██║   ██║██║╚██╗██║   ██║   ██╔══██╗██║   ██║██║         ██║     ██║   ██║██║   ██║██║██║       --
  -- ██████╔╝   ██║   ██║ ╚═╝ ██║██║  ██║    ╚██████╗╚██████╔╝██║ ╚████║   ██║   ██║  ██║╚██████╔╝███████╗    ███████╗╚██████╔╝╚██████╔╝██║╚██████╗  --
  -- ╚═════╝    ╚═╝   ╚═╝     ╚═╝╚═╝  ╚═╝     ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚══════╝    ╚══════╝ ╚═════╝  ╚═════╝ ╚═╝ ╚═════╝  --
  -----------------------------------------------------------------------------------------------------------------------------------------------------                                                                                                                                          

  --dTMR SYNC LOGIC CONTROL
  -- fill the buffers signals and manage the load_valid and store_valid control signals
  dTMR_LSU_SYNC : process(clk_i, rst_ni)  
  begin
    if rst_ni = '0' then
        load_op_buf_lat              <= (others => '0');
        decoded_instruction_LS_voted <= (others => '0');
        data_width_ID_voted          <= (others => '0');
        load_op_buf                  <= (others => '0');
        store_op_buf                 <= (others => '0');
        ls_instr_req_buf             <= (others => '0');
        data_req_o_buf               <= (others => '0');
        data_we_o_buf                <= (others => '0');
    
        data_addr_internal_voted     <= (others => '0');
        data_wdata_o_voted           <= (others => '0');
        data_we_o_voted              <= '0';
        data_req_o_voted             <= '0';
        data_be_internal_voted       <= (others => '0');
    
        for h in harc_range loop
            data_addr_internal_buf(h)       <= (others => '0');
            data_wdata_o_buf(h)             <= (others => '0');
            data_be_internal_buf(h)         <= (others => '0');
            decoded_instruction_LS_buf(h)   <= (others => '0');
            data_width_ID_buf(h)            <= (others => '0');
        end loop;    
        restore_fault_LSU <= '0';
        store_valid <= '0';
        load_valid <= '0';
        restore_store_active <= '0';
        LS_is_running        <= '0';
        LS_is_running_lat    <= '0';
        core_busy_LS_lat     <= '0';
        

    elsif rising_edge(clk_i) then
        decoded_instruction_LS_voted <= decoded_instruction_LS_voted_wire;
        data_width_ID_voted          <= data_width_ID_voted_wire;

        load_op_buf                  <= load_op_buf_wire;
        store_op_buf                 <= store_op_buf_wire;
        ls_instr_req_buf             <= ls_instr_req_buf_wire;
        data_addr_internal_buf       <= data_addr_internal_buf_wire;
        data_wdata_o_buf             <= data_wdata_o_buf_wire;
        data_req_o_buf               <= data_req_o_buf_wire;
        data_we_o_buf                <= data_we_o_buf_wire;
        data_be_internal_buf         <= data_be_internal_buf_wire;
        decoded_instruction_LS_buf   <= decoded_instruction_LS_buf_wire;
        data_width_ID_buf            <= data_width_ID_buf_wire;

        data_addr_internal_voted     <= data_addr_internal_voted_wire;
        data_wdata_o_voted           <= data_wdata_o_voted_wire;
        data_we_o_voted              <= data_we_o_voted_wire;
        data_req_o_voted             <= data_req_o_voted_wire;
        data_be_internal_voted       <= data_be_internal_voted_wire;

        data_rvalid_i_lat            <= data_rvalid_i;
        load_op_buf_lat              <= load_op_buf_wire;

        restore_fault_LSU            <= restore_fault_LSU_wire;
        LS_is_running                <= LS_is_running_wire;
        LS_is_running_lat            <= LS_is_running;

        core_busy_LS_lat             <= core_busy_LS;

        case state_LS is  
          when normal =>
            -- flag signal to replicate the data_rvalid_i signal when the load store operation are redundant
            if add_vect_bits(store_op_buf_wire) > 0 then 
              -- the last LS operation is the real one, and it should work with the data_rvalid_i signal, not with the fake one store_valid 
              if (harc_EXEC /= 1 and restore_fault_PC = '0' ) then --or harc_loss /= 1 ) then
                store_valid <= '1';
              else
                store_valid <= '0';                  
              end if;
            elsif add_vect_bits(load_op_buf_wire) > 0 then
              -- the last LS operation is the real one, and it should work with the data_rvalid_i signal, not with the fake one load_valid 
              if (harc_EXEC /= 1  and restore_fault_PC = '0' ) then --or harc_loss /= 1 ) then
                load_valid <= '1';
              else
                load_valid <= '0';               
              end if;          
            else
              store_valid <= '0';
              load_valid <= '0';
            end if;

          when data_valid_waiting =>  
            store_valid <= '0';
            load_valid <= '0';

          when voting =>

          when others =>
            store_valid <= '0';
            load_valid <= '0';
            null;
        end case;

        -- signal useful for harc_WB management (registerfile) during a restore store operation
        if (data_we_o_buf_wire(0) = '1' and data_rvalid_i = '0') then
          restore_store_active <= '1';
        else
          restore_store_active <= '0';
        end if;

    end if;
  end process;



  --dTMR LOGIC CONTROL
  -- it manages all the dTMR signals, filling the buffers and managing the control signal buffers
  dTMR_LSU_COMB : process(all)  -- the combinational voting for TTMR
  begin
  load_op_buf_wire                  <= load_op_buf;
  store_op_buf_wire                 <= store_op_buf;
  ls_instr_req_buf_wire             <= ls_instr_req_buf;
      
  data_addr_internal_buf_wire       <= data_addr_internal_buf;
  data_wdata_o_buf_wire             <= data_wdata_o_buf;
  data_req_o_buf_wire               <= data_req_o_buf;
  data_we_o_buf_wire                <= data_we_o_buf;
  data_be_internal_buf_wire         <= data_be_internal_buf;  
  decoded_instruction_LS_buf_wire   <= decoded_instruction_LS_buf;
  data_width_ID_buf_wire            <= data_width_ID_buf;

  data_addr_internal_voted_wire     <= data_addr_internal_voted;
  data_wdata_o_voted_wire           <= data_wdata_o_voted;
  data_we_o_voted_wire              <= data_we_o_voted;
  data_req_o_voted_wire             <= data_req_o_voted;
  data_be_internal_voted_wire       <= data_be_internal_voted;

  case state_LS is  
    -- in case of a normal state, fill the buffer if there are load or store operations 
    when normal =>

      if (load_op = '1' or add_vect_bits(load_op_buf) > 0 ) or ( store_op = '1' or add_vect_bits(store_op_buf) > 0 )   then 
        data_addr_internal_buf_wire(harc_EXEC)         <= data_addr_internal_wires;
        data_wdata_o_buf_wire(harc_EXEC)               <= data_wdata_o_wires;
        data_we_o_buf_wire(harc_EXEC)                  <= data_we_o_wires;
        data_req_o_buf_wire(harc_EXEC)                 <= data_req_o_wires;
        data_be_internal_buf_wire(harc_EXEC)           <= data_be_internal_wires;

        data_width_ID_buf_wire(harc_EXEC)              <= data_width_ID;
        decoded_instruction_LS_buf_wire(harc_EXEC)     <= decoded_instruction_LS;     

        -- if there are two subsequent LS operation, turn down the two control signal buffers 
        if ( harc_EXEC = 2 or harc_loss = 2 ) then 
             data_we_o_buf_wire(1)                  <= '0';  
             data_req_o_buf_wire(1)                 <= '0';
             data_we_o_buf_wire(0)                  <= '0';
             data_req_o_buf_wire(0)                 <= '0';
        elsif ( harc_EXEC = 1 or harc_loss = 1 )then
             data_we_o_buf_wire(0)                  <= '0';
             data_req_o_buf_wire(0)                 <= '0';
        end if;

      else
        -- reset the buffers when they are full.
        if add_vect_bits(data_we_o_buf) >1 or add_vect_bits(data_req_o_buf) > 1 then
           for h in harc_range loop  
             data_we_o_buf_wire(h)                  <= '0';
             data_req_o_buf_wire(h)                 <= '0';
           end loop;                    
        end if;
      end if;

    -- don't touch the buffer in case of data_valid state  
    when data_valid_waiting =>

    -- when you are in a voting stage, reset the control signal buffers if you are in a restore state    
    when voting =>
      if add_vect_bits(data_we_o_buf) > 1 or add_vect_bits(data_req_o_buf) > 1 or restore_fault_PC = '1' then
         for h in harc_range loop  
           data_we_o_buf_wire(h)                  <= '0';
           data_req_o_buf_wire(h)                 <= '0';
         end loop;                    
      end if;
  end case;



  -- since store_op and load_op signals remains high during LS operations while ls_instr_req no, they should be managed in different ways 
  if ( harc_EXEC = 2 or harc_loss = 2 ) then 
    store_op_buf_wire(2) <= store_op;
    store_op_buf_wire(1) <= '0';
    store_op_buf_wire(0) <= '0';

    load_op_buf_wire(2) <= load_op;
    load_op_buf_wire(1) <= '0';
    load_op_buf_wire(0) <= '0';

    if ls_instr_req = '1' then
      ls_instr_req_buf_wire(2) <= ls_instr_req;
    else
      if add_vect_bits(ls_instr_req_buf) > 1 then
        ls_instr_req_buf_wire(2) <= '0';
      end if;
    end if;

    ls_instr_req_buf_wire(1) <= '0';
    ls_instr_req_buf_wire(0) <= '0';
  elsif ( harc_EXEC = 1 or harc_loss = 1 )then
      store_op_buf_wire(1) <= store_op;
      store_op_buf_wire(0) <= '0';

      load_op_buf_wire(1) <= load_op;
      load_op_buf_wire(0) <= '0';

      if ls_instr_req = '1' then
        ls_instr_req_buf_wire(1) <= ls_instr_req;
      end if;
      ls_instr_req_buf_wire(0) <= '0';
--        elsif LS_WB_wrong_EXEC = '0' and ( harc_EXEC = 0 or harc_loss = 0 ) then
  elsif ( harc_EXEC = 0 or harc_loss = 0 ) and restore_fault_PC = '1' then

    store_op_buf_wire(0) <= store_op;
    load_op_buf_wire(0) <= load_op;

    if ls_instr_req = '1' then
      ls_instr_req_buf_wire(0) <= ls_instr_req;
    end if;
  end if;

  -- reset the buffers in case of restore operation only if thread 0 is not working 
  if restore_fault_lat = '1' or ( restore_fault_PC = '1' and harc_EXEC /= 0 ) then
    load_op_buf_wire            <= (others => '0');
    store_op_buf_wire           <= (others => '0');
    ls_instr_req_buf_wire       <= (others => '0');
  end if;

  -- reset the buffers when LS_WB_wrong if performed, see Registerfile to understand LS_WB_wrong_END signal
  if LS_WB_wrong_END = '1' then
    store_op_buf_wire(2) <= '0';
    store_op_buf_wire(1) <= '0';
    store_op_buf_wire(0) <= '0';

    load_op_buf_wire(2) <= '0';
    load_op_buf_wire(1) <= '0';
    load_op_buf_wire(0) <= '0';

    if ls_instr_req = '1' then
      ls_instr_req_buf_wire(2) <= ls_instr_req;
    else
      if add_vect_bits(ls_instr_req_buf) > 1 then
        ls_instr_req_buf_wire(2) <= '0';
      end if;
    end if;  

    ls_instr_req_buf_wire(1) <= '0';
    ls_instr_req_buf_wire(0) <= '0';    
  end if; 
    
  end process;




  -----------------------------------------------------------------------------------------------
  --   █████╗ ██████╗ ██████╗ ██████╗ ███████╗███████╗███████╗     ██████╗ ███████╗███╗   ██╗  --
  --  ██╔══██╗██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝    ██╔════╝ ██╔════╝████╗  ██║  --
  --  ███████║██║  ██║██║  ██║██████╔╝█████╗  ███████╗███████╗    ██║  ███╗█████╗  ██╔██╗ ██║  --
  --  ██╔══██║██║  ██║██║  ██║██╔══██╗██╔══╝  ╚════██║╚════██║    ██║   ██║██╔══╝  ██║╚██╗██║  --
  --  ██║  ██║██████╔╝██████╔╝██║  ██║███████╗███████║███████║    ╚██████╔╝███████╗██║ ╚████║  --
  --  ╚═╝  ╚═╝╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝     ╚═════╝ ╚══════╝╚═╝  ╚═══╝  --
  -----------------------------------------------------------------------------------------------
--  Address generation moved into Registerfile 
--  LSU_Mapper_comb : process(all)
--  begin
--    add_op_A <= (others => '0');
--    add_op_B <= (others => '0');
--
--
----    if ( harc_EXEC = 1 or harc_loss = 1 or ((harc_EXEC = 0 or harc_loss = 0) ) ) then --make the voting
----      -- MAP input address generator -----------------------------------------------------------------------------------
----      if load_op = '1' or add_vect_bits(load_op_buf_wire) >= 1 then  -- address building operands
----        add_op_A <= RS1_data_IE;
----        add_op_B <= I_immediate(instr_word_IE);
----      end if;
----      if store_op = '1' or add_vect_bits(store_op_buf_wire) >= 1 then -- address building operands
----        add_op_A <= RS1_data_IE;
----        add_op_B <= S_immediate(instr_word_IE);
----      end if;
----      if accl_en = 1 then
----        if decoded_instruction_LS_voted(KMEMLD_bit_position)   = '1' or  -- calculates overflow spm write
----           decoded_instruction_LS_voted(KBCASTLD_bit_position) = '1' then
----          add_op_A <= (Addr_Width to 31 => '0') & RD_data_IE(Addr_Width -1 downto 0);
----          add_op_B <= (Addr_Width to 31 => '0') & std_logic_vector(unsigned(RS2_data_IE(Addr_Width -1 downto 0))-1);
----        end if;
----        if decoded_instruction_LS_voted(KMEMSTR_bit_position) = '1' then -- calculates overflow spm read
----          add_op_A <= (Addr_Width to 31 => '0') & RS1_data_IE(Addr_Width -1 downto 0);
----          add_op_B <= (Addr_Width to 31 => '0') & std_logic_vector(unsigned(RS2_data_IE(Addr_Width -1 downto 0))-1);
----        end if;
----      end if;
----    else
--      -- MAP input address generator -----------------------------------------------------------------------------------
--      if load_op = '1' or add_vect_bits(load_op_buf_wire) >= 1 then  -- address building operands
--        add_op_A <= RS1_data_IE;
--        add_op_B <= I_immediate(instr_word_IE);
--      end if;
--      if store_op = '1' or add_vect_bits(store_op_buf_wire) >= 1 then -- address building operands
--        add_op_A <= RS1_data_IE;
--        add_op_B <= S_immediate(instr_word_IE);
--      end if;
--      if accl_en = 1 then
--        if decoded_instruction_LS(KMEMLD_bit_position)   = '1' or  -- calculates overflow spm write
--           decoded_instruction_LS(KBCASTLD_bit_position) = '1' then
--          add_op_A <= (Addr_Width to 31 => '0') & RD_data_IE(Addr_Width -1 downto 0);
--          add_op_B <= (Addr_Width to 31 => '0') & std_logic_vector(unsigned(RS2_data_IE(Addr_Width -1 downto 0))-1);
--        end if;
--        if decoded_instruction_LS(KMEMSTR_bit_position) = '1' then -- calculates overflow spm read
--          add_op_A <= (Addr_Width to 31 => '0') & RS1_data_IE(Addr_Width -1 downto 0);
--          add_op_B <= (Addr_Width to 31 => '0') & std_logic_vector(unsigned(RS2_data_IE(Addr_Width -1 downto 0))-1);
--        end if;
--      end if;
----    end if;
--     -- Perform the addition ---------------------------------------------------
--    add_out <= std_logic_vector(signed(add_op_A) + signed(add_op_B));
--    ---------------------------------------------------------------------------
--  end process;


  LSU_Mapper_comb : process(all)
  begin
    add_op_A <= (others => '0');
    add_op_B <= (others => '0');


      -- MAP input address generator -----------------------------------------------------------------------------------
      if load_op = '1' or add_vect_bits(load_op_buf_wire) >= 1 then  -- address building operands
        add_out <= add_out_load; 
      end if;
      if store_op = '1' or add_vect_bits(store_op_buf_wire) >= 1 then -- address building operands
        add_out <= add_out_store; 
      end if;
      if accl_en = 1 then
        if decoded_instruction_LS_voted(KMEMLD_bit_position)   = '1' or  -- calculates overflow spm write
           decoded_instruction_LS_voted(KBCASTLD_bit_position) = '1' then
          add_op_A <= (Addr_Width to 31 => '0') & RD_data_IE(Addr_Width -1 downto 0);
          add_op_B <= (Addr_Width to 31 => '0') & std_logic_vector(unsigned(RS2_data_IE(Addr_Width -1 downto 0))-1);
        end if;
        if decoded_instruction_LS_voted(KMEMSTR_bit_position) = '1' then -- calculates overflow spm read
          add_op_A <= (Addr_Width to 31 => '0') & RS1_data_IE(Addr_Width -1 downto 0);
          add_op_B <= (Addr_Width to 31 => '0') & std_logic_vector(unsigned(RS2_data_IE(Addr_Width -1 downto 0))-1);
        end if;

      -- Perform the addition ---------------------------------------------------
      add_out <= std_logic_vector(signed(add_op_A) + signed(add_op_B));

      end if;

    ---------------------------------------------------------------------------
  end process;




--------------------------------------------------------------------- end of LSU -----------------
--------------------------------------------------------------------------------------------------

end LSU;
--------------------------------------------------------------------------------------------------
-- END of Load-Store architecture ----------------------------------------------------------------
--------------------------------------------------------------------------------------------------
