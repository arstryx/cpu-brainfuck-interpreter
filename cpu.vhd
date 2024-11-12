-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Arsenii Zakharenko xzakha02
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is


-- PC signals
signal PC_REG: std_logic_vector(12 downto 0);
signal PC_INC: std_logic;
signal PC_DEC: std_logic;


-- PTR signals
signal PTR_REG: std_logic_vector(12 downto 0);
signal PTR_INC: std_logic;
signal PTR_DEC: std_logic;

-- MX signals
signal MX1_SEL: std_logic;
signal MX2_SEL: std_logic_vector(1 downto 0);

-- TMP signals
-- TODO

-- FSM signals, states
type FSM_STATE is
(
  ST_RESET,
  ST_WAIT1,
  ST_WAIT2,
  ST_FETCH,
  ST_DECODE,
  ST_PTR_INC,
  ST_PTR_DEC,
  ST_VAL_INC1,
  ST_VAL_INC2,
  ST_VAL_DEC1, 
  ST_VAL_DEC2, 
  ST_PRINT_1,
  ST_PRINT_2,
  ST_READ_1,
  ST_READ_2,
  ST_WHILE_START_1,
  ST_WHILE_START_2,
  ST_WHILE_START_3,
  ST_WHILE_START_4,
  ST_WHILE_END_1,
  ST_WHILE_END_2,
  ST_WHILE_END_3,
  ST_WHILE_END_4,
  ST_COMMENT, -- For not instructions....
  ST_HALT
  -- TODO
);
signal NOW_STATE: FSM_STATE;
signal NEXT_STATE: FSM_STATE;

begin

-- FSM logic
FSM_NOW_STATE: process(CLK, RESET, EN) begin
  if RESET = '1' then
    NOW_STATE <= ST_RESET;
  elsif rising_edge(CLK) and (EN = '1') then
    NOW_STATE <= NEXT_STATE;
  end if;
end process;

-- Getting next FSM state
FSM_NEXT_STATE: process(NOW_STATE, IN_VLD, DATA_RDATA) begin

  -- Setting basic values
      PTR_INC <= '0';
      PTR_DEC <= '0';
      PC_INC <= '0';
      PC_DEC <= '0';
      DATA_EN <= '0';
      OUT_WE <= '0';
      IN_REQ <= '0';
      DATA_RDWR <= '1'; -- ReadMode 
      OUT_DATA <= DATA_RDATA;
      





  case NOW_STATE is



    -- Initialising

    when ST_RESET =>
      READY <= '0';
      DONE <= '0';
      NEXT_STATE <= ST_WAIT1;

    -- Reading the symbol, waiting for @
    when ST_WAIT1 =>
      MX1_SEL <= '1'; -- PTR to Data Address
      DATA_EN <= '1'; -- Enable reading
      DATA_RDWR <= '1'; -- Read mode
      NEXT_STATE <= ST_WAIT2; -- Go next

    -- If @ then move on, otherwise loop
    when ST_WAIT2 =>
      if DATA_RDATA = X"40" then
        READY <= '1'; -- READY is 1 as soon as @ is got
        NEXT_STATE <= ST_FETCH; -- Ready to fetch instructions...
      else
        NEXT_STATE <= ST_WAIT1; -- Or step back if not @
      end if;
      PTR_INC <= '1'; -- Increase pointer anyway









    -- Reading the next instruction

    when ST_FETCH =>
      DATA_RDWR <= '1'; -- ReadMode
      MX1_SEL <= '0'; -- PC to Data Address to read the instr
      DATA_EN <= '1'; --Enable read
      NEXT_STATE <= ST_DECODE; -- ... and decode






    -- Decoding the instruction

    when ST_DECODE => 
      case DATA_RDATA is -- Data RDATA contains instruction
        when X"40" => NEXT_STATE <= ST_HALT; -- @
        when X"3E" => NEXT_STATE <= ST_PTR_INC; -- >
        when X"3C" => NEXT_STATE <= ST_PTR_DEC;
        when X"2B" => NEXT_STATE <= ST_VAL_INC1;
        when X"2D" => NEXT_STATE <= ST_VAL_DEC1;
        when X"2E" => NEXT_STATE <= ST_PRINT_1;
        when X"2C" => NEXT_STATE <= ST_READ_1;
        when X"5B" => NEXT_STATE <= ST_WHILE_START_1;
        when X"5D" => NEXT_STATE <= ST_WHILE_END_1;
        when others => NEXT_STATE <= ST_COMMENT;
      end case;




    -- Simple ops with the pointer
    
    when ST_PTR_INC => -- Increase
      PTR_INC <= '1';
      PC_INC <= '1'; -- Always increase PC before fetching
      NEXT_STATE <= ST_FETCH;

    when ST_PTR_DEC => -- Decrease
      PTR_DEC <= '1';
      PC_INC <= '1';
      NEXT_STATE <= ST_FETCH;
    




    -- Changing cells` values

    when ST_VAL_INC1 => 
      DATA_RDWR <= '1'; -- ReadMode
      MX1_SEL <= '1'; -- PTR as address
      DATA_EN <= '1'; -- Enable data
      MX2_SEL <= "11"; -- Data to write will be Data Read + 1
      NEXT_STATE <= ST_VAL_INC2; -- Update cell

    
    when ST_VAL_INC2 =>
      DATA_EN <= '1';
      DATA_RDWR <= '0'; -- Write mode
      PC_INC <= '1';
      NEXT_STATE <= ST_FETCH;

    when ST_VAL_DEC1 => -- See increment, almost the same
      MX1_SEL <= '1';
      DATA_EN <= '1';
      DATA_RDWR <= '1';
      MX2_SEL <= "01";
      NEXT_STATE <= ST_VAL_DEC2;

    when ST_VAL_DEC2 =>
      DATA_EN <= '1';
      DATA_RDWR <= '0';
      PC_INC <= '1';
      NEXT_STATE <= ST_FETCH;





    


    -- Print Output Instruction

    when ST_PRINT_1 =>
        DATA_RDWR <= '1'; -- Read Mode
      MX1_SEL <= '1'; -- PTR to DATA_ADDR
      DATA_EN <= '1'; -- Enable read data
      NEXT_STATE <= ST_PRINT_2; -- Go to the next part

    when ST_PRINT_2 =>
      if OUT_BUSY = '0' then -- If not busy...
        OUT_INV <= '0';
        OUT_WE <= '1'; -- Enable output
        OUT_DATA <= DATA_RDATA; -- Read data to output
        PC_INC <= '1'; -- Next instr
        NEXT_STATE <= ST_FETCH; -- Finished, go fetch
      else -- Otherwise...
        NEXT_STATE <= ST_PRINT_1; -- Step back
      end if;






    -- Read Input Instruction

    when ST_READ_1 =>
      MX1_SEL <= '1'; -- PTR to DATA_ADDR
      IN_REQ <= '1'; -- Require input
      NEXT_STATE <= ST_READ_2; -- Go next

    when ST_READ_2 =>
      if IN_VLD = '1' then -- If Data are valid
        DATA_EN <= '1';
        DATA_RDWR <= '0'; -- Write Mode
        MX2_SEL <= "00"; -- IN_DATA to DATA_WDATA
        PC_INC <= '1'; -- PC++ to go to next instr
        NEXT_STATE <= ST_FETCH; --return to fetch
      else
        NEXT_STATE <= ST_READ_1; --Otherwise retry
      end if;



    -- Simple while cycling using recursive manipulations with PC
    
    when ST_WHILE_START_1 => -- Read the value of the cell
      DATA_EN <= '1';
      PC_INC <= '1';
      MX1_SEL <= '1';
      NEXT_STATE <= ST_WHILE_START_2;

    when ST_WHILE_START_2 => -- Data is read, process
      if DATA_RDATA = "00000000" then -- if zero, goto ]
        
        NEXT_STATE <= ST_WHILE_START_3;
      else -- otherwise continue
        NEXT_STATE <= ST_FETCH;
      end if;

    when ST_WHILE_START_3 =>
      DATA_EN <= '1';
      MX1_SEL <= '0';
      NEXT_STATE <= ST_WHILE_START_4;

    when ST_WHILE_START_4 =>
      if DATA_RDATA = X"5D" then
        PC_INC <= '1';
        NEXT_STATE <= ST_FETCH;
      else
        PC_INC <= '1';
        NEXT_STATE <= ST_WHILE_START_3;
      end if;


    when ST_WHILE_END_1 =>
      DATA_EN <= '1';
      MX1_SEL <= '1';
      NEXT_STATE <= ST_WHILE_END_2;

    when ST_WHILE_END_2 =>
      if DATA_RDATA = "00000000" then
        PC_INC <= '1';
        NEXT_STATE <= ST_FETCH;
      else
        PC_DEC <= '1';
        NEXT_STATE <= ST_WHILE_END_3;
      end if;
    
    when ST_WHILE_END_3 =>
      DATA_EN <= '1';
      MX1_SEL <= '0';
      NEXT_STATE <= ST_WHILE_END_4;

    when ST_WHILE_END_4 =>
      if DATA_RDATA = X"5B" then
        PC_INC <= '1';
        NEXT_STATE <= ST_FETCH;
      else
        PC_DEC <= '1';
        NEXT_STATE <= ST_WHILE_END_3;
      end if;
      

    
    
      when ST_COMMENT => -- When unknowm intr (comment), just pc++ and go on
        PC_INC <= '1';
        NEXT_STATE <= ST_FETCH;
        


    -- Finishing the programm

    when ST_HALT =>
      DONE <= '1';
      NEXT_STATE <= ST_HALT;


    when others =>

  end case;
end process;


-- PC register logic
PC_REG_PROC: process(CLK, RESET, PC_REG, PC_INC, PC_DEC) begin
  if RESET = '1' then
    PC_REG <= (OTHERS => '0');
  elsif rising_edge(CLK) then
    if PC_INC = '1' then
      PC_REG <= PC_REG + 1;
    elsif PC_DEC = '1' then
      PC_REG <= PC_REG - 1;
    end if;
  end if ;
end process;


-- PTR register logic
PTR_REG_PROC: process(CLK, RESET, PTR_REG, PTR_INC, PTR_DEC) begin
  if RESET = '1' then
    PTR_REG <= (OTHERS => '0');
  elsif rising_edge(CLK) then
    if PTR_INC = '1' then
      PTR_REG <= PTR_REG + 1;
    elsif PTR_DEC = '1' then
      PTR_REG <= PTR_REG - 1;
    end if;
  end if ;
end process;

  

-- Multiplexer 1 logic
MX1_PROC: process(MX1_SEL, PC_REG, PTR_REG) begin
  case MX1_SEL is
    when '0' => DATA_ADDR <= PC_REG;
    when '1' => DATA_ADDR <= PTR_REG;
    when others => 
  end case;
end process;



-- Multiplexer 2 logic
MX2_PROC: process(MX2_SEL, IN_DATA, DATA_RDATA) begin
  case MX2_SEL is
    when "00" => DATA_WDATA <= IN_DATA;
    when "11" => DATA_WDATA <= DATA_RDATA + 1;
    when "01" => DATA_WDATA <= DATA_RDATA - 1;
    -- 10 fot TMP TODO
    when others =>
  end case;
end process;


-- TMP register logic
-- TODO
-- Sorry, cas to udelat uz nedovoluje

end behavioral;