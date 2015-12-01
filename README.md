-- Project EE4620
-- Created by: 
-- Byron Burkes
-- Vince Haenni
-- Theodore Hood
-- Alexander Kinkade

------------------------------------------------------------------------------
-- PB_N   => Displays next value
-- PB_S   => Displays first value in list
-- DIP1   => Sorts list
-----------------------------------------------------------------------------
library ieee,unisim;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use unisim.vcomponents.all;

entity sorter is
port(

-- pushbuttons
GPIO_SW_N : in std_logic;  
GPIO_SW_S : in std_logic;  
 
 --dipswitch
 GPIO_DIP_SW     : in    std_logic;  
 
  --lcd display port
  LCD_DB4_LS  :   out std_logic; 
  LCD_DB5_LS  :   out std_logic;
  LCD_DB6_LS  :   out std_logic;
  LCD_DB7_LS  :   out std_logic;
  LCD_E_LS    :   out std_logic;
  LCD_RS_LS   :   out std_logic;
  LCD_RW_LS   :   out std_logic);
  end;
 
architecture structural of sorter is
  
  signal PB_EVENT_N,PB_EVENT_S   : std_logic;        -- PB pulse
  signal PB_DELAY_N,PB_DELAY_S   : std_logic_vector(11 downto 0);
  
  end;
