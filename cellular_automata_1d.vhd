
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cellular_automata_1d is
  generic (
    G_LEN : integer := 64               -- Must be 2^N
    );
  port (
    clk                   : in  std_logic;
    rst                   : in  std_logic;
    start                 : in  std_logic;
    busy                  : out std_logic;
    rule                  : in  std_logic_vector(7 downto 0);
    number_of_generations : in  std_logic_vector(31 downto 0);
    data_in               : in  std_logic_vector(G_LEN-1 downto 0);
    data_out              : out std_logic_vector(G_LEN-1 downto 0);
    valid_data_out        : out std_logic
    );
end cellular_automata_1d;

architecture rtl of cellular_automata_1d is

  type state_t is (ST_IDLE, ST_SET, ST_RUN, ST_STOP);
  signal state : state_t;

  signal data_latch : std_logic_vector(G_LEN-1 downto 0);

  signal generation_count : unsigned(31 downto 0);

  function step (
    input_bit : std_logic_vector(G_LEN-1 downto 0);
    rule_vec  : std_logic_vector(7 downto 0)) return std_logic_vector is
    variable blk        : std_logic_vector(2 downto 0);
    variable index1     : unsigned(G_LEN-1 downto 0);
    variable index2     : unsigned(G_LEN-1 downto 0);
    variable index3     : unsigned(G_LEN-1 downto 0);
    variable output_bit : std_logic_vector(G_LEN-1 downto 0);
  begin

    for i in 0 to G_LEN-1 loop

      if(i = G_LEN-1) then
        index1 := to_unsigned(G_LEN-1, G_LEN);
      else
        index1 := to_unsigned(i+1, G_LEN);
      end if;

      index2 := to_unsigned(i, G_LEN);

      if(i = 0) then
        index3 := to_unsigned(0, G_LEN);
      else
        index3 := to_unsigned(i-1, G_LEN);
      end if;

      blk := input_bit(to_integer(index1)) & input_bit(to_integer(index2)) & input_bit(to_integer(index3));
      case(blk) is
        when "000" =>
          output_bit(i) := rule_vec(0);
        when "001" =>
          output_bit(i) := rule_vec(1);
        when "010" =>
          output_bit(i) := rule_vec(2);
        when "011" =>
          output_bit(i) := rule_vec(3);
        when "100" =>
          output_bit(i) := rule_vec(4);
        when "101" =>
          output_bit(i) := rule_vec(5);
        when "110" =>
          output_bit(i) := rule_vec(6);
        when "111" =>
          output_bit(i) := rule_vec(7);
      end case;

    end loop;

    return output_bit;
  end function;

begin

  -----------------------------------------------------------------------------
  -- State
  -----------------------------------------------------------------------------
  process(clk)
  begin
    if(rising_edge(clk))then
      if(rst = '1') then
        state            <= ST_IDLE;
        generation_count <= (others => '0');
        valid_data_out   <= '0';
        busy             <= '0';
      else
        case (state) is
          when ST_IDLE =>
            if(start = '1') then
              state <= ST_SET;
              busy  <= '1';
            end if;

          when ST_SET =>
            data_latch <= data_in;
            state      <= ST_RUN;

          when ST_RUN =>
            generation_count <= generation_count + 1;
            data_latch       <= step(data_latch, rule);
            valid_data_out   <= '1';
            if(generation_count = unsigned(number_of_generations)) then
              state <= ST_STOP;
            end if;

          when ST_STOP =>
            valid_data_out   <= '0';
            busy             <= '0';
            generation_count <= (others => '0');
            state            <= ST_IDLE;

          when others =>
            state <= ST_IDLE;
        end case;
      end if;
    end if;
  end process;
  data_out <= data_latch;

end rtl;



