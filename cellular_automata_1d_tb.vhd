
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;

entity cellular_automata_1d_tb is
  generic (G_LEN : integer := 128);
end cellular_automata_1d_tb;

architecture behavioral of cellular_automata_1d_tb is

  signal clk                       : std_logic := '1';
  signal rst                       : std_logic;
  signal start                     : std_logic;
  signal busy                      : std_logic;
  signal data_in                   : std_logic_vector(G_LEN-1 downto 0);
  signal data_out                  : std_logic_vector(G_LEN-1 downto 0);
  signal valid_data_out            : std_logic;
  signal rule                      : integer;
  signal rule_vec                  : std_logic_vector(7 downto 0);
  signal number_of_generations     : integer;
  signal number_of_generations_vec : std_logic_vector(31 downto 0);

  file output_file : text;

begin

  DUT : entity work.cellular_automata_1d
    generic map(
      G_LEN => G_LEN
      )
    port map (
      clk                   => clk,
      rst                   => rst,
      start                 => start,
      busy                  => busy,
      data_in               => data_in,
      data_out              => data_out,
      rule                  => rule_vec,
      number_of_generations => number_of_generations_vec,
      valid_data_out        => valid_data_out
      );

  rule_vec                  <= std_logic_vector(to_unsigned(rule, 8));
  number_of_generations_vec <= std_logic_vector(to_unsigned(number_of_generations, 32));

  clk <= not clk after 10 ns;

  process(clk)
    variable buf : line;
  begin
    if(rising_edge(clk)) then
      if(rst = '1') then
      else
        if(valid_data_out = '1') then
          write(buf, data_out);
          writeline(output_file, buf);
        end if;
      end if;
    end if;
  end process;

  process
    variable buf              : line;
    variable output_file_name : string(1 to 100);
  begin
    rst   <= '1';
    start <= '0';
    wait for 1 us;
    wait until rising_edge(clk);
    rst   <= '0';
    wait until rising_edge(clk);

    data_in <= X"00000000" & X"80000000" & X"00000008" & X"00000000";

    number_of_generations <= 1000;

    -- rule 0 to 255
    for i in 0 to 255 loop
      wait until rising_edge(clk);

      -- open output_<rule>.txt
      output_file_name := string'("output_" & integer'image(i) & ".txt");
      file_open(output_file, output_file_name, write_mode);

      -- write data_in to the log
      write(buf, data_in);
      writeline(output_file, buf);

      rule <= i;

      -- Start calculation
      start <= '1';
      wait until rising_edge(clk);
      start <= '0';

      -- Waiting for completion
      while true loop
        wait until rising_edge(clk);
        if(busy = '0') then
          wait until rising_edge(clk);
          file_close(output_file);
          exit;
        end if;
        wait until rising_edge(clk);
      end loop;

    end loop;

    wait;                               -- end

  end process;

end behavioral;

