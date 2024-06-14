# frozen_string_literal: false

module AoC2015
  class Day18
    def part1(input)
      @grid = {}
      input.lines.each_with_index { |line, i| parse_line(line, i) }
      STEPS
        .times
        .reduce(@grid) { |grid, _| animate(grid) }
        .yield_self { |grid| count_on(grid) }
    end

    def part2(input)
      @grid = {}
      input.lines.each_with_index { |line, i| parse_line(line, i) }
      illuminate_corners(@grid)

      STEPS
        .times
        .reduce(@grid) { |grid, _| g = animate(grid); illuminate_corners(g); g }
        .yield_self { |grid| count_on(grid) }
    end

    private

    def count_on(grid)
      grid.values.count { |c| c == ON }
    end

    def illuminate_corners(grid)
      grid[[0, 0]] = grid[[0, 99]] = grid[[99, 0]] = grid[[99, 99]] = ON
    end

    def animate(grid)
      step = {}
      grid.each_key do |(x, y)|
        step[[x, y]] = if on?(grid, x, y)
                         turn_off?(grid, x, y) ? OFF : ON
                       else
                         turn_on?(grid, x, y) ? ON : OFF
                       end
      end
      step
    end

    def turn_off?(grid, x, y)
      count_neighbours_on(grid, x, y)
        .yield_self { |l| l < 2 || l > 3 }
    end

    def turn_on?(grid, x, y)
      count_neighbours_on(grid, x, y) == 3
    end

    def count_neighbours_on(grid, x, y)
      neighbours(x, y)
        .filter { |(a, b)| on?(grid, a, b) }
        .size
    end

    def neighbours(x, y)
      (x - 1..x + 1).flat_map { |a| (y - 1..y + 1).map { |b| [a, b] } } - [[x, y]]
    end

    def parse_line(line, line_index)
      line
        .chars
        .each_with_index { |char, j| parse_char(char, line_index, j) }
    end

    def parse_char(char, line_index, char_index)
      return if char == "\n"

      @grid[[line_index, char_index]] = char
    end

    def on?(grid, x, y)
      grid.fetch([x, y], OFF) == ON
    end

    def off?(grid, x, y)
      grid.fetch([x, y], OFF) == OFF
    end

    STEPS = 100
    ON = "#".freeze
    OFF = ".".freeze
  end
end
