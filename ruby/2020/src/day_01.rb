# frozen_string_literal: true

require '../src/day'

module Year2020
  class Day01 < Day
    def part1
      @input.lines.map(&:to_i).combination(2).find { |a, b| a + b == 2020 }.inject(:*)
    end

    def part2
      @input.lines.map(&:to_i).combination(3).find { |a, b, c| a + b + c == 2020 }.inject(:*)
    end
  end
end
