# frozen_string_literal: true

require "utils"

module AoC2015
  class Day01
    def part1(input)
      input.chars.inject(0) { |sum, c| sum + paren_to_int(c) }
    end

    def part2(input)
      input.chars.each_with_index.inject(0) do |sum, (c, i)|
        return i if sum.negative?

        sum + paren_to_int(c)
      end
    end

    private

    def paren_to_int(c)
      c == "(" ? 1 : -1
    end
  end
end
