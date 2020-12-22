# frozen_string_literal: true

require_relative '../../src/day'

module Year2015
  class Day01 < Day
    def part1
      @input.count('(') - @input.count(')')
    end

    def part2
      floor = 0
      @input.each_char.with_index do |c, i|
        floor += c == '(' ? 1 : -1
        return i + 1 if floor.negative?
      end
    end
  end
end
