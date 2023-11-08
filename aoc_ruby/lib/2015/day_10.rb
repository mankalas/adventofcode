# frozen_string_literal: true

module AoC2015
  class Day10
    def part1(input)
      compute(input, 40)
    end

    def part2(input)
      compute(input, 50)
    end

    private

    def compute(input, n)
      n.times { input = say(input) }
      input.chars.count
    end

    def say(s)
      s.chars.chunk_while { |i, j| i == j }.map { |run| "#{run.count}#{run.uniq.first}" }.join
    end
  end
end
