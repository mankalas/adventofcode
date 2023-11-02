# frozen_string_literal: true

require "utils"

module AoC2015
  class Day02
    def part1(input)
      input.lines.inject(0) do |surface, dimensions|
        lwh = parse_dimensions(dimensions)
        surface + wrapping(lwh) + slack(lwh)
      end
    end

    def part2(input)
      input.lines.inject(0) do |length, dimensions|
        lwh = parse_dimensions(dimensions)
        length + shortest_distance(lwh) + product(lwh)
      end
    end

    private

    def product(terms)
      terms.inject(1) { |prod, i| prod * i }
    end

    def parse_dimensions(line)
      line.split("x").map(&:to_i)
    end

    def wrapping(dimensions)
      dimensions.combination(2).inject(0) { |sum, terms| sum + 2 * product(terms) }
    end

    def slack(dimensions)
      dimensions.combination(2).map { |a, b| a * b }.min
    end

    def shortest_distance(dimensions)
      dimensions.min(2).map { |d| d * 2 }.sum
    end
  end
end
