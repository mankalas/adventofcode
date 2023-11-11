# frozen_string_literal: false

require "json"

module AoC2015
  class Day12
    def part1(input)
      input.scan(/-?\d+/).map(&:to_i).sum
    end

    def part2(input)
      sum(JSON.parse(input))
    end

    private

    def sum(v)
      case v
      when Integer
        v
      when Hash
        sum_hash(v)
      when Array
        sum_values(v.flatten)
      else
        0
      end
    end

    def sum_hash(h)
      return 0 if h.values.filter { |v| v == "red" }.any?

      sum_values(h.values)
    end

    def sum_values(values)
      values.inject(0) { |sum, v| sum + sum(v) }
    end
  end
end
