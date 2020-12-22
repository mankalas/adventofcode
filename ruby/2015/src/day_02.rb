# frozen_string_literal: true

require_relative '../../src/day'

module Year2015
  class Day02 < Day
    def part1
      each_lwh do |l, w, h|
        a = [l * w, l * h, w * h]
        a.map { |area| area * 2 }.sum + a.min
      end.sum
    end

    def part2
      each_lwh do |l, w, h|
        p = [2 * l + 2 * w, 2 * l + 2 * h, 2 * w + 2 * h]
        p.min + l * w * h
      end.sum
    end

    private

    def each_lwh
      @input.lines.map do |gift_dimension|
        yield gift_dimension.split('x').map(&:to_i)
      end
    end
  end
end
