# frozen_string_literal: true

module AoC2015
  class Day03
    def part1(input)
      count_visited_houses(visit_houses(input))
    end

    def part2(input)
      count_visited_houses(visit_houses_duo(input))
    end

    private

    def count_visited_houses(visits)
      visits.keys.count
    end

    def visit_houses_duo(input)
      unzip(input)
        .map { |santa| visit_houses(santa) }
        .inject({}) { |total, h| total.merge(h) }
    end

    def visit_houses(input)
      Hash.new(0).merge([0, 0] => 1).tap do |house_map|
        input.chars.inject([0, 0]) do |position, direction|
          visit(*position, direction: direction).tap do |new_position|
            house_map[new_position] += 1
          end
        end
      end
    end

    def visit(x, y, direction:)
      case direction
      when ">"
        [x + 1, y]
      when "^"
        [x, y + 1]
      when "v"
        [x, y - 1]
      when "<"
        [x - 1, y]
      end
    end

    def unzip(s)
      a = ""
      b = ""
      s.chars.each.with_index do |c, i|
        i.even? ? a += c : b += c
      end
      [a, b]
    end
  end
end
