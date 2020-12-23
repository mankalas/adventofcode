# frozen_string_literal: true

require_relative '../../src/day'
require_relative '../../src/navigation'

require 'set'

module Year2015
  class Day03 < Day
    class Santa < Navigation::Cursor
      attr_reader :visited_cells

      def initialize(grid:)
        super
        @visited_cells = Set[grid[0, 0]]
      end

      def on_cell_enter
        visited_cells.add(cell)
      end

      def value
        visited_cells.count
      end
    end

    def part1
      grid = Navigation::Grid.new
      santa = Santa.new(grid: grid)
      santa.navigate(@input)
      santa.value
    end

    def part2
      grid = Navigation::Grid.new
      santa = Santa.new(grid: grid)
      robo_santa = Santa.new(grid: grid)
      santa_input, robo_santa_input = split_map
      santa.navigate(santa_input.join)
      robo_santa.navigate(robo_santa_input.join)
      [santa, robo_santa].map(&:visited_cells).reduce(:|).count
    end

    private

    def split_map
      [[], []].tap do |a, b|
        map = @input.chars
        while map.any?
          a << map.shift
          b << map.shift
        end
      end
    end
  end
end
