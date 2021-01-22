# frozen_string_literal: true

require_relative '../../src/day'
require_relative '../../src/navigation'

require 'benchmark'

module Year2015
  class Day06 < Day
    def switch(cell, action)
      case action
      when "turn on"
        cell.value = true
      when "turn off"
        cell.value = false
      when "toggle"
        cell.value = !cell.value
      end
    end

    def brightness(cell, action)
      case action
      when "turn on"
        cell.value += 1
      when "turn off"
        cell.value -= 1 unless cell.value.zero?
      when "toggle"
        cell.value += 2
      end
    end

    def part1
      @grid = Navigation::Grid.new(size: Navigation::Coord.new(1000, 1000),
                                   default_cell_value: false)
      follow_instructions(:switch)
      @grid.cells.select(&:value).count
    end

    def part2
      @grid = Navigation::Grid.new(size: Navigation::Coord.new(1000, 1000),
                                   default_cell_value: 0)
      follow_instructions(:brightness)
      @grid.cells.sum(&:value)
    end

    private

    def follow_instructions(m)
      @input.lines.each do |line|
        action, src_x, src_y, dst_x, dst_y = parse(line)
        rectangle = @grid.rectangle(Navigation::Coord.new(src_x, src_y),
                                    Navigation::Coord.new(dst_x, dst_y))
        rectangle.each do |cell|
          send(m, cell, action)
        end
      end
    end

    def parse(line)
      line.split(/(.+) (\d+),(\d+) through (\d+),(\d+)/).drop(1)
    end
  end
end
