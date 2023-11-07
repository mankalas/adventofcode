# frozen_string_literal: true

require "parslet"

module AoC2015
  class Day06
    def part1(input)
      input
        .lines
        .map { |line| parse(line) }
        .inject(Hash.new(false)) { |map, instruction| follow_instruction(instruction, map, :switch) }
        .values
        .filter { |v| v }
        .count
    end

    def part2(input)
      input
        .lines
        .map { |line| parse(line) }
        .inject(Hash.new(0)) { |map, instruction| follow_instruction(instruction, map, :brightness) }
        .values
        .sum
    end

    private

    def brightness(value, action)
      [value + { toggle: 2, turn_on: 1, turn_off: -1 }[action], 0].max
    end

    def switch(value, action)
      case action
      when :toggle
        !value
      when :turn_on
        true
      when :turn_off
        false
      end
    end

    def follow_instruction(instruction, map, modifier)
      rectangle(instruction.start, instruction.finish)
        .each { |cell| map[cell] = send(modifier, map[cell], instruction.action) }
      map
    end

    def rectangle(origin, destination)
      min_x, max_x = [origin.x, destination.x].minmax
      min_y, max_y = [origin.y, destination.y].minmax

      (min_x..max_x).map do |x|
        (min_y..max_y).map do |y|
          Coord.new(x, y)
        end
      end
        .flatten
    end

    def parse(line)
      Transform.new.apply(Parser.new.parse(line))
    rescue Parslet::ParseFailed => e
      puts e.parse_failure_cause.ascii_tree
    end

    Coord = Struct.new(:x, :y)
    Instruction = Struct.new(:action, :start, :finish)

    class Parser < Parslet::Parser
      rule(:space)      { match('\s').repeat(1) }
      rule(:space?)     { space.maybe }
      rule(:integer) { match("[0-9]").repeat(1) }
      rule(:coord) { integer.as(:x) >> str(",") >> integer.as(:y) }
      rule(:action) { str("toggle") | str("turn off") | str("turn on") }

      rule(:instruction) do
        action.as(:action) >>
          str(" ") >>
          coord.as(:start) >>
          str(" through ") >>
          coord.as(:finish) >>
          space?
      end
      root(:instruction)
    end

    class Transform < Parser::Transform
      rule(action: simple(:action), start: subtree(:start), finish: subtree(:finish)) do
        Instruction.new(action.to_s.sub(" ", "_").to_sym, start, finish)
      end

      rule(x: simple(:x), y: simple(:y)) { Coord.new(x.to_i, y.to_i) }
    end
  end
end
