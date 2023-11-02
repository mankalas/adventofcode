# frozen_string_literal: true

require "parsere"

module AoC2015
  class Day07

    def part1(input)
    end

    def part2(input)
    end

    private

    class Parser < ::AoC::Parser
      rule(:wire) { match("[a-z]").repeat(1) }
      rule(:left) { wire.as(:wire) | integer.as(:int) }
      rule(:gate_and) { left.as(:left) >> space >> str("AND").as(:op) >> space >> wire.as(:right) }
      rule(:gate_or) { wire.as(:left) >> space >> str("OR").as(:op) >> space >> wire.as(:right) }
      rule(:gate_not) { str("NOT").as(:op) >> space >> wire.as(:value) }
      rule(:gate_lshift) { wire.as(:value) >> space >> str("LSHIFT").as(:op) >> space >> integer.as(:shift) }
      rule(:gate_rshift) { wire.as(:value) >> space >> str("RSHIFT").as(:op) >> space >> integer.as(:shift) }
      rule(:gate) { binary_operator | unary_operator }

      rule(:source) { gate.as(:gate) | wire.as(:wire) | integer.as(:int) }
      rule(:instruction) { source.as(:source) >> space >> str("->") >> space >> wire.as(:wire) }

      root(:instruction)
    end

    class Transform < Parser::Transform
      rule(action: simple(:action), start: subtree(:start), finish: subtree(:finish)) do
        Instruction.new(action.to_s.sub(" ", "_").to_sym, start, finish)
      end

      rule(x: simple(:x), y: simple(:y)) { Coord.new(x.to_i, y.to_i) }

      rule(signal: simple(:signal)) { signal.to_i }
      rule(wire: simple(:wire)) { wire.to_s }
    end
  end
end

begin
  puts Parser.new.parse("ce OR cd -> cf")
  puts Parser.new.parse("bn RSHIFT 2 -> bo")
  puts Parser.new.parse("bn LSHIFT 2 -> bo")
  puts Parser.new.parse("lx -> a")
  puts Parser.new.parse("NOT ax -> ay")
  puts Parser.new.parse("1 AND bh -> bi")
  puts Parser.new.parse("1 -> bi")
rescue Parslet::ParseFailed => e
  puts e.parse_failure_cause.ascii_tree
end
