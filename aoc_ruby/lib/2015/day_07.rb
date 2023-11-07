# frozen_string_literal: true

require_relative "../parser"
require_relative "../utils"

module AoC2015
  class Day07
    def part1(input)
      run(input)
    end

    def part2(input)
      run(input, { "b" => run(input) })
    end

    private

    def run(input, signals = {})
      input
        .lines
        .map { |line| Parser.new.parse(line) }
        .map { |tree| Transform.new.apply(tree) }
        .tap { |instructions| record_wire_signal("a", instructions, signals) }
      signals["a"]
    rescue Parslet::ParseFailed => e
      puts e.parse_failure_cause.ascii_tree
    end

    def fetch_wire_signal(wire, instructions, signals)
      signals.fetch(wire) { record_wire_signal(wire, instructions, signals) }
    end

    def record_wire_signal(wire, instructions, signals)
      source = find_instruction(wire, instructions)
               .fetch(:source)
      signals[wire] = compute_wire_signal(source, instructions, signals)
    end

    def compute_wire_signal(source, instructions, signals)
      return source if source.is_a? Integer

      return fetch_wire_signal(source, instructions, signals) if source.is_a? String

      return execute_gate(source[:gate], instructions, signals) if source.key?(:gate)

      record_wire_signal(source[:wire], instructions, signals)
    end

    def execute_gate(gate, instructions, signals)
      if gate.key?(:left)
        left = compute_wire_signal(gate.fetch(:left), instructions, signals)
        right = compute_wire_signal(gate.fetch(:right), instructions, signals)
        compute_binary_gate(left, gate.fetch(:op), right)
      elsif gate.key?(:value)
        Utils.bin_not(compute_wire_signal(gate.fetch(:value), instructions, signals))
      end
    end

    def compute_binary_gate(left, op, right)
      case op
      when :and
        left & right
      when :or
        left | right
      when :rshift
        left >> right
      when :lshift
        left << right
      end
    end

    def find_instruction(wire, instructions)
      instructions.find { |ins| ins[:wire] == wire }
    end

    class Parser < AoC::Parser
      rule(:wire) { match("[a-z]").repeat(1) }
      rule(:gate) { binary_operation | unary_operation }
      rule(:source) { gate.as(:gate) | wire.as(:wire) | integer.as(:integer) }
      rule(:operand) { integer.as(:integer) | wire.as(:wire) }
      rule(:binary_operator) { str("AND") | str("OR") | str("LSHIFT") | str("RSHIFT") }
      rule(:unary_operator) { str("NOT") }
      rule(:binary_operation) do
        operand.as(:left) >> space? >>
          binary_operator.as(:op) >> space? >>
          operand.as(:right)
      end
      rule(:unary_operation) { unary_operator.as(:op) >> space? >> operand.as(:value) }
      rule(:instruction) { source.as(:source) >> space >> str("->") >> space >> wire.as(:wire) >> space? }

      root(:instruction)
    end

    class Transform < AoC::Transform
      rule(integer: simple(:i)) { i.to_i }
      rule(wire: simple(:v)) { v.to_s }
      rule(op: simple(:op)) { op.to_s.downcase.to_sym }
      rule(left: subtree(:left), op: simple(:op), right: subtree(:right)) do
        { left: left, op: op.to_s.downcase.to_sym, right: right }
      end
    end
  end
end
