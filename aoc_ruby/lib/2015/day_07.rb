# frozen_string_literal: true

require_relative "../parser"
require_relative "../utils"

module AoC2015
  class Day07
    def part1(input)
      run(input, {})
    end

    def part2(input)
      run(input, { "b" => 123123 })
    end

    private

    def run(input, signals)
      instructions = input
                       .lines
                       .map { |line| Parser.new.parse(line) }
                       .map { |tree| Transform.new.apply(tree) }
      execute("a", instructions, signals)
      signals["a"]
    rescue Parslet::ParseFailed => e
      puts e.parse_failure_cause.ascii_tree
    end

    def execute(wire, instructions, signals)
      instruction = find_instruction(wire, instructions)
      puts instruction
      source = instruction.fetch(:source)
      return signals.fetch("b") if wire == "b" and signals.key?("b") and source.is_a? Integer

      signals[wire] =
        if source.is_a? Integer
          source
        elsif source.is_a? String
          signals.fetch(source) { execute(source, instructions, signals) }
        elsif source.key?(:gate)
          execute_gate(source[:gate], instructions, signals)
        elsif source.key?(:wire)
          execute(source[:wire], instructions, signals)
        end
    end

    def execute_operand(operand, instructions, signals)
      return signals.fetch("b") if operand == "b" and signals.key?("b") and operand.is_a? Integer

      if operand.is_a? Integer
        operand
      elsif operand.is_a? String
        signals.fetch(operand) { execute(operand, instructions, signals) }
      end
    end

    def execute_gate(gate, instructions, signals)
      if gate.key?(:left)
        left = execute_operand(gate.fetch(:left), instructions, signals)
        right = execute_operand(gate.fetch(:right), instructions, signals)
        compute_binary_gate(left, gate.fetch(:op), right)
      elsif gate.key?(:value)
        compute_unary_gate(gate.fetch(:value), instructions, signals)
      end
    end

    def compute_unary_gate(value, instructions, signals)
      Utils.bin_not(execute_operand(value, instructions, signals))
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
      instructions.find { |ins| ins.fetch(:wire) == wire }
    end

    def compute(wire, instructions)
      instruction = find_instruction(wire, instructions)
      execute(instruction)
    end

    Instruction = Struct.new(:source, :destination)

    Wire = Struct.new(:value) do
      def to_i(signals)
        signals[value]
      end
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
