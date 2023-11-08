# frozen_string_literal: true

module AoC2015
  class Day08
    def part1(input)
      sum_lines(input) { |line| code_chars(line) - memory_chars(line) }
    end

    def part2(input)
      sum_lines(input) { |line| encoded_chars(line) - code_chars(line) }
    end

    private

    def sum_lines(input, &block)
      input.lines.map(&:strip).sum(&block)
    end

    def code_chars(s)
      s.chars.count
    end

    # rubocop:disable Security/Eval
    def memory_chars(s)
      eval(s).chars.count
    end
    # rubocop:enable Security/Eval

    def encoded_chars(s)
      s.inspect.chars.count
    end
  end
end
