# frozen_string_literal: true

require_relative '../../src/day'

require 'digest'

module Year2015
  class Day05 < Day
    def enough_vowels?(string)
      string.count('aeiou') > 2
    end

    def double?(string)
      string.chars.chunk_while { |i, j| i == j }.to_a.any? { |chunk| chunk.size > 1 }
    end

    def illegal_strings?(string)
      %w[ab cd pq xy].none? { |sequence| string.include?(sequence) }
    end

    def double_twice?(string)
      string.scan(/.{2}/)
    end

    def letter_sandwich?(string)
    end

    def nice1?(string)
      enough_vowels?(string) && double?(string) && illegal_strings?(string)
    end

    def nice2?(string)
      double_twice?(string) && letter_sandwich?(string)
    end

    def part1
      @input.lines.select { |line| nice1?(line) }.count
    end

    def part2
      @input.lines.select { |line| nice2?(line) }.count
    end
  end
end
