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
      %w[ab cd pq xy].any? { |sequence| string.include?(sequence) }
    end

    def pair_twice?(s)
      return false unless s

      cc = s[0..1]
      s2 = s[2..]
      s2&.include?(cc) || pair_twice?(s[1..])
    end

    def letter_sandwich?(string)
      string
        .chars
        .each_with_index
        .any? { |c, i| c == string[i + 2] }
    end

    def nice1?(string)
      enough_vowels?(string) && double?(string) && !illegal_strings?(string)
    end

    def nice2?(string)
      pair_twice?(string) && letter_sandwich?(string)
    end

    def part1
      @input.lines.select { |line| nice1?(line) }.count
    end

    def part2
      @input.lines.select { |line| nice2?(line) }.count
    end
  end
end
