# frozen_string_literal: true

module AoC2015
  class Day05
    def part1(input)
      input.lines.filter { |s| nice1?(s) }.count
    end

    def part2(input)
      input.lines.filter { |s| nice2?(s) }.count
    end

    private

    def nice1?(s)
      three_vowels?(s) && one_letter_twice?(s) && !naughty_substrings?(s)
    end

    def nice2?(s)
      pair_twice?(s) && letter_sandwich?(s)
    end

    def three_vowels?(s)
      s.count("aeiou") > 2
    end

    def one_letter_twice?(s)
      s.chars.chunk_while { |i, j| i == j }.to_a.any? { |chunk| chunk.size > 1 }
    end

    def naughty_substrings?(s)
      %w[ab cd pq xy].any? { |sequence| s.include?(sequence) }
    end

    def pair_twice?(s)
      return false unless s

      s[2..]&.include?(s[0..1]) || pair_twice?(s[1..])
    end

    def letter_sandwich?(s)
      s
        .chars
        .each_with_index
        .any? { |c, i| c == s[i + 2] }
    end
  end
end
