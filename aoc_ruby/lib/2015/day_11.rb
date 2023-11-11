# frozen_string_literal: false

module AoC2015
  class Day11
    def part1(password)
      password = quick_succ(password)
      password = quick_succ(password) until secure?(password)
      password
    end

    def part2(password)
      password = quick_succ(password)
      password = quick_succ(password) until secure?(password)
      password = quick_succ(password)
      password = quick_succ(password) until secure?(password)
      password
    end

    private

    def quick_succ(s)
      i = s.chop.chars.find_index { |c| %w[i o l].include?(c) }
      return s.succ unless i

      s[i] = s[i].succ
      (i + 1).upto(s.length - 1) { |j| s[j] = "a" } unless i + 1 >= s.length
      s
    end

    def secure?(s)
      increasing_triplet?(s) && !any_confusing_char?(s) && two_pairs?(s)
    end

    def increasing_triplet?(s)
      s.chars
        .chunk_while { |i, j| j == i.succ }
        .any? { |chunk| chunk.size > 2 }
    end

    def any_confusing_char?(s)
      s.match?(/\A.*[iol].*\z/)
    end

    def two_pairs?(s)
      s.chars
        .chunk_while { |i, j| j == i }
        .filter { |chunk| chunk.size > 1 }
        .count > 1
    end
  end
end
