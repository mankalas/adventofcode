# frozen_string_literal: true

require "digest"

module AoC2015
  class Day04
    def part1(input)
      mine(input, 5)
    end

    def part2(input)
      mine(input, 6)
    end

    private

    def mine(s, zero_count)
      (1..).each do |i|
        return i if Digest::MD5.hexdigest("#{s}#{i}")[0..(zero_count - 1)].chars.uniq == ["0"]
      end
    end
  end
end
