# frozen_string_literal: true

require '../src/common'

module Year2020
  class Day01 < Day
    def part1
      @input.lines.map(&:to_i).combination(2).find { |a, b| a + b == 2020}.inject(:*)
    end

    def part2
      @input.lines.map(&:to_i).combination(3).find { |a, b, c| a + b + c == 2020}.inject(:*)
    end
  end

  require 'test/unit'

  class MyTest < Test::Unit::TestCase
    def test_true
      assert_equal(876459, Day01.new.part1)
      assert_equal(116168640, Day01.new.part2)
    end
  end
end
