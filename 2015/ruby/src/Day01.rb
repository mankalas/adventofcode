require './common'

class Day01 < Day
  def part1
    return @input.count("(") - @input.count(")")
  end

  def part2
    floor = 0
    @input.each_char.with_index do |c, i|
      floor += c == '(' ? 1 : -1
      return i + 1 if floor.negative?
    end
  end
end

require 'test/unit'

class MyTest < Test::Unit::TestCase
  def test_true
    assert_equal(232, Day01.new.part1)
    assert_equal(1783, Day01.new.part2)
  end
end
