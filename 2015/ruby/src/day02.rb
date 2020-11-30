require './common'

class Day02 < Day
  def part1
    each_lwh do |l, w, h|
      a = [l * w, l * h, w * h]
      a.map { |area| area * 2 }.sum + a.min
    end.sum
  end

  def part2
    each_lwh do |l, w, h|
      p = [2 * l + 2 * w, 2 * l + 2 * h, 2 * w + 2 * h]
      p.min + l * w * h
    end.sum
  end

  private

  def each_lwh
    @input.split("\n").map do |gift_dimension|
      yield gift_dimension.split("x").map(&:to_i)
    end
  end
end

class TestDay02 < Test::Unit::TestCase
  def test_me
    @answer1 = 1586300
    @answer2 = 3737498
    @clazz = Day02
    assert_equal(@answer1, @clazz.new.part1)
    assert_equal(@answer2, @clazz.new.part2)
  end
end
