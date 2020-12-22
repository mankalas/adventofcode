# frozen_string_literal: true

require_relative '../src/file_reflection'

# Generic Day Test Suite
module TestDay
  include FileReflection

  def test_case_one
    assert_equal(answer(1), tested_class.new.part1)
  end

  def test_case_two
    assert_equal(answer(2), tested_class.new.part2)
  end

  private

  def tested_class
    Object.const_get("Year#{year}::Day#{day}")
  end

  def answer(part)
    File.open("../../output/#{year}/day_#{day}_part_#{part}").read.strip.to_i
  end
end
