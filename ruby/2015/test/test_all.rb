# frozen_string_literal: true

require 'test/unit'

module Year2015
  class TestYear2015 < Test::Unit::TestCase
    def self.file_count
      Dir.glob(File.join('src', '*')).select { |file| File.file?(file) }.count
    end

    file_count.times do |i_day|
      s_day = format('%02d', i_day + 1)
      require_relative "../src/day_#{s_day}"
      define_method(:"test_day_#{s_day}") do
        tested_class = Object.const_get("Year2015::Day#{s_day}")

        assert_equal(answer(2015, s_day, 1), tested_class.new.part1)
        assert_equal(answer(2015, s_day, 2), tested_class.new.part2)
      end
    end

    private

    def answer(year, day, part)
      File.open("../../output/#{year}/day_#{day}_part_#{part}").read.strip.to_i
    end
  end
end
