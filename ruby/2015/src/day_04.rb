# frozen_string_literal: true

require_relative '../../src/day'

require 'digest'

module Year2015
  class Day04 < Day
    def part1
      1.step do |i|
        return i if Digest::MD5.hexdigest("#{@input}#{i}")[0..4] == '00000'
      end
    end

    def part2
      1.step do |i|
        return i if Digest::MD5.hexdigest("#{@input}#{i}")[0..5] == '000000'
      end
    end
  end
end
