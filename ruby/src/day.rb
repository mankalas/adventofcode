# frozen_string_literal: true

require_relative './file_reflection'

# Generic Day class
class Day
  include FileReflection

  def initialize
    file = File.open("../../input/#{year}/day_#{day}")
    @input = file.read.strip
  end
end
