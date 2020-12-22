# frozen_string_literal: true

# Generic Day class
class Day
  include FileReflection

  def initialize
    file = File.open("../../input/#{year}/day_#{day}")
    @input = file.read
  end
end
