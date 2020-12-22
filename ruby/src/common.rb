# frozen_string_literal: true

# Generic Day class
class Day
  def initialize
    day = self.class.name.chars.last(2).join
    year = self.class.name.split('::').first.chars.last(4).join
    file = File.open("../../input/#{year}/day_#{day}")
    @input = file.read
  end
end
