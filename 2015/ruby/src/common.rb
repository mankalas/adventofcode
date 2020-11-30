class Day
  def initialize
    day = self.class.name.chars.last(2).join
    today = File.basename("Day#{day}", ".*")
    file = File.open("../../test/#{today}.input")
    @input = file.read
  end
end
