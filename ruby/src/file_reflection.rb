# frozen_string_literal: true

module FileReflection
  def day
    self.class.name.chars.last(2).join
  end

  def year
    self.class.name.split('::').first.chars.last(4).join
  end
end
