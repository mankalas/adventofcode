# frozen_string_literal: true

class Utils
  def self.input(klass)
    File.read(root_dir(klass, "input"))
  end

  def self.output_part1(klass)
    output_dir(klass, 1)
  end

  def self.output_part2(klass)
    output_dir(klass, 2)
  end

  def self.day_dir(klass)
    mod, kls = klass.name.split("::")
    "#{mod[-4..]}/day_#{kls[-2..]}"
  end

  def self.root_dir(klass, root)
    "../#{root}/#{day_dir(klass)}"
  end

  def self.output_dir(klass, part)
    File.read("#{root_dir(klass, "output")}_part_#{part}")
  end
end
