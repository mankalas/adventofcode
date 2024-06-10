# frozen_string_literal: false

module AoC2015
  class Day15
    def part1(input)
      run(input, &method(:cookie))
    end

    def part2(input)
      run(input, &method(:light_cookie))
    end

    private

    def run(input, &score)
      input
        .lines
        .map { |line| parse(line) }
        .map { |ingredients| ingredients.map(&:to_i) }
        .yield_self do |ingredients|
          amounts
            .map { |amount| score.call(amount, ingredients) }
            .max
        end
    end

    def parse(line)
      line.split(/.+: capacity (-?\d), durability (-?\d), flavor (-?\d), texture (-?\d), calories (-?\d)\n/).drop(1)
    end

    def amounts
      return @combinations unless @combinations.nil?

      @combinations = []
      100.times do |a|
        (100-a).times do |b|
          (100 - a - b).times do |c|
            @combinations << [a, b, c, 100 - a - b - c]
          end
        end
      end
      @combinations
    end

    def cookie(amount, ingredients)
      ingredients
        .map { |e| e.slice(..-2) } # Array of 4 arrays of 4 int (4 ingr with 4 prop)
        .transpose # Array of 5 arrays of 4 int (4 prop across 4 ingr)
        .map { |e| e.zip(amount) } # Each prop maps to the given amount
        .map { |e| e.map { |x, y| x * y }.sum } # Run the math
        .map { |i| i.negative? ? 0 : i }
        .inject(:*)
    end

    def light_cookie(amount, ingredients)
      ingredients # Array of 4 arrays of 5 int (4 ingr with 5 prop)
        .transpose # Array of 5 arrays of 4 int (5 prop across 4 ingr)
        .map { |e| e.zip(amount) } # Each prop maps to the given amount
        .map { |e| e.map { |x, y| x * y }.sum } # Run the math
        .tap { |e| return 0 if e[4] != 500 }
        .map { |i| i.negative? ? 0 : i }
        .slice(..-2)
        .inject(:*)
    end

    PROPERTIES_COUNT = 4
    MAX_CALORIES = 500
  end
end
