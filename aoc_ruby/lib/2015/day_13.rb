# frozen_string_literal: false

module AoC2015
  class Day13
    def part1(input)
      optimal(generate_map(input, :happiness_map))
    end

    def part2(input)
      optimal(generate_map(input, :happiness_map2))
    end

    private

    def parse(line)
      line.split(/(.+) would (.+) (\d+) happiness units by sitting next to (.+)./).drop(1)
    end

    def generate_map(input, map)
      input
        .lines
        .map { |line| parse(line) }
        .inject({}) { |h, (a, direction, amount, b)| send(map, h, a, direction, amount, b) }
    end

    def happiness_units(direction, amount)
      amount.to_i * (direction == "gain" ? 1 : -1)
    end

    def happiness_map(h, a, direction, amount, b)
      h.merge!({ [a, b] => happiness_units(direction, amount) })
    end

    def happiness_map2(h, a, direction, amount, b)
      happiness_map(h, a, direction, amount, b)
      h.merge!(happiness_with_me(a, b))
    end

    def happiness_with_me(a, b)
      [a, b]
        .inject([]) { |acc, guest| acc + ["Me", guest].permutation.to_a }
        .map { |pair| [pair, 0] }
        .to_h
    end

    def arrangement_score(arrangement, happiness_map)
      arrangement.count.times.sum do
        a, b = arrangement[0..1]
        arrangement.rotate!
        happiness_map[[a, b]] + happiness_map[[b, a]]
      end
    end

    def optimal(happiness_map)
      happiness_map
        .keys.flatten.uniq.permutation
        .each_with_object({}) do |arrangement, scores|
        scores.merge!({ arrangement => arrangement_score(arrangement, happiness_map) })
      end
        .values.max
    end
  end
end
