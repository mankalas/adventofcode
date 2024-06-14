# frozen_string_literal: false

module AoC2015
  class Day17
    def part1(input)
      matching_containers(parse_containers(input)).size
    end

    def part2(input)
      matching = matching_containers(parse_containers(input))
      minimum_size = matching.min_by(&:size).size
      matching.filter { |c| c.size == minimum_size }.size
    end

    private

    def matching_containers(cs)
      (2..cs.size - 1)
        .flat_map { |size| cs.combination(size).to_a }
        .filter { |c| c.sum == EGGNOG }
    end

    def parse_containers(input)
      input.lines.map(&:to_i)
    end

    EGGNOG = 150
  end
end
