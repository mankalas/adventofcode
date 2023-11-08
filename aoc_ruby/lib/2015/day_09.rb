# frozen_string_literal: true

module AoC2015
  class Day09
    def part1(input)
      find(input, :min)
    end

    def part2(input)
      find(input, :max)
    end

    private

    def find(input, f)
      distance_map = distance_map(input)
      routes = distance_map.keys.flatten.uniq.permutation
      distances = routes.map { |route| route_distance(route, distance_map) }
      distances.send(f)
    end

    def route_distance(route, distance_map)
      route.each_cons(2).sum { |path| distance_map[path.sort] }
    end

    def distance_map(input)
      input
        .lines
        .map { |line| line.split(/(.+) to (.+) = (\d+)/).drop(1) }
        .each_with_object({}) { |(a, b, d), m| m[[a, b].sort] = d.to_i }
    end
  end
end
