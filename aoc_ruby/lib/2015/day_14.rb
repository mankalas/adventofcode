# frozen_string_literal: false

module AoC2015
  class Day14
    def part1(input)
      winner(generate_map(input))
    end

    def part2(input)
      champion(generate_map(input))
    end

    private

    def parse(line)
      line.split(%r{(.+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.\n}).drop(1)
    end

    def generate_map(input)
      input
        .lines
        .map { |line| parse(line) }
        .inject({}) { |h, variables| speed_map(h, *variables) }
    end

    def speed_map(h, reindeer, speed, speed_duration, rest_duration)
      h.merge!({ reindeer => speed_cycle(speed, speed_duration, rest_duration) })
    end

    def speed_cycle(speed, speed_duration, rest_duration)
      first_speed_cycle(speed, speed_duration, rest_duration).cycle.take(LIMIT)
    end

    def first_speed_cycle(speed, speed_duration, rest_duration)
      speed_array(speed_duration, speed) + speed_array(rest_duration, 0)
    end

    def speed_array(duration, value)
      Array.new(duration.to_i) { value.to_i }
    end

    def winner(speed_map)
      speed_map.values.map(&:sum).max
    end

    def champion(speed_map)
      distance_map = distance_map(speed_map)
      scores = scores_at_step(distance_map)
      aggregate_scores(scores).values.max
    end

    def aggregate_scores(scores)
      scores.reduce({}) do |results, score_at_step|
        results.merge(score_at_step) { |_, old, new| old + new }
      end
    end

    def scores_at_step(distance_map)
      LIMIT.times.map do |step|
        distances_at_step = distance_map.transform_values { |d| d[step] }
        distance_map.map { |r, _| [r, champions_at_step(distances_at_step).include?(r) ? 1 : 0] }.to_h
      end
    end

    def distance_map(speed_map)
      speed_map.transform_values do |speeds|
        speeds.each_with_index.map { |_, i| speeds[0..i].sum }
      end
    end

    def champions_at_step(distances_at_step)
      max = distances_at_step.values.max
      distances_at_step.select { |_, v| v == max }.keys
    end

    LIMIT = 2503
  end
end
