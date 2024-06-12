# frozen_string_literal: false

module AoC2015
  class Day16
    def part1(input)
      sues = run(input)
      sues.key(one_sue(sues, &method(:good_sue1?)))
    end

    def part2(input)
      sues = run(input)
      sues.key(one_sue(sues, &method(:good_sue2?)))
    end

    private

    def run(input)
      input
        .lines
        .each_with_index
        .map { |line, i| [i + 1, parse(line)] }
        .to_h
    end

    def one_sue(sues, &predicate)
      sues
        .values
        .filter { |sue| predicate.call(sue) }
        .tap { |result| raise unless result.one? }
        .first
    end

    def good_sue1?(sue)
      sue
        .reject { |_, v| v.nil? }
        .all? { |k, v| v == COMPOUNDS[k] }
    end

    def good_sue2?(sue)
      sue
        .reject { |_, v| v.nil? }
        .all? { |k, v| valid_compound?(k, v) }
    end

    def valid_compound?(compound, value)
      if %i[cats trees].include?(compound)
        value > COMPOUNDS[compound]
      elsif %i[pomeranians goldfish].include?(compound)
        value < COMPOUNDS[compound]
      else
        value == COMPOUNDS[compound]
      end
    end

    def parse(line)
      line
        .sub(/Sue \d+: /, "")
        .split(", ")
        .map { |p| p.split(": ") }
        .inject({}) { |hash, pair| hash.merge({ pair[0].to_sym => pair[1].to_i }) }
    end

    COMPOUNDS = {
      children: 3,
      cats: 7,
      samoyeds: 2,
      pomeranians: 3,
      akitas: 0,
      vizslas: 0,
      goldfish: 5,
      trees: 3,
      cars: 2,
      perfumes: 1
    }.freeze
  end
end
