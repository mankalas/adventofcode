# frozen_string_literal: false

module AoC2015
  class Day19
    def part1(input)
      replacements, molecule = parse(input)

      parts = molecule.scan(Regexp.union(replacements.keys.uniq))

      results = parts.uniq.sort.map do |part|
        indices = find_indices(molecule, part).sort
        new_atoms = replacements[part]
        indices.map do |i|
          new_atoms.map do |new_atom|
            if i.zero?
              new_atom + molecule[1..]
            else
              molecule[0..i - 1] + new_atom + molecule[i + part.length..]
            end
          end
        end.flatten
      end.flatten

      results.uniq.count
      # 674 too high
      # 558 too low
    end

    def part2(input)
    end

    private

    def find_indices(source, pattern, offset = 0)
      return [] if source.nil? || source.empty?

      idx = source.index(pattern)
      return [] unless idx

      next_index = idx + offset + 1
      [idx + offset] + find_indices(source[idx + 1..], pattern, next_index)
    end

    def parse(input)
      replacements = Hash.new { |hash, key| hash[key] = [] }
      molecule = nil
      input
        .lines
        .each do |line|
        if (m = line.match(/([a-zA-Z]+) => ([a-zA-Z]+)/))
          replacements[m[1]].append(m[2])
        elsif (m = line.match(/([a-zA-Z0-9]+)/))
          molecule = m[0]
        end
      end
      [replacements, molecule]
    end

    def split(tokens, string)
      head = tokens.find { |token| string.start_with?(token) }
      return [nil, string] unless head
      [head, string[head.length..]]
    end
  end
end
