# frozen_string_literal: true

module Navigation
  class Coord
    attr_reader :x, :y

    def initialize(x = 0, y = 0)
      @x = x
      @y = y
    end

    def to_s
      "(x: #{x}, y: #{y})"
    end
  end

  class Cell
    attr_accessor :value, :num_edges

    def initialize(grid:, num_edges:, coord: Coord.new, value: 0)
      @grid = grid
      @num_edges = num_edges
      @coord = coord
      @value = value
    end
  end

  class Square < Cell
    def initialize(grid:, coord: Coord.new, value: 0)
      super(grid: grid, coord: coord, num_edges: 4, value: value)
    end

    def north; @grid[x, y + 1] end

    def east;  @grid[x + 1, y] end

    def south; @grid[x, y - 1] end

    def west;  @grid[x - 1, y] end

    def x; @coord.x end

    def y; @coord.y end
  end

  class Cursor
    attr_reader :value, :position

    def initialize(grid:, pos: Coord.new, value: 0)
      @grid = grid
      @position = pos
      @start_position = position
      @value = value
    end

    def navigate(map, nesw: '^>v<')
      map.chars do |step|
        i = nesw.index(step)
        raise "'#{step}' isn't a valid direction" unless i

        go(%i[north east south west][i])
      end
    end

    def cell
      grid.at(position)
    end

    def go(direction)
      @position = grid.at(position).send(direction)
    end

    private

    attr_writer :value
    attr_reader :grid
  end

  class Grid
    def initialize(size: nil, default_cell_value: 0)
      @size = size
      @grid = Hash.new do |hash, key|
        x, y = key
        hash[key] = Square.new(grid: self,
                               value: default_cell_value,
                               coord: Coord.new(x, y))
      end
      populate
    end

    def [](x, y)
      if size
        raise "Access #{x} outside grid's width #{size.x}" unless x < size.x
        raise "Access #{y} outside grid's height #{size.y}" unless y < size.y
      end

      grid[[x, y]]
    end

    def at(position)
      self[position.x, position.y]
    end

    def count
      grid.count
    end

    def rectangle(origin, destination)
    end

    private

    attr_reader :grid, :size

    def populate
      return unless size

      size.x.times do |w|
        size.y.times do |h|
          self[w, h]
        end
      end
    end
  end
end
