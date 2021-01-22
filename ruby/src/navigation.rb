# frozen_string_literal: true

module Navigation
  class Coord
    attr_reader :x, :y

    def initialize(x = 0, y = 0)
      @x = x.to_i
      @y = y.to_i
    end

    def to_s
      "(x: #{x}, y: #{y})"
    end

    def upto(dest)
      return [] if dest.x < x || dest.y < y

      (x..dest.x).map do |dx|
        (y..dest.y).map do |dy|
          Coord.new(dx, dy)
        end
      end.flatten
    end

    def ==(other)
      x == other.x && y == other.y
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
      on_cell_enter
    end

    def position=(coord)
      self.position = coord
      on_cell_enter
    end

    private

    attr_writer :value
    attr_reader :grid

    def on_cell_enter; end
  end

  class Grid
    class OutOfBounds < StandardError; end

    def initialize(size: nil, default_cell_value: 0)
      @size = size
      @grid = Hash.new do |hash, key|
        x, y = key
        hash[key] = Square.new(grid: self,
                               value: default_cell_value,
                               coord: Coord.new(x, y))
      end
    end

    def [](x, y)
      if size
        raise OutOfBounds, "Access #{x} outside grid's width #{size.x}" unless x < size.x
        raise OutOfBounds, "Access #{y} outside grid's height #{size.y}" unless y < size.y
      end

      grid[[x, y]]
    end

    def at(position)
      self[position.x, position.y]
    end

    def count
      grid.count
    end

    def cells
      @grid.values
    end

    def rectangle(origin, destination)
      min_x, max_x = [origin.x, destination.x].minmax
      min_y, max_y = [origin.y, destination.y].minmax

      (min_x..max_x).map do |x|
        (min_y..max_y).map do |y|
          self[x, y]
        end
      end.flatten
    end

    private

    attr_reader :grid, :size
  end
end
