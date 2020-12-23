# frozen_string_literal: true

require 'test/unit'
require_relative '../src/navigation'

module Navigation
  class TestCoord < Test::Unit::TestCase
    def test_init
      c = Coord.new
      assert_equal(c.x, 0)
      assert_equal(c.y, 0)
    end
  end

  class TestGrid < Test::Unit::TestCase
    def test_init
      assert_not_nil(Grid.new)
    end

    def test_new_cell
      g = Grid.new(default_cell_value: :dummy)
      cell = g[1, 2]
      assert_equal(1, cell.x)
      assert_equal(2, cell.y)
      assert_equal(:dummy, cell.value)
    end
  end

  class TestSquare < Test::Unit::TestCase
    def test_directions
      g = Grid.new
      x = g[0, 0]
      n = g[0, 1]
      e = g[1, 0]
      s = g[0, -1]
      w = g[-1, 0]
      assert_equal(4, x.num_edges)
      assert_equal(n, x.north)
      assert_equal(e, x.east)
      assert_equal(s, x.south)
      assert_equal(w, x.west)
    end
  end

  class TestCursor < Test::Unit::TestCase
    def grid
      @grid ||= Grid.new
    end

    def cursor
      @cursor ||= Cursor.new(grid: grid)
    end

    def test_init
      assert_equal(grid[0, 0], cursor.cell)
    end

    def test_circle
      map = '^>v<'
      cursor.navigate(map)
      assert_equal(grid[0, 0], cursor.cell)
    end

    def test_go_north
      map = '^'
      cursor.navigate(map)
      assert_equal(grid[0, 1], cursor.cell)
    end

    def test_go_east
      map = '>'
      cursor.navigate(map)
      assert_equal(grid[1, 0], cursor.cell)
    end

    def test_go_south
      map = 'v'
      cursor.navigate(map)
      assert_equal(grid[0, -1], cursor.cell)
    end

    def test_go_west
      map = '<'
      cursor.navigate(map)
      assert_equal(grid[-1, 0], cursor.cell)
    end

    def test_nesw
      # Another 'alphabet'
      map = 'NEESW'
      cursor.navigate(map, nesw: 'NESW')
      assert_equal(grid[1, 0], cursor.cell)
    end

    def test_cell_enter_called
      map = '>'
      cursor.on_cell_enter = ->(cursor) { cursor.increment }
      assert_equal(0, cursor.value)
      cursor.navigate(map)
      assert_equal(1, cursor.value)
    end
  end
end
