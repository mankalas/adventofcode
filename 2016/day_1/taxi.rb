class Direction
  NORTH = 0
  EAST = 1
  SOUTH = 2
  WEST = 3

  attr_reader :cardinal

  def initialize
    @cardinal = NORTH
  end

  def rotate!(code)
    shift = (code == 'L' ? -1 : 1)
    @cardinal = (@cardinal + shift) % 4
  end
end

class Position
  attr_accessor :x, :y
  attr_reader :twice_visited

  def initialize
    @x, @y = 0, 0
    @paths = []
    @twice_visited = nil
  end

  def incr_x
    @x += 1
  end

  def decr_x
    @x -= 1
  end

  def incr_y
    @y += 1
  end

  def decr_y
    @y -= 1
  end

  def forward!(direction:, length:)
    case direction.cardinal
    when Direction::NORTH
      method = :incr_y
    when Direction::SOUTH
      method = :decr_y
    when Direction::EAST
      method = :incr_x
    when Direction::WEST
      method = :decr_x
    end
    length.times do
      send(method)
      if @paths.include?([@x, @y])
        @twice_visited = [@x, @y]
      else
        @paths << [@x, @y]
      end unless @twice_visited
    end
  end

  def to_s
    "#{x}, #{y}"
  end
end

class Taxi
  attr_reader :position

  def initialize
    @direction = Direction.new
    @position = Position.new
  end

  def read_sequence!(sequence)
    steps = sequence.split(',')
    steps.each do |step|
      step.strip!
      rotate!(step[0])
      move!(step[1..-1].to_i)
    end
  end

  def rotate!(code)
    @direction.rotate!(code)
  end

  def move!(length)
    @position.forward!(direction: @direction, length: length)
  end

  def distance
    @position.x.abs + @position.y.abs
  end
end

def drive(seq, expected = nil)
  taxi = Taxi.new
  taxi.read_sequence!(seq)
  distance = taxi.distance
  if expected && distance != expected
    puts "Failed sequence #{seq}: expected #{expected}, got #{distance} "
  else
    puts "Drove #{distance}"
  end
  if taxi.position.twice_visited
    puts "Visited #{taxi.position.twice_visited} twice"
  end
end

seq1 = "R2, L3" #5
drive(seq1, 5)
seq2 = "R2, R2, R2" #2
drive(seq2, 2)
seq4 = "R2, R2, R2, R2" #0
drive(seq4, 0)
seq3 = "R5, L5, R5, R3" #12
drive(seq3, 12)
seq = "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3"
drive(seq)
seq5 = "R8, R4, R4, R8"
drive(seq5)
