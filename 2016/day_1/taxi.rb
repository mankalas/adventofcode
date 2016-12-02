module Direction
  NORTH = 0
  EAST = 1
  SOUTH = 2
  WEST = 3

  def self.rotate(code, direction)
    shift = (code == 'L' ? -1 : 1)
    (direction + shift) % 4
  end
end

class Position
  attr_reader :x, :y, :twice_visited

  def initialize
    @x, @y = 0, 0
    @paths = []
    @twice_visited = nil
  end

  def forward(direction:, length:)
    #puts "Going #{direction} x #{length}"
    case direction
    when Direction::NORTH
      method = :y=
      shift = 1
    when Direction::SOUTH
      method = :y=
      shift = -1
    when Direction::EAST
      method = :x=
      shift = 1
    when Direction::WEST
      method = :x=
      shift = -1
    end
    length.times do
      self.send(method, shift)
      if @paths.include(self)
        @twice_visited = self
      else
        @paths << self
      end
    end
  end
end

class Taxi
  def initialize
    @direction = Direction::NORTH
    @position = Position.new
  end

  def read_sequence(sequence)
    steps = sequence.split(',')
    steps.each do |step|
      step.strip!

      rotation_code = step[0]
      rotate!(rotation_code)

      length = step[1..-1].to_i
      move!(length)
    end
    self
  end

  def rotate!(code)
    @direction = Direction::rotate(code, @direction)
  end

  def move!(length)
    @position.forward(direction: @direction, length: length)
  end

  def distance
    @position.x.abs + @position.y.abs
  end
end

def drive(seq, expected = nil)
  taxi = Taxi.new
  taxi.read_sequence(seq)
  actual = taxi.distance
  if expected && actual != expected
    puts "Failed sequence #{seq}: expected #{expected}, got #{actual} "
  else
    puts "Drove #{actual}"
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
