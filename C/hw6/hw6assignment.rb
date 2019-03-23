# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                  [[0, 0], [0, -1], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [1, -1]]),
                  rotations([[0, 0], [0, 1], [0, -1], [0, -2], [0, 2]]),
                  rotations([[0, 0], [1, 0], [0, -1]])]

  Cheat_Piece = [[[0, 0]]]

  def initialize(point_array, board)
    super(point_array, board)
  end

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece, board)
  end   

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super (game)
    @current_block = MyPiece.next_piece(self)
    @cheat_status = false
  end

  def rotate_180
    rotate_clockwise
    rotate_clockwise
  end

  def next_piece
    if @cheat_status 
      @current_block = MyPiece.cheat_piece(self)
      @cheat_status = false
      @current_pos = nil
    else
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    end
  end

  def commit_cheat
    if (@score > 100)
      @score = @score - 100
      @cheat_status = true
    end
  end

  def store_current
    locations = @current_block.current_rotation
    l = locations.length - 1
    displacement = @current_block.position
    (0..l).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end



end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    super
    key_bindings_enhancements
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  

  def key_bindings_enhancements
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.commit_cheat})
  end

end


