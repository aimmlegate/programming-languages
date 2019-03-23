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
                  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]),
                  rotations([[0, 0], [-1, 0], [1,0], [-1, 1], [0, 1]]),
                  [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # longest (only needs two)
                  [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
                  rotations([[0, 0], [1, 0], [0,1]])]

  def initialize(point_array, board)
    super(point_array, board)
  end

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super (game)
    @current_block = MyPiece.next_piece(self)
  end

  def rotate_180
    rotate_clockwise
    rotate_clockwise
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
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
  end

end


