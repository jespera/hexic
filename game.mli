module type Game = 
  sig
    type board
    type position

    (* get a filled cluster-free board *)
    val init_board : int -> board

    (* get a list of all possible board-positions at which one can perform a
     * rotation; i.e. one cannot rotate in positions that do not have three
     * cells adjacent
     *)
    val get_rotatable_positions : board -> position list

    (* rotate the cells clockwise at the given position, but don't 
     * do the subsequent filling of new cells and implied clearings
     *)
    val rotate : board -> position -> board

    (* compute score of clearing all clusters from board; return cleared board
     * as well as score of doing so
     *
     * does NOT make (new) cells fall
     *)
    val clear_and_score : board -> board * int

    (* make all cells with no cells below fall and pull new (random) cells if a
     * top-most cells fell
     *)
    val drop_cells : board -> board

    (* return string representation of board suitable for printing to console *)
    val string_of_board : board -> string

  end
