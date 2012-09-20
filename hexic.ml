open Game

module Game : Game =
  struct
    type cell = R | G | B | Y | E

    (* each row contains an equal number of cells *)
    type board = cell list list

    type board_coord = int * int
    type position = board_coord * board_coord * board_coord 

    let row_width = 5
    let num_rows = 17
    (* 5 * 17 = 85 cells *)

    let init_board seed = 
      (* randomly generate board from given seed *)
      [ [R;R;R];
       [G;G;G];
        [R;R;R];
       [G;G;G]
      ]

    let get_rotatable_positions board = 
      (* based on length and heigth of board, systematically construct all
       * rotatable positions
       *)
      [((0,0),(0,1),(0,2))
      ]

    let rotate board ((x1,y1), (x2,y2), (x3,y3)) = board

    let clear_and_score board = (board, 0)
    let drop_cells board = board
    let string_of_board board = 
      let 
  end

let rec fix f x =
  let new_x = f x in
  if new_x = x then x
  else fix f new_x

  (* trace the iteration of application of function f;
   * trace starts with last value computed and ends in the initial value given
   *)
let ite_trace f init =
  let rec loop trace x =
    let new_x = f x in
    if x = new_x then trace
    else loop (new_x :: trace) new_x
  in
    loop [] init

let hexic_step (board, score) =
  (* in each iteration, we check for each position on the board, which move
   * would give the most points in return, when a move does not change the score
   * the game ends
   *)
  let positions = Game.get_rotatable_positions board in
  let best_greedy_move = 
    List.fold_left (fun (board, score) position -> 
      let rot_board = Game.rotate board position in
      let (clr_board, new_score) = Game.clear_and_score rot_board in
      if new_score + score > score then (clr_board, new_score + score)
      else (board, score)
    ) 
    (board, score) positions 
  in
    fix (fun (board, score) -> 
      let (new_board, new_score) = Game.clear_and_score board in
      if new_score = 0 then (board, score)
      else (Game.drop_cells new_board, score + new_score)
    ) best_greedy_move
  



let main () = 
  ite_trace hexic_step (Game.init_board 0, 0)

(*
 * let _ = main()
 *)
