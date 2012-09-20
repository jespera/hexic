open Game

module Game =
  struct
    type cell = R | G | B | Y | E

    (* each row contains an equal number of cells *)
    type board = cell list list

    type board_coord = int * int

    (* assumption: coordinates come in _clockwise_ order *)
    type position = board_coord * board_coord * board_coord 

    type cluster = board_coord list

    let cluster_eq cl1 cl2 = 
      List.for_all (fun x -> List.mem x cl2) cl1 &&
      List.for_all (fun x -> List.mem x cl1) cl2

    let row_width = 5
    let num_rows = 17
    (* 5 * 17 = 85 cells *)

    (* TODO *)
    let init_board seed = 
      (* randomly generate board from given seed *)
      [ [R;R;R];
       [G;G;G];
        [R;R;R];
       [G;G;G]
      ]

    (* TODO *)
    let get_rotatable_positions board = 
      (* based on length and heigth of board, systematically construct all
       * rotatable positions
       *)
      [((0,1),(0,0),(0,2))
      ]

    let rec set_row row x value =
      match row with
      | [] -> failwith ("'row_set' out of bounds " ^ string_of_int x)
      | e :: row ->
          if x = 0 then value :: row
          else e :: set_row row (x - 1) value

    let rec set_board board (x,y) value =
      (* traverse board-structure until reaching 0,0 *)
      (* first find the correct row; then colum in that row *)
      match board with 
      | [] -> failwith ("'set_bounds' out of bounds " ^ string_of_int y)
      | row :: board -> 
          if y = 0 then set_row row x value :: board
          else row :: set_board board (x, y - 1) value
    
    let rec get_row row x =
      match row with
      | [] -> failwith ("'row_get' out of bounds " ^ string_of_int x)
      | e :: row ->
          if x = 0 then e
          else get_row row (x - 1)

    let rec get_board board (x, y) =
      match board with 
      | [] -> failwith ("'set_bounds' out of bounds " ^ string_of_int y)
      | row :: board -> 
          if y = 0 then get_row row x
          else get_board board (x, y - 1)

    let rotate board (coord1, coord2, coord3) = 
      let val1 = get_board board coord1
      and val2 = get_board board coord2
      and val3 = get_board board coord3
      in
      set_board (set_board (set_board board 
                                      coord1 val2)
                           coord2 val3)
                coord3 val1

    (* TODO *)
    let collect_clusters board = [[]] 

    let rec three_pow n = if n = 1 then 3 else 3 * three_pow (n - 1)

    let score_cluster cluster =
     (* a cluster of size 3 being cleared provides 3 points, while a cluster of
      * size 4 provides 3*3 = 9 points and a cluster of
      * size 5 provides 3*3*3 = 27
      * ...
      * size n provides 3^(n-2) 
      *)
     three_pow (List.length cluster - 2)

    let clear_cluster board cluster = 
      List.fold_left (fun clr_board pos -> set_board clr_board pos E) board cluster

    let clear_and_score board =
      (* summarize each cluster-score *)
      let clear_clusters = List.fold_left clear_cluster board in
      let clusters = collect_clusters board in
      (clear_clusters clusters
      ,List.fold_left (+) 0 (List.map score_cluster clusters)
      )


    (* TODO *)
    let drop_cells board = board

    (* simple version of printing first *)
    let string_of_board board = 
      let string_of_cell cell = 
        match cell with
        | R -> "R"
        | G -> "G"
        | B -> "B"
        | Y -> "Y"
        | E -> "." in
      let rec string_of_row col odd row = 
        match row with 
        | [] -> ""
        | c :: row -> string_of_cell c 
                      ^ (if col = 1 && odd then "\\" else "\\_/") 
                      ^ string_of_row (col - 1) odd row in
      let rec loop first odd board =
        match board with
        | [] -> ""
        | row :: board -> 
            (
              if first then " _/"
              else if odd then "\\_/"
              else "/"
            )
            ^ string_of_row (List.length row) odd row ^ "\n" ^ loop false (not odd) board in
      let prelude = List.fold_right (fun _ pre -> pre ^ "_   ") (List.hd board) "   " in
      let postlude = List.fold_right (fun _ post -> post ^ "\\_/ ") 
                     (List.hd board) 
                     (if List.length board mod 2 = 0 then "" else "  ") 
      in
        prelude ^ "\n" ^
        loop true true board ^
        postlude
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
