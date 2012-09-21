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

    let coord_eq (a,b,c) (d,e,f) =
      let cl1 = [a;b;c] in
      let cl2 = [d;e;f] in 
      List.for_all (fun x -> List.mem x cl2) cl1 &&
      List.for_all (fun x -> List.mem x cl1) cl2

    let row_width = 5
    let num_rows = 17
    (* 5 * 17 = 85 cells *)
    
    let coords_of_board board =
      List.concat (
        List.mapi (fun y row -> List.mapi (fun x value -> (x,y)) row) board
      )


    (* TODO *)
    let init_board seed = 
      (* randomly generate board from given seed *)
      begin
        Random.init seed;
      [ [B;R;B];
       [G;G;G];
        [R;R;G];
       [B;B;G]
      ]
      end

    let validate_pos board (x, y) = 
      let cols = List.length (List.hd board)
      and rows = List.length board in
      if y >= 0 && x >= 0
         && x < cols && y < rows
      then Some (x, y)
      else None

    let get_sw board (x, y) = if y mod 2 = 0 
                        then validate_pos board (x, y + 1)
                        else validate_pos board (x - 1, y + 1)
    let get_s  board (x, y) = validate_pos board (x, y + 2)
    let get_se board (x, y) = if y mod 2 = 0
                        then validate_pos board (x, y + 1)
                        else validate_pos board (x + 1, y + 1)

    (* TODO *)
    let get_rotatable_positions board = 
      (* based on length and heigth of board, systematically construct all
       * rotatable positions
       *)
      let add_position acc_positions (coord, adj_s, adj_diag) =
        match adj_s, adj_diag with
        | None, _ 
        | _, None -> acc_positions
        | Some coord_s, Some coord_d ->
            let pos = (coord, coord_s, coord_d) in
            if List.exists (coord_eq pos) acc_positions
            then acc_positions
            else pos :: acc_positions in
      List.fold_left (fun acc_positions coord -> 
        let sw = get_sw board coord
        and s  = get_s board coord
        and se = get_se board coord in
        add_position (add_position acc_positions (coord, s, sw))
                     (coord, s, se)
      ) [] (coords_of_board board)

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

    let swap_values board pos1 pos2 =
      let val1 = get_board board pos1 
      and val2 = get_board board pos2 in
      set_board (set_board board pos1 val2) pos2 val1

    let is_empty board pos = get_board board pos = E 
    let is_filled board pos = not(is_empty board pos)


    let is_on_top (x1, y1) (x2, y2) = 
      x1 = x2 && (y1 + 2 = y2 || y1 = y2 + 2)

    (* check whether two positions are adjacent diagonally *)
    let is_diag (x1, y1) (x2, y2) = 
      let local_diag (x1, y1) (x2, y2) =
        if y1 mod 2 = 0
        then x1 = x2 && y1 + 1 = y2 || x1 + 1 = x2 && y1 + 1 = y2
        else x1 - 1 = x2 && y1 + 1 = y2 || x1 = x2 && y1 + 1 = y2 in
      if y1 = y2 then false
      else 
         local_diag (x1, y1) (x2, y2)
      || local_diag (x2, y2) (x1, y1)
      
    let string_of_coord (x, y) = "("^string_of_int x^","^string_of_int y^")"

    let collect_clusters board =
      let adjacent_eq_pos pos1 pos2 = 
        get_board board pos1 = get_board board pos2 
        && is_filled board pos1
        && ( is_on_top pos1 pos2 
          || is_diag pos1 pos2 
           ) in
      let is_relevant pos cluster =
        List.exists (adjacent_eq_pos pos) cluster in
      let add_coord clusters pos = 
        let (to_merge_clusters, others) = 
          List.partition (is_relevant pos) clusters 
        in
          if to_merge_clusters = []
          then [pos] :: others
          else (pos :: List.concat to_merge_clusters) :: others in
      let all_positions = coords_of_board board 
      in
        List.filter 
          (fun cluster -> List.length cluster >= 3)
          (List.fold_left add_coord [] all_positions)

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


    let cell_of_int i = match i with
      | 0 -> R
      | 1 -> G
      | 2 -> B
      | 3 -> Y
      | _ -> failwith ("'cell_of_int' unhandled input " ^ string_of_int i)

    let get_random_cell_value () = cell_of_int (Random.int 3)

    let fill_board board = 
      List.map (fun row -> List.map 
                           (fun e -> if e = E 
                                     then get_random_cell_value() 
                                     else e
                           ) 
                           row) 
               board

    let to_n inc start n = 
      let rec loop x =
        if x >= n then []
        else x :: loop (inc x)
      in
        loop start

    let get_column_idxs board = to_n (fun x -> x + 1) 0 (List.length (List.hd board))

    let get_odd_row_idxs board =
        to_n (fun x -> x + 2) 1 (List.length board)

    let get_even_row_idxs board =
        to_n (fun x -> x + 2) 0 (List.length board)

    let scan_col f board col_idx row_idxs =
      let rec loop row_idxs =
        match row_idxs with
        | [] -> None
        | row_idx :: row_idxs -> if f board (col_idx, row_idx)
                                 then Some (col_idx, row_idx)
                                 else loop row_idxs
      in
        loop row_idxs

    let pull_down_odd_even_colums board col_idx =
      let rec loop acc_board row_idxs = match row_idxs with
          | [] -> acc_board
          | row_idx :: row_idxs -> 
              if is_empty acc_board (col_idx, row_idx)
              then match scan_col is_filled acc_board col_idx row_idxs with
                   | None -> acc_board
                   | Some pos -> 
                       let new_board = swap_values acc_board (col_idx, row_idx) pos in
                       loop new_board row_idxs
              else loop acc_board row_idxs
      in
        loop (loop board (List.rev (get_even_row_idxs board)))
             (List.rev (get_odd_row_idxs board))


    (* make cells fall as far down as possible, not filling in new cells *)
    let pull_down_cells board = 
      List.fold_left 
        pull_down_odd_even_colums
        board (get_column_idxs board)
     

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
    
    let drop_cells board =
      fill_board (pull_down_cells board)

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
      let dropped_board = Game.drop_cells board in
      let (new_board, new_score) = Game.clear_and_score dropped_board in
      if new_score = 0 then (board, score)
      else (Game.drop_cells new_board, score + new_score)
    ) best_greedy_move
  



let main () = 
  ite_trace hexic_step (Game.init_board 0, 0)


let _ = 
  List.iter (fun (board, score) -> 
    print_endline (string_of_int score ^ "\n" 
                   ^ Game.string_of_board board ^ "\n")
  ) ((Game.init_board 0, 0) :: (List.rev (main())))
