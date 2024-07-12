open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let basic_omok =
    let open Game in
    empty Game_kind.Omok
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 4 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 3 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 0 }
  ;;

  (* let bw_game = let open Game in empty_game |> place_piece ~piece:Piece.X
     ~position:{ Position.row = 0; column = 0 } |> place_piece ~piece:Piece.O
     ~position:{ Position.row = 2; column = 0 } |> place_piece ~piece:Piece.X
     ~position:{ Position.row = 1; column = 1 } |> place_piece ~piece:Piece.O
     ~position:{ Position.row = 2; column = 1 } ;;

     let block_game = let open Game in empty_game |> place_piece
     ~piece:Piece.X ~position:{ Position.row = 0; column = 0 } |> place_piece
     ~piece:Piece.O ~position:{ Position.row = 2; column = 0 } |> place_piece
     ~piece:Piece.X ~position:{ Position.row = 0; column = 2 } |> place_piece
     ~piece:Piece.O ~position:{ Position.row = 2; column = 1 } ;; *)

  let print_game (game : Game.t) =
    let len =
      match game.game_kind with Game.Game_kind.Tic_tac_toe -> 3 | _ -> 15
    in
    let rows = List.range 0 len in
    let cols = List.range 0 len in
    List.iter rows ~f:(fun r ->
      List.iter cols ~f:(fun c ->
        let printing_char =
          match
            Map.find game.board { Game.Position.row = r; column = c }
          with
          | None -> " "
          | Some p -> (match p with X -> "X" | O -> "O")
        in
        if c <> len - 1
        then print_string (printing_char ^ " | ")
        else print_string (printing_char ^ "\n"));
      if r <> len - 1 then print_string "---------\n")
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  let get_diagonals r c offset (game : Game.t) =
    match game.game_kind with
    | Tic_tac_toe ->
      [ Map.find game.board { Game.Position.row = r; column = c }
      ; Map.find
          game.board
          { Game.Position.row = r + 1; column = c + offset }
      ; Map.find
          game.board
          { Game.Position.row = r + 2; column = c + (2 * offset) }
      ]
    | Omok ->
      [ Map.find game.board { Game.Position.row = r; column = c }
      ; Map.find
          game.board
          { Game.Position.row = r + 1; column = c + offset }
      ; Map.find
          game.board
          { Game.Position.row = r + 2; column = c + (2 * offset) }
      ; Map.find
          game.board
          { Game.Position.row = r + 3; column = c + (3 * offset) }
      ; Map.find
          game.board
          { Game.Position.row = r + 4; column = c + (4 * offset) }
      ]
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    let len =
      match game.game_kind with Game.Game_kind.Tic_tac_toe -> 3 | _ -> 15
    in
    let rows = List.range 0 len in
    let cols = List.range 0 len in
    let positions_grid =
      List.map rows ~f:(fun r ->
        List.map cols ~f:(fun c -> { Game.Position.row = r; column = c }))
    in
    let positions_list = List.concat positions_grid in
    List.filter positions_list ~f:(fun pos -> not (Map.mem game.board pos))
  ;;

  (* let rec find_streak piece_list last_piece streak (game : Game.t) = ( (*
     if streak = Game.Game_kind.win_length game.game_kind then Some
     (Game.Evaluation.Game_over {winner = Some last_piece}) else if
     List.length piece_list < Game.Game_kind.win_length game.game_kind then
     None else ( match piece_list with | next_piece::rest -> if
     Option.is_some next_piece && (Game.Piece.equal (Option.value_exn
     next_piece) last_piece) then find_streak rest (Option.value_exn
     next_piece) (streak+1) game else find_streak rest next_piece 0 game | []
     -> None *) List.map piece_list ~f:( fun p -> match piece_list with

     ) ) ;; *)

  let empty_and_adj_to_smth (game : Game.t) =
    List.map
      (Map.keys game.board)
      ~f:(fun { Game.Position.row = r; column = c } ->
        let neighbors =
          [ { Game.Position.row = r - 1; column = c - 1 }
          ; { Game.Position.row = r - 1; column = c }
          ; { Game.Position.row = r - 1; column = c + 1 }
          ; { Game.Position.row = r; column = c - 1 }
          ; { Game.Position.row = r; column = c + 1 }
          ; { Game.Position.row = r + 1; column = c - 1 }
          ; { Game.Position.row = r + 1; column = c }
          ; { Game.Position.row = r + 1; column = c + 1 }
          ]
        in
        List.filter
          neighbors
          ~f:(fun ({ Game.Position.row = r2; column = c2 } as pos) ->
            r2 >= 0
            && r2 < Game.Game_kind.board_length game.game_kind
            && c2 >= 0
            && c2 < Game.Game_kind.board_length game.game_kind
            && Option.is_none (Map.find game.board pos)))
    |> List.concat
    |> Game.Position.Set.of_list
  ;;

  (* Option.is_some (List.find neighbors ~f:(fun p -> Option.is_some
     (Map.find game.board p))) && Option.is_none (Map.find game.board pos) *)
  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    (* let rec row_check r c (g : Game.t) (piece : Game.Piece.t) cons_count =
       if cons_count = (Game.Game_kind.win_length g.game_kind) then Some
       (Game.Evaluation.Game_over {winner = Some piece}) else ( let p =
       Map.find g.board {Game.Position.row = r; column = c} in if
       Option.is_some p && (Game.Piece.equal (Option.value_exn p) piece && c
       < Game.Game_kind.board_length g.game_kind) then (row_check r (c+1) g
       piece (cons_count + 1)) else row_check r (c+1) g piece (0) ) in if
       List.is_empty (available_moves game) then Game.Evaluation.Game_over
       {winner = None} else let len = match game.game_kind with |
       Game.Game_kind.Tic_tac_toe -> 3 | _ -> 15 in let rows = List.range 0
       len in List.find (List.map rows ~f(fun row -> row_check row )) *)
    (* let row_check r = let len = match game.game_kind with |
       Game.Game_kind.Tic_tac_toe -> 3 | _ -> 15 in let rows = List.range 0
       len in let pieces = List.map (List.range 0 len) ~f:(fun c -> Map.find
       game.board {Game.Position.row = r; column = c}) in *)
    let possible_illegal_move =
      List.find (Map.keys game.board) ~f:(fun pos ->
        match pos with
        | { Game.Position.row = r; column = c } ->
          r < 0
          || c < 0
          || r >= Game.Game_kind.board_length game.game_kind
          || c >= Game.Game_kind.board_length game.game_kind)
    in
    if Option.is_some possible_illegal_move
    then Game.Evaluation.Illegal_move
    else (
      (* given a list of moves, returns finds a position where a streak
         starts *)
      (* let _ = (if Game.Game_kind.equal game Game.Game_kind.Omok then
         printf "INDEX IS %d\n" index else print_string " ") in [p] *)
      let find_streak
        (piece_arr : Game.Piece.t option array)
        desired_streak_length
        =
        (* let _ = if Game.Game_kind.equal game Game.Game_kind.Omok then
           printf "length is %d\n" (Array.length piece_arr) in *)
        let streak_start =
          Array.mapi piece_arr ~f:(fun index p ->
            ( p
            , index + desired_streak_length - 1 < desired_streak_length
              && Option.is_some
                   (List.all_equal
                      ~equal:(fun p1 p2 ->
                        Option.is_some p1
                        && Option.is_some p2
                        && Game.Piece.equal
                             (Option.value_exn p1)
                             (Option.value_exn p2))
                      (List.of_array
                         (Array.slice
                            piece_arr
                            index
                            (index + desired_streak_length)))) ))
        in
        Array.find streak_start ~f:(fun (_p, b) -> b)
      in
      let len =
        match game.game_kind with Game.Game_kind.Tic_tac_toe -> 3 | _ -> 15
      in
      let rows = List.range 0 len in
      let cols = List.range 0 len in
      let piece_grid =
        List.map rows ~f:(fun r ->
          List.map cols ~f:(fun c ->
            let pos = { Game.Position.row = r; column = c } in
            Map.find game.board pos))
      in
      let streak_row =
        List.find piece_grid ~f:(fun row_pieces ->
          Option.is_some
            (find_streak
               (Array.of_list row_pieces)
               (Game.Game_kind.win_length game.game_kind)))
      in
      if Option.is_some streak_row
      then (
        let p, _ =
          Option.value_exn
            (find_streak
               (Array.of_list (Option.value_exn streak_row))
               (Game.Game_kind.win_length game.game_kind))
        in
        Game.Evaluation.Game_over { winner = p })
      else (
        let columns =
          List.map cols ~f:(fun c ->
            List.map rows ~f:(fun r ->
              let pos = { Game.Position.row = r; column = c } in
              Map.find game.board pos))
        in
        let streak_col =
          List.find columns ~f:(fun col_pieces ->
            Option.is_some
              (find_streak
                 (Array.of_list col_pieces)
                 (Game.Game_kind.win_length game.game_kind)))
        in
        if Option.is_some streak_col
        then (
          let p2, _ =
            Option.value_exn
              (find_streak
                 (Array.of_list (Option.value_exn streak_col))
                 (Game.Game_kind.win_length game.game_kind))
          in
          Game.Evaluation.Game_over { winner = p2 })
        else (
          let coordinates =
            List.concat_map rows ~f:(fun r ->
              List.map cols ~f:(fun c -> r, c))
          in
          let diagonal_start_bl =
            List.find coordinates ~f:(fun (r, c) ->
              let diagonal_list = get_diagonals r c (-1) game in
              let diagonal_array = List.to_array diagonal_list in
              Option.is_some
                (find_streak
                   diagonal_array
                   (Game.Game_kind.win_length game.game_kind)))
          in
          if Option.is_some diagonal_start_bl
          then (
            let r, c = Option.value_exn diagonal_start_bl in
            let piece =
              Map.find_exn game.board { Game.Position.row = r; column = c }
            in
            Game.Evaluation.Game_over { winner = Some piece })
          else (
            let diagonal_start_br =
              List.find coordinates ~f:(fun (r, c) ->
                let diagonal_list = get_diagonals r c 1 game in
                let diagonal_array2 = List.to_array diagonal_list in
                Option.is_some
                  (find_streak
                     diagonal_array2
                     (Game.Game_kind.win_length game.game_kind)))
            in
            if Option.is_some diagonal_start_br
            then (
              let r2, c2 = Option.value_exn diagonal_start_br in
              let piece =
                Map.find_exn
                  game.board
                  { Game.Position.row = r2; column = c2 }
              in
              Game.Evaluation.Game_over { winner = Some piece })
            else if List.is_empty (available_moves game)
            then Game.Evaluation.Game_over { winner = None }
            else Game.Evaluation.Game_continues))))
  ;;

  (* let len = match game.game_kind with | Game.Game_kind.Tic_tac_toe -> 3 |
     _ -> 15 in let rows = Array.range 0 len in let cols = Array.range 0 len
     in *)
  (* let piece_lists = List.map rows ~f:(fun r -> ) Array.map *)

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    List.filter
      (Set.to_list (empty_and_adj_to_smth game))
      ~f:(fun pos ->
        let new_game = place_piece game ~piece:me ~position:pos in
        match evaluate new_game with
        | Game.Evaluation.Game_over { winner = victor } ->
          Option.is_some victor
          && Game.Piece.equal (Option.value_exn victor) me
        | _ -> false)
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    winning_moves ~me:(Game.Piece.flip me) game
  ;;

  (* Exercise 5 *)
  let _available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    =
    let l =
      Set.filter (empty_and_adj_to_smth game) ~f:(fun pos ->
        List.is_empty
          (losing_moves ~me (place_piece ~position:pos ~piece:me game)))
      |> Set.to_list
    in
    let _ = print_s [%message (l : Game.Position.t list)] in
    l
  ;;

  let available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (_game : Game.t)
    =
    ignore me;
    List.init 5 ~f:(fun _i ->
      { Game.Position.column = Random.int 15; row = Random.int 15 })
  ;;

  let find_streaks
    (piece_arr : Game.Piece.t option array)
    desired_streak_length
    =
    (* let _ = if Game.Game_kind.equal game Game.Game_kind.Omok then printf
       "length is %d\n" (Array.length piece_arr) in *)
    let streak_start =
      Array.mapi piece_arr ~f:(fun index p ->
        ( p
        , index + desired_streak_length - 1 < desired_streak_length
          && Option.is_some
               (List.all_equal
                  ~equal:(fun p1 p2 ->
                    Option.is_some p1
                    && Option.is_some p2
                    && Game.Piece.equal
                         (Option.value_exn p1)
                         (Option.value_exn p2))
                  (List.of_array
                     (Array.slice
                        piece_arr
                        index
                        (index + desired_streak_length)))) ))
    in
    Array.filter streak_start ~f:(fun (_p, b) -> b)
  ;;

  let score (_game : Game.t) _my_piece = Random.float 20.

  let _score (game : Game.t) my_piece =
    match evaluate game with
    | Game.Evaluation.Game_over { winner = Some w } ->
      if Game.Piece.equal w my_piece
      then Float.max_value
      else Float.min_value
    | _ ->
      if not (List.is_empty (winning_moves ~me:my_piece game))
      then Float.max_value -. 1.0
      else if not (List.is_empty (losing_moves ~me:my_piece game))
      then Float.min_value +. 1.0
      else (
        (* actual heuristic done (?) *)
        (* let middle = {Game.Position.row = (Game.Game_kind.board_length
           game.game_kind)/2; column = (Game.Game_kind.board_length
           game.game_kind)/2} in let _ = if Option.is_none (Map.find
           game.board middle) then modifier := !modifier + 2 in *)
        (* let my_positions = List.filter (Map.keys game.board) ~f:(fun pos
           -> Option.is_some (Map.find game.board pos) && Game.Piece.equal
           (Option.value_exn (Map.find game.board pos)) my_piece) in *)
        let rows =
          List.range 0 (Game.Game_kind.board_length game.game_kind)
        in
        let cols =
          List.range 0 (Game.Game_kind.board_length game.game_kind)
        in
        (* let value_list = List.map my_positions ~f:(fun ({Game.Position.row
           = r; column = c}) -> let ) *)
        let row_adj_vals =
          List.map rows ~f:(fun r ->
            let row =
              List.map cols ~f:(fun c ->
                Map.find game.board { Game.Position.row = r; column = c })
            in
            let lens =
              List.range 2 (Game.Game_kind.win_length game.game_kind + 1)
            in
            let streak_len =
              List.find (List.rev lens) ~f:(fun l ->
                let possible_streaks = find_streaks (Array.of_list row) l in
                let my_streaks =
                  Array.filter possible_streaks ~f:(fun (p, _) ->
                    Game.Piece.equal (Option.value_exn p) my_piece)
                in
                not (Array.is_empty my_streaks)
                (* in not (Array.is_empty possible_streaks) && Array.exists
                   possible_streaks ~f:(fun (p, _) -> Game.Piece.equal
                   (Option.value_exn p) my_piece) *))
            in
            match streak_len with
            | None -> 0
            | _ ->
              Option.value_exn streak_len
              * Option.value_exn streak_len
              * Int.of_float
                  (sqrt
                     (Float.of_int
                        (List.length
                           (List.filter row ~f:(fun p -> Option.is_none p)))))
            (* possibly fine tune so that function doesn't just look for
               highest l as it does rn bc of list.find (Array.length
               my_streaks) * *))
        in
        let row_adj_score =
          List.fold row_adj_vals ~init:0 ~f:(fun accum s -> accum + s)
        in
        let col_adj_vals =
          List.map cols ~f:(fun c ->
            let col =
              List.map rows ~f:(fun r ->
                Map.find game.board { Game.Position.row = r; column = c })
            in
            let lens =
              List.range 2 (Game.Game_kind.win_length game.game_kind + 1)
            in
            let streak_len =
              List.find (List.rev lens) ~f:(fun l ->
                let possible_streaks = find_streaks (Array.of_list col) l in
                let my_streaks =
                  Array.filter possible_streaks ~f:(fun (p, _) ->
                    Game.Piece.equal (Option.value_exn p) my_piece)
                in
                not (Array.is_empty my_streaks)
                (* in not (Array.is_empty possible_streaks) && Array.exists
                   possible_streaks ~f:(fun (p, _) -> Game.Piece.equal
                   (Option.value_exn p) my_piece) *))
            in
            match streak_len with
            | None -> 0
            | _ ->
              Option.value_exn streak_len
              * Option.value_exn streak_len
              * Int.of_float
                  (sqrt
                     (Float.of_int
                        (List.length
                           (List.filter col ~f:(fun p -> Option.is_none p)))))
            (* possibly fine tune so that function doesn't just look for
               highest l as it does rn bc of list.find (Array.length
               my_streaks) * *))
        in
        let col_adj_score =
          List.fold col_adj_vals ~init:0 ~f:(fun accum s -> accum + s)
        in
        let diag_bls_scores2d =
          List.map rows ~f:(fun r ->
            List.map cols ~f:(fun c ->
              if r < 10 || c < 4
              then 0
              else (
                let bl_diags = get_diagonals r c (-1) game in
                let lens =
                  List.range 2 (Game.Game_kind.win_length game.game_kind + 1)
                in
                let streak_len =
                  List.find (List.rev lens) ~f:(fun l ->
                    let possible_streaks =
                      find_streaks (Array.of_list bl_diags) l
                    in
                    let my_streaks =
                      Array.filter possible_streaks ~f:(fun (p, _) ->
                        Game.Piece.equal (Option.value_exn p) my_piece)
                    in
                    not (Array.is_empty my_streaks)
                    (* in not (Array.is_empty possible_streaks) &&
                       Array.exists possible_streaks ~f:(fun (p, _) ->
                       Game.Piece.equal (Option.value_exn p) my_piece) *))
                in
                match streak_len with
                | None -> 0
                | _ ->
                  Option.value_exn streak_len
                  * Option.value_exn streak_len
                  * Int.of_float
                      (sqrt
                         (Float.of_int
                            (List.length
                               (List.filter bl_diags ~f:(fun p ->
                                  Option.is_none p))))))))
        in
        let diag_bls_scores2d = List.concat diag_bls_scores2d in
        let diag_bl_adj_score =
          List.fold diag_bls_scores2d ~init:0 ~f:(fun accum s -> accum + s)
        in
        let diag_brs_scores2d =
          List.map rows ~f:(fun r ->
            List.map cols ~f:(fun c ->
              if r < 10 || c < 10
              then 0
              else (
                let br_diags = get_diagonals r c 1 game in
                let lens =
                  List.range 2 (Game.Game_kind.win_length game.game_kind + 1)
                in
                let streak_len =
                  List.find (List.rev lens) ~f:(fun l ->
                    let possible_streaks =
                      find_streaks (Array.of_list br_diags) l
                    in
                    let my_streaks =
                      Array.filter possible_streaks ~f:(fun (p, _) ->
                        Game.Piece.equal (Option.value_exn p) my_piece)
                    in
                    not (Array.is_empty my_streaks)
                    (* in not (Array.is_empty possible_streaks) &&
                       Array.exists possible_streaks ~f:(fun (p, _) ->
                       Game.Piece.equal (Option.value_exn p) my_piece) *))
                in
                match streak_len with
                | None -> 0
                | _ ->
                  Option.value_exn streak_len
                  * Option.value_exn streak_len
                  * Int.of_float
                      (sqrt
                         (Float.of_int
                            (List.length
                               (List.filter br_diags ~f:(fun p ->
                                  Option.is_none p))))))))
        in
        let diag_brs_scores = List.concat diag_brs_scores2d in
        let diag_br_adj_score =
          List.fold diag_brs_scores ~init:0 ~f:(fun accum s -> accum + s)
        in
        Float.of_int
          (row_adj_score
           + col_adj_score
           + diag_bl_adj_score
           + diag_br_adj_score))
  ;;

  let rec minimax
    (game : Game.t)
    (depth : int)
    (maximizing_player : bool)
    (me : Game.Piece.t)
    (a : float)
    (b : float)
    =
    (* doesn't work with illegal moves *)
    let a_ref = ref a in
    let b_ref = ref b in
    let game_continuing =
      match evaluate game with
      | Game.Evaluation.Game_over { winner = _ } -> false
      | _ -> true
    in
    let considered_moves =
      available_moves_that_do_not_immediately_lose ~me game
    in
    if depth = 0 || not game_continuing
    then score game me
    else if maximizing_player
    then (
      let max_val = ref Float.min_value in
      let _ =
        let cutoff = ref false in
        List.iter considered_moves ~f:(fun pos ->
          if not !cutoff
          then (
            let possible_board1 = place_piece ~piece:me ~position:pos game in
            let eval1 =
              minimax
                possible_board1
                (depth - 1)
                (not maximizing_player)
                (Game.Piece.flip me)
                !a_ref
                !b_ref
            in
            max_val := Float.max eval1 !max_val;
            if Float.( <= ) !max_val !b_ref
            then a_ref := Float.max !a_ref !max_val
            else cutoff := true))
        (* let opt_val = List.max_elt considered_moves ~compare:(fun pos1
           pos2 -> let possible_board1 = place_piece ~piece:(me)
           ~position:(pos1) game in let eval1 = minimax possible_board1
           (depth-1) (not maximizing_player) (Game.Piece.flip me) in let
           possible_board2 = place_piece ~piece:(me) ~position:(pos2) game in
           let eval2 = minimax possible_board2 (depth-1) (not
           maximizing_player) (Game.Piece.flip me) in if (Float.(>.) eval1
           eval2) then 1 else if Float.(<.) eval1 eval2 then -1 else 0 ) *)
      in
      !max_val
      (* match opt_val with | None -> Float.min_value | _ -> Option.value_exn
         opt_val *))
    else (
      let min_val = ref Float.max_value in
      let _ =
        let cutoff = ref false in
        List.iter considered_moves ~f:(fun pos ->
          if not !cutoff
          then (
            let possible_board1 = place_piece ~piece:me ~position:pos game in
            let eval1 =
              minimax
                possible_board1
                (depth - 1)
                (not maximizing_player)
                (Game.Piece.flip me)
                !a_ref
                !b_ref
            in
            min_val := Float.min eval1 !min_val;
            if Float.( >= ) !min_val !a_ref
            then b_ref := Float.min !b_ref !min_val
            else cutoff := true))
      in
      !min_val)
  ;;

  let choose_move (game : Game.t) my_piece =
    if List.is_empty (Map.keys game.board)
    then { Game.Position.row = 0; column = 0 }
    else if not (List.is_empty (winning_moves game ~me:my_piece))
    then List.hd_exn (winning_moves game ~me:my_piece)
    else (
      let continuing_moves =
        available_moves_that_do_not_immediately_lose ~me:my_piece game
      in
      (* let _ = print_s [%message (continuing_moves : Game.Position.t list)]
         in *)
      match continuing_moves with
      | [] -> List.hd_exn (available_moves game)
      | _ ->
        (* let considered_moves_set = Set.filter (Game.Position.Set.of_list
           (available_moves_that_do_not_immediately_lose ~me:my_piece game))
           ~f:(fun pos -> Set.mem (empty_and_adj_to_smth game) pos) in *)
        (* let _ = print_s [%message (considered_moves : Game.Position.t
           list)] in { Game.Position.row = 0; column = 0 } *)
        let champ_pos = ref (List.hd_exn continuing_moves) in
        let champ_eval = ref Float.min_value in
        let depth =
          match game.game_kind with
          | Game.Game_kind.Tic_tac_toe -> 9
          | _ -> 0
        in
        let _ =
          List.iter continuing_moves ~f:(fun pos ->
            let possible_game =
              place_piece ~piece:my_piece ~position:pos game
            in
            let eval =
              minimax
                possible_game
                depth
                false
                (Game.Piece.flip my_piece)
                Float.min_value
                Float.max_value
            in
            if Float.( > ) eval !champ_eval
            then (
              champ_eval := eval;
              champ_pos := pos))
        in
        !champ_pos)
  ;;

  (* let%expect_test "block_and_win" = let move = choose_move bw_game
     Game.Piece.X in let _ = print_s [%sexp (move : Game.Position.t)] in
     [%expect {| ((row 2) (column 2))|}]; return () ;;

     let%expect_test "block" = let move = choose_move block_game Game.Piece.X
     in let _ = print_s [%sexp (move : Game.Position.t)] in [%expect {| ((row
     2) (column 2))|}]; return () ;; *)

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate non_win in
         let _ = print_game non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate basic_omok in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_five =
    Command.async
      ~summary:"Exercise 5: Available moves that do not immediately lose?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let continuing_moves =
           available_moves_that_do_not_immediately_lose ~me:piece non_win
         in
         print_s [%sexp (continuing_moves : Game.Position.t list)];
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ; "five", exercise_five
      ]
  ;;
end

(* module Response = struct type t = { piece : Game.Piece.t; pos :
   Game.Position.t} [@@deriving sexp_of, bin_io] end *)

module Echo = struct
  module Query = struct
    type t = string [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      { time : Time_ns_unix.t
      ; message : string
      }
    [@@deriving sexp_of, bin_io]
  end
end
(* edit *)

let handle (_client : unit) (query : Rpcs.Take_turn.Query.t) =
  let response =
    { Rpcs.Take_turn.Response.piece = query.you_play
    ; position =
        (* { Game.Position.row = 0; column = 0 } *)
        Exercises.choose_move query.game query.you_play
    }
  in
  return response
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and _controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller" *)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     let implementations =
       Rpc.Implementations.create_exn
         ~on_unknown_rpc:`Close_connection
         ~implementations:[ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle ]
     in
     fun () ->
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
