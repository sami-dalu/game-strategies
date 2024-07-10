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

  Map.add

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
    List.filter positions_list ~f:(fun pos ->
      not
        (List.exists (Map.keys game.board) ~f:(fun p ->
           Game.Position.equal p pos)))
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
      let find_streak (piece_arr : Game.Piece.t option array) game =
        (* let _ = if Game.Game_kind.equal game Game.Game_kind.Omok then
           printf "length is %d\n" (Array.length piece_arr) in *)
        let streak_start =
          Array.mapi piece_arr ~f:(fun index p ->
            ( p
            , index + Game.Game_kind.win_length game - 1
              < Game.Game_kind.win_length game
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
                            (index + Game.Game_kind.win_length game)))) ))
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
            (find_streak (Array.of_list row_pieces) game.game_kind))
      in
      if Option.is_some streak_row
      then (
        let p, _ =
          Option.value_exn
            (find_streak
               (Array.of_list (Option.value_exn streak_row))
               game.game_kind)
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
              (find_streak (Array.of_list col_pieces) game.game_kind))
        in
        if Option.is_some streak_col
        then (
          let p2, _ =
            Option.value_exn
              (find_streak
                 (Array.of_list (Option.value_exn streak_col))
                 game.game_kind)
          in
          Game.Evaluation.Game_over { winner = p2 })
        else (
          let get_diagonals r c offset =
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
          in
          let coordinates =
            List.concat_map rows ~f:(fun r ->
              List.map cols ~f:(fun c -> r, c))
          in
          let diagonal_start_bl =
            List.find coordinates ~f:(fun (r, c) ->
              let diagonal_list = get_diagonals r c (-1) in
              let diagonal_array = List.to_array diagonal_list in
              Option.is_some (find_streak diagonal_array game.game_kind))
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
                let diagonal_list = get_diagonals r c 1 in
                let diagonal_array2 = List.to_array diagonal_list in
                Option.is_some (find_streak diagonal_array2 game.game_kind))
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
    List.filter (available_moves game) ~f:(fun pos ->
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
  let available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    =
    List.filter (available_moves game) ~f:(fun pos ->
      List.is_empty
        (losing_moves ~me (place_piece ~position:pos ~piece:me game)))
  ;;

  let score (game : Game.t) my_piece =
    match evaluate game with
    | Game.Evaluation.Game_over { winner = Some w } ->
      if Game.Piece.equal w my_piece
      then Float.max_value
      else Float.min_value
    | _ ->
      if not (List.is_empty (winning_moves ~me:my_piece game))
      then Float.max_value
      else if not (List.is_empty (losing_moves ~me:my_piece game))
      then Float.min_value
      else (* actual heuristic TBD *)
        0.0
  ;;

  let rec minimax
    (game : Game.t)
    (depth : int)
    (maximizing_player : bool)
    (me : Game.Piece.t)
    =
    (* doesn't work with illegal moves *)
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
        List.iter considered_moves ~f:(fun pos ->
          let possible_board1 = place_piece ~piece:me ~position:pos game in
          let eval1 =
            minimax
              possible_board1
              (depth - 1)
              (not maximizing_player)
              (Game.Piece.flip me)
          in
          max_val := Float.max eval1 !max_val)
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
        List.iter considered_moves ~f:(fun pos ->
          let possible_board1 = place_piece ~piece:me ~position:pos game in
          let eval1 =
            minimax
              possible_board1
              (depth - 1)
              (not maximizing_player)
              (Game.Piece.flip me)
          in
          min_val := Float.min eval1 !min_val)
      in
      !min_val)
  ;;

  let choose_move game my_piece = 
    let considered_moves =
      available_moves_that_do_not_immediately_lose ~me:my_piece game in
    if List.is_empty considered_moves then List.hd_exn (available_moves game)
    else
    let champ_pos = ref (List.hd_exn considered_moves) in
    
    ;;
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
    ; position = { row = 0; column = 0 }
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
