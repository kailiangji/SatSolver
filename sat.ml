
[@@@landmark "auto"]

let remove_if_exists literal clause =
  let rec remove_if_exists l cl acc =
    match cl with
    | [] -> List.rev acc
    | l' :: cl' ->
      if l = l' then (List.rev acc) @ cl'
      else remove_if_exists l cl' (l' :: acc)
  in
  remove_if_exists literal clause []

let assign_literal literal clauses =
  let rec assign_literal l cls acc =
    match cls with
    | [] -> List.rev acc
    | [] :: cls' -> assign_literal l cls' acc
    | cl :: cls' ->
      if (List.mem l cl) then assign_literal l cls' acc
      else
        match remove_if_exists (-l) cl with
        | [] -> [[0]]
        | cl' -> assign_literal l cls' (cl' :: acc)
  in
  assign_literal literal clauses []

let rec rest_of_literals acc literals =
  match acc, literals with
  | [], _ -> literals
  | h :: t, [] -> []
  | h :: t, h1 :: t1 -> rest_of_literals t t1
  
let is_satisfiable literals clauses =
  let rec is_satisfiable ls cls acc1 acc2 =
    match cls with
    | [] -> true
    | _ ->
     (* let () = print_string "acc1\n" in
      let () = List.iter (fun x -> Printf.printf "%d " x) acc1
      in
      let () = print_newline() in
      let () = print_string "acc2\n" in
      let () = List.iter (fun u -> (List.iter (fun x ->
          List.iter (fun y -> Printf.printf "%d " y) x;
          print_newline()
        ) u)) acc2
      in
      let () = print_string "clauses\n" in
      let () = List.iter ( fun x ->
          List.iter (fun y -> Printf.printf "%d " y) x;
        print_newline()) cls
        in*)
      if List.mem [0] cls then
        ( match acc1, acc2 with
          | [], _ -> print_endline "failure 1"; false
          | a :: acc1', a2 :: acc2' ->
            let rls = rest_of_literals acc1 literals in
            let cls' = assign_literal a a2 in
            is_satisfiable rls cls' acc1' acc2'
          | _, _ -> print_endline "failure 2"; false
        )
      else
        (match ls with
         | [] -> (match acc1, acc2 with
             | [], _ -> print_endline "failure"; false
             | a :: acc1', a2 :: acc2' ->
               let rls = rest_of_literals acc1 literals in
               let cls' = assign_literal a a2 in
               is_satisfiable rls cls' acc1' acc2'
             | _, _ -> print_endline "failure"; false)
         | l :: ls' ->
           let cls' = assign_literal l cls in
           is_satisfiable ls' cls' (-l :: acc1) (cls :: acc2))
  in
  is_satisfiable literals clauses [] []

let rec add r = function
  | [] -> r
  | l :: ls -> add (if List.mem (abs l) r then r else abs(l) :: r) ls

let list_literals = fun clauses -> List.sort compare (List.fold_left add [] clauses)

let rec string_list_to_clause r = function
  | [] -> r
  | "0" :: _ -> r
  | v :: vs -> string_list_to_clause (int_of_string v :: r) vs

let rec tsv_to_clauses clauses = fun in_channel ->
  match input_line in_channel with
  | exception End_of_file -> close_in_noerr in_channel; clauses
  | line when line.[0] == 'c' || line.[0] == 'p' -> tsv_to_clauses clauses in_channel
  | line ->
    let clause = Str.split (Str.regexp "[ ]+") line in
    let clause2 = string_list_to_clause [] clause in
    tsv_to_clauses (clause2 :: clauses) in_channel

let _ =
  Landmark.start_profiling();
  let file = Sys.argv.(1) in 
  let clauses = tsv_to_clauses [] (open_in file) in
  let literals = list_literals clauses in
  Printf.printf "=== %s ===\nVariables: %i\nClauses: %i"
    (Sys.argv.(0))
    (List.length literals) 
    (List.length clauses);
  print_newline(); (* Force flushing *)
  let is_sat = is_satisfiable literals clauses in
  Printf.printf "Satisfiable: %b\n" is_sat;
  print_newline() (* Force flushing *)
