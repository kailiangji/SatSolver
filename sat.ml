
[@@@landmark "auto"]

let rec remove_if_exists literal = function
  | [] -> []
  | l :: ls -> if (l == literal) then ls else l :: remove_if_exists literal ls

let rec assign_literal literal = function
  | [] -> []
  | [] :: clauses -> assign_literal literal clauses
  | c :: clauses ->
    if (List.mem literal c) then assign_literal literal clauses
    else (match remove_if_exists (-literal) c with
        | [] -> [[0]]
        | clause -> clause :: assign_literal literal clauses)

let rec is_satisfiable literals = function
  | [] -> true
  | clauses ->
    if List.mem [0] clauses then false else
      (match literals with
       | [] -> print_endline "failure"; false
       | literal :: ls -> 
         is_satisfiable ls (assign_literal literal clauses) || 
         is_satisfiable ls (assign_literal (-literal) clauses)
      )

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
