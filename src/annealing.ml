(** Simulated annealing optimisation algorithm
    @author Victor Sannier *)

(** Evaluate a grid by counting the number of empty squares it contains *)
let evaluate (g : Grid.t) : float = Float.of_int (Grid.count_none g)

(** Get a new grid by adding or recursively removing a word at random *)
let get_random_neighbour (words : Words.t) (g : Grid.t) : Grid.t =
  let dimx, dimy = Grid.get_dims g in
  if Random.int (dimx + dimy) = 0 && Grid.count_some g > 0 then
    Grid.remove_random_word words g
  else Grid.insert_random_word words g

(** Calculate the acceptance probability
    given the temperature, the evaluation of the current state
    and the evaluation of the new state *)
let acceptance_probability (temperature : float) (e : float) (e' : float) =
  if e' < e then 1.0 else Float.pow 2.718281 ((e -. e') /. temperature)

(** Execute the simulated annealing algorithm to generate a puzzle
    with a given height, size and allowed words *)
let generate_puzzle (width : int) (height : int) (kmax : int) (words : Words.t)
    : Grid.t * float =
  let state : Grid.t ref = ref (Grid.make height width) in
  let state_score : float ref = ref (evaluate !state) in
  let best_state : Grid.t ref = ref !state in
  let best_score : float ref = ref !state_score in
  for k = 0 to pred kmax do
    let temperature = 1.0 -. ((Float.of_int k +. 1.0) /. Float.of_int kmax) in
    let neighbour = get_random_neighbour words !state in
    let neighbour_score = evaluate neighbour in
    (* Probabilistically accept the neighbour *)
    if
      acceptance_probability temperature !state_score neighbour_score
      > Random.float 1.0
    then (
      state := neighbour;
      state_score := neighbour_score);
    (* Update the best state if necessary *)
    if state_score < best_score then (
      best_state := !state;
      best_score := !state_score);
    Printf.printf "[INFO] k=%d T=%.2f E=%.0f (Emin=%.0f)\r" (succ k) temperature
      !state_score !best_score;
    flush stdout
  done;
  print_char '\n';
  (!best_state, !best_score)
