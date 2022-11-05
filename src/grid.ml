(** Grid operations
    @author Victor Sannier *)

type t = char option array array

let make (dimx : int) (dimy : int) : t = Array.make_matrix dimx dimy None
let copy : t -> t = Array.(map copy)

(** Print a grid by printing each line sequentially *)
let print : t -> unit = Array.iter Line.print

(** Get the dimension of a grid *)
let get_dims (g : t) : int * int =
  let dimx = Array.length g in
  if dimx = 0 then failwith "undefined" else (dimx, Array.length g.(0))

(** Check whether a grid (given by its two dimensions) contains a square (given by its coordinates) *)
let out_of_bounds (dimx : int) (dimy : int) (x : int) (y : int) : bool =
  x < 0 || x > pred dimx || y < 0 || y > pred dimy

let get_row (x : int) (g : t) : Line.t = Array.copy g.(x)

let get_col (y : int) (g : t) : Line.t =
  let dimx, _ = get_dims g in
  Array.init dimx (fun i -> g.(i).(y))

(** Count the number of times a predicate is verified in a grid *)
let count_predicate (f : char option -> bool) (g : t) : int =
  let c = ref 0 in
  Array.iter (Array.iter (fun a -> if f a then incr c)) g;
  !c

(** Count the number of empty squares in a grid *)
let count_none : t -> int = count_predicate Option.is_none

(** Count the number of filled squares in a grid *)
let count_some : t -> int = count_predicate Option.is_some

let transpose (g : t) : t =
  let dimx, dimy = get_dims g in
  let tg = make dimy dimx in
  for x = 0 to pred dimx do
    for y = 0 to pred dimy do
      tg.(y).(x) <- g.(x).(y)
    done
  done;
  tg

(** Get all the words in a grid, in both directions *)
let get_words (g : t) : string list =
  let aux (lines : t) : string list =
    lines |> Array.map Line.get_words |> Array.to_list |> List.concat
  in
  aux g @ aux (transpose g)

(** Determine whether a grid is a valid crossword puzzle, i.e. whether all the words in it are valid *)
let is_valid (words : Words.t) (g : t) : bool =
  List.for_all (Words.is_valid words) (get_words g)

let remove_words_at (words : Words.t) (x : int) (y : int) (g : t) : t =
  let res = copy g in
  let dimx, dimy = get_dims res in
  (match Line.get_word_bounds_at y (get_row x res) with
  | None -> ()
  | Some (hstart, hend) ->
      for i = hstart to hend do
        res.(x).(i) <- None
      done);
  (match Line.get_word_bounds_at x (get_col y res) with
  | None -> ()
  | Some (vstart, vend) ->
      for i = vstart to vend do
        res.(i).(y) <- None
      done);
  let i : int ref = ref 0 in
  let j : int ref = ref 0 in
  while not (is_valid words res) do
    incr i;
    if !i > pred dimx then (
      incr j;
      i := 0);
    if !j > pred dimy then j := 0;
    if
      not
        (Line.is_valid_word_at words !i (get_col !j res)
        && Line.is_valid_word_at words !j (get_row !i res))
    then res.(!i).(!j) <- None
  done;
  res

let remove_random_word (words : Words.t) (g : t) : t =
  let dimx, dimy = get_dims g in
  let x : int option ref = ref None in
  let y : int option ref = ref None in
  let current_index : int ref = ref 0 in
  let n = count_some g in
  assert (n > 0);
  let target_index : int = Random.int n in
  for i = 0 to pred dimx do
    for j = 0 to pred dimy do
      if Option.is_some g.(i).(j) then
        if !current_index = target_index then (
          x := Some i;
          y := Some j)
        else incr current_index
    done
  done;
  remove_words_at words (Option.get !x) (Option.get !y) g

let insert_random_word (words : Words.t) (g : t) : t =
  let dimx, dimy = get_dims g in
  let res : t ref = ref (copy g) in
  let found : bool ref = ref false in
  while not !found do
    let x = Random.int dimx in
    let y = Random.int dimy in
    let w = Words.choose words in
    if Random.bool () then
      for i = 0 to pred (String.length w) do
        if not (out_of_bounds dimx dimy x (y + i)) then
          !res.(x).(y + i) <- Some w.[i]
      done
    else
      for i = 0 to pred (String.length w) do
        if not (out_of_bounds dimx dimy (x + i) y) then
          !res.(x + i).(y) <- Some w.[i]
      done;
    if is_valid words !res then found := true else res := copy g
  done;
  !res
