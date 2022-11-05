(** Line (row or column) operations
    @author Victor Sannier *)

type t = char option array

(** Print a line using an underscore for empty squares *)
let print (l : t) : unit =
  Array.iter
    (fun a ->
      (match a with None -> print_char '_' | Some c -> print_char c);
      print_char ' ')
    l;
  print_string "\b\n"

(** Check whether the line given by its length contains a square given by its index *)
let out_of_bounds (n : int) (i : int) : bool = i < 0 || i > pred n

let get_word_bounds_at (i : int) (l : t) : (int * int) option =
  let n = Array.length l in
  if out_of_bounds n i || Option.is_none l.(i) then None
  else
    let start = ref i in
    let end' = ref i in
    while pred !start > 0 && Option.is_some l.(pred !start) do
      decr start
    done;
    while succ !end' < pred n && Option.is_some l.(succ !end') do
      incr end'
    done;
    Some (!start, !end')

let get_word_at (i : int) (l : t) : string option =
  match get_word_bounds_at i l with
  | None -> None
  | Some (start, end') ->
      Some (String.init (end' - start + 1) (fun j -> Option.get l.(start + j)))

let is_valid_word_at (words : Words.t) (i : int) (l : t) : bool =
  match get_word_at i l with None -> true | Some w -> Words.is_valid words w

let get_words (l : t) : string list =
  let rec get_words_from (i : int) (l : t) (acc : string list) : string list =
    if i > pred (Array.length l) then acc
    else
      match get_word_at i l with
      | None -> get_words_from (succ i) l acc
      | Some w ->
          assert (String.length w > 0);
          get_words_from (i + String.length w) l (w :: acc)
  in
  get_words_from 0 l []

let is_valid (words : Words.t) (l : t) : bool =
  List.for_all (Words.is_valid words) (get_words l)
