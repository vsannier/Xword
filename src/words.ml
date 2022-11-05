(** Word validation and operations
    @author Victor Sannier *)

type t = string array

let length : t -> int = Array.length

let mem (word : string) (words : t) : bool =
  let found : bool ref = ref false in
  let left : int ref = ref 0 in
  let right : int ref = ref (pred (length words)) in
  while !left < !right && not !found do
    let middle = (!left + !right + 1) / 2 in
    if words.(middle) = word then found := true
    else if words.(middle) < word then left := middle
    else right := pred middle
  done;
  if !left = !right && words.(!left) = word then true else !found

(** Determine whether a word is valid,
    i.e. whether it is one letter or less in length
    or part of the set of allowed words *)
let is_valid (words : t) (w : string) : bool =
  String.length w < 2 || mem w words

(** Get a random string by choosing an index and iterating the words *)
let choose (words : t) : string = words.(Random.int (length words))

(** Load words from a file *)
let load (path : string) : t =
  let read_lines filepath =
    let lines = ref [] in
    let channel = open_in filepath in
    try
      while true do
        lines := input_line channel :: !lines
      done;
      !lines
    with End_of_file ->
      close_in channel;
      List.rev !lines
  in
  let wordlist =
    read_lines path |> List.filter (fun w -> String.length w <> 0)
  in
  let words = Array.of_list wordlist |> Array.map String.uppercase_ascii in
  Array.fast_sort String.compare words;
  words
