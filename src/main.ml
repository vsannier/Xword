(** Command-line application
    @author Victor Sannier *)

(** Usage string for the command-line application *)
let usage = "Usage: xword [-h <int>] [-w <int>] [-kmax <int>] <path>"

(** Pointer to the height of the puzzle to generate *)
let height : int ref = ref 9

(** Pointer to the width of the puzzle to generate *)
let width : int ref = ref 9

(** Pointer to the number of iterations of the simulated annealing algorithm *)
let kmax : int ref = ref 50000

(** Pointer to the path of the file that contains the list of allowed words *)
let filepath : string ref = ref ""

(** Specification of the command-line arguments *)
let specifcation =
  [
    ("-h", Arg.Set_int height, "Set the height of the puzzle (default: 9)");
    ("-w", Arg.Set_int width, "Set the width of the puzzle (default: 9)");
    ( "-kmax",
      Arg.Set_int kmax,
      "Set the number of iterations of the algorithm (default: 50000)" );
  ]

let () =
  Random.self_init ();
  Arg.parse specifcation (fun path -> filepath := path) usage;
  let words = Words.load !filepath in
  print_string "[INFO] Word list loaded into memory\n";
  Array.fast_sort String.compare words;
  let (puzzle, score) : Grid.t * float =
    Annealing.generate_puzzle !width !height !kmax words
  in
  assert (Grid.is_valid words puzzle);
  Grid.print puzzle;
  Printf.printf "[INFO] Percentage of empty squares: %.0f%%\n"
    (100.0 *. score /. Float.of_int (!height * !width))
