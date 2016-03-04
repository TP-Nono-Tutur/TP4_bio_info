

(* renvoi la liste des indices de début de la sous chaine [substring] présent dans la chaien [string]*)
let naive_substring_search substring string =
  let string_l = String.length string in
  let substring_l = String.length substring in
  let rec aux acc = function
    | i when i = string_l - (substring_l - 1) -> acc
    | i -> let acc2 =
	     if (String.sub string i substring_l) = substring
	     then i::acc
	     else acc
	   in aux acc2 (i+1)
  in List.rev (aux [] 0)


let search_fasta_naive genome_file string =
  (* extraction du de la sequence  *)
  let (_,seq) = Fasta.extract_first_seq (Fasta.of_file genome_file) in
  (* on extrait les positions de la sequences recherché sous forme de liste *)
  let i_list = naive_substring_search string seq in
  (* On l'affiche *)
  List.iter (fun x -> Printf.printf "%d " x) i_list

let suffix_array seq_file =
  let seq = Fasta.extract_first_seq (Fasta.of_file seq_file) in
  let i_array = Sequence.extract_suffix_array seq in
  Array.iter (fun x -> Printf.printf "%d " x) i_array



	     
let search_fasta_sa genome_file read_seq =
  let seq = Fasta.extract_first_seq (Fasta.of_file genome_file) in
  let l = Sequence.search read_seq seq in
  List_mp.display (print_int) l
	     
  
(* fonction calculant le temps d'execution d'une fonction passé en paramètre *)
let chrono f =
  let t1 = Unix.gettimeofday() in
  f ();
  let t2 = Unix.gettimeofday() in
  t2 -. t1

	   
let main =
  let commande = Sys.argv.(1) in
  match commande with
  | "test" ->
     let string = Sys.argv.(2)
     in print_endline string
  | "search-fasta-naive" ->
     let genome_file = Sys.argv.(2)
     and seq_file = Sys.argv.(3)
     in let time = chrono (fun () -> search_fasta_naive genome_file seq_file)
	in Printf.printf "\n\nexecuté en %f secondes" time
  | "suffix-array" ->
     let genome_file = Sys.argv.(2)
     in let time = chrono (fun () ->  suffix_array genome_file)
	in Printf.printf "\n\nexecuté en %f secondes" time
  | "search-fasta-sa" ->
     let genome_file = Sys.argv.(2)
     and read_seq =  Sys.argv.(3)
     in  let time = chrono (fun () ->  search_fasta_sa genome_file read_seq)
	 in Printf.printf "\n\nexecuté en %f secondes" time
  | _ -> prerr_endline "commande inconue"
	       
