

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


(* renvoi la liste de l'ensemble des couples (i, s) avec s la sous chaine commençant à l'indice i de la chaine [string] (pour tout i allant de 0 à |string|*)
let extract_suffixes string =
  let length = String.length string in
  let rec aux acc = function
    | i when i = length -> acc
    | i ->
       let substring = String.sub string i (length - i)
       in aux ((i, substring)::acc) (i + 1)
  in List.rev (aux [] 0)

let suffix_array seq_file =
  let (_,seq) = Fasta.extract_first_seq (Fasta.of_file seq_file) in
  (* extraction de la liste des suffixes*)
  let suffix_list = extract_suffixes (seq^"$") in
  (*trie de la liste des suffixe*)
  let f (_, s1) (_, s2) = compare s1 s2 in
  let sorted_suffix_list = List.sort f suffix_list in
  (* extraction des indices *)
  let i_list = List.map (fun (i, _) -> i) sorted_suffix_list in
  List.iter (fun x -> Printf.printf "%d " x) i_list

  
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
			 
  | _ -> prerr_endline "commande inconue"
	       
