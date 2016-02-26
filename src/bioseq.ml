let main =
  let commande = Sys.argv.(1) in
  match commande with
  | "test" ->
     let string = Sys.argv.(2)
     in print_endline string
  | _ -> prerr_endline "commande inconue"
	       
