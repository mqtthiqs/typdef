#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open MLast

module Sset = Set.Make(String)

let old_parse_implem = !Pcaml.parse_implem

let list_inter l1 l2 =
  List.filter (fun x -> List.mem x l2) l1

let names_of tds : string list =
  List.fold_right (fun td acc -> snd td.tdNam :: acc) tds []
    
let typdefs_of ml =
  List.fold_left 
    (fun s (si,_) -> 
       match si with 
	 | StTyp(_,tds) -> List.fold_right Sset.add (names_of tds) s
	 | _ -> s
    ) Sset.empty ml
    
let rec combine_typdefs td_implem str_items sig_items =
  match str_items, sig_items with
    | _, [] -> 
	Printf.printf"THE END\n";
	str_items
    | ((StTyp(l1,str_tds), l1') :: str_items), 
	((SgTyp(l2,sig_tds), l2') :: sig_items) ->
	let td_implem = List.fold_right Sset.remove (names_of str_tds) td_implem in
    	if list_inter (names_of sig_tds) (names_of str_tds) = []
	then (* no common definition, we put one after the other *)
	  (StDcl (l1', [StTyp(l1,str_tds); StTyp(l2,sig_tds)]), l1') ::
	    combine_typdefs td_implem str_items sig_items    
	else (* merge both typdefs into a mutually dependent type *)
	  (StTyp(l1', str_tds @ sig_tds), l2') ::
	    combine_typdefs td_implem str_items sig_items
    | _, ((SgTyp(l, tds), l') :: sig_items) -> (* cas _, type : *)
	(* Si un de ces tds est défini plus tard *)
	if List.exists (fun s -> Sset.mem s td_implem) (names_of tds)
	then
	  (* Alors on continue en oubliant cette entrée *) (* TODO faut pas l'oublier! *)
	  (print_string "on attend\n";
	  combine_typdefs td_implem str_items sig_items)
	else
	  (* Sinon on la met maintenant, et on continue *)
	  (print_string "on pousse dans le ml\n";
	  (StTyp(l, tds), l') ::
	    combine_typdefs td_implem str_items sig_items)
    | _, ((si, _) :: sig_items) ->				(* cas _, val *)
	Printf.printf "continue %s\n" (Eprinter.apply Pcaml.pr_sig_item Pprintf.empty_pc si);
     	combine_typdefs td_implem str_items sig_items	(* on avance *)

let my_parse_implem str =
  Printf.printf"HELLO\n";
  try
    let  nml = !Pcaml.input_file in
    let nmli = (Filename.chop_extension nml)^".mli" in
    let (mli,b1) = !Pcaml.parse_interf (Stream.of_channel (open_in nmli)) in
    let (ml,b2) = !Pcaml.parse_implem (Stream.of_channel (open_in nml)) in
    combine_typdefs (typdefs_of ml) ml mli, b1 || b2
  with Sys_error _ -> old_parse_implem str
    
let _ =
  Pcaml.parse_implem := my_parse_implem;
 
