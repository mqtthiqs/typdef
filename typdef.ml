#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open MLast

let names_of tds : string list =
  List.fold_right (fun td acc -> snd td.tdNam :: acc) tds []

let old_parse_implem = !Pcaml.parse_implem

let my_parse_implem str =
  try
    let  nml = !Pcaml.input_file in
    let nmli = (Filename.chop_extension nml)^".mli" in
    let (mli,b1) = !Pcaml.parse_interf (Stream.of_channel (open_in nmli)) in
    let types =
      List.fold_right
    	(fun (si, loc) acc ->
    	   match si with
    	     | SgTyp (l,tds) ->
    		 (names_of tds, (l, tds)) :: acc
    	     | _ -> acc
    	) mli [] in
    let (ml,b2) = !Pcaml.parse_implem (Stream.of_channel (open_in nml)) in
    List.map
      (function
  	 | StTyp(l,tds) -> a
      ) ml, b1 && b2

  with Sys_error _ -> old_parse_implem str

let _ =
  Pcaml.parse_implem := my_parse_implem;
