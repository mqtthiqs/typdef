#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open MLast
open Pcaml

let rec with_constr_td_assoc path : with_constr list -> type_decl = function
  | [] -> raise Not_found
  | WcTyp(loc, p, tv, b, t) :: wcs -> 
      if path=p then {tdNam=loc, List.hd p; tdPrm=tv; tdPrv=b; tdDef=t; tdCon=[]}
      else with_constr_td_assoc path wcs
  | WcMod(loc, p, me) :: wcs -> with_constr_td_assoc path wcs

let rec with_constr_mod_assoc path : with_constr list -> module_expr = function
  | [] -> raise Not_found
  | WcTyp _ :: wcs -> with_constr_mod_assoc path wcs
  | WcMod(loc, p, me) :: wcs ->
      if path=p then me
      else with_constr_mod_assoc path wcs
	
let subst_tds path sigma : type_decl list -> type_decl list =
  List.map
    (fun td ->
       try with_constr_td_assoc (snd td.tdNam :: path) sigma
       with Not_found -> td
    )

let rec subst_mts path sigma 
    : (string * module_type) list -> (string * module_expr) list =
  List.map
    (fun (x,mt) -> 
       try x, with_constr_mod_assoc (x::path) sigma
       with Not_found -> x, types path sigma mt
    )

and types path sigma : module_type -> module_expr = function
  | MtSig(loc, items) -> 
      let l = List.fold_right
	(fun si acc ->
	   match si with
	     | SgTyp(loc, tds) -> StTyp(loc, subst_tds path sigma tds) :: acc
	     | SgMod(loc, b, mts) -> 
		 StMod(loc, b, subst_mts path sigma mts) :: acc
	     | _ -> acc
	) items [] in
      MeStr(loc, l)
  | MtWit(_, mt, wcs) -> types path (wcs @ sigma) mt
  | _ -> raise (Invalid_argument "types")

EXTEND
module_type:
  [[ LIDENT "mli" -> 
       try
	 let nmli = (Filename.chop_extension (!Pcaml.input_file))^".mli" in
	 let (mli,_) = !Pcaml.parse_interf (Stream.of_channel (open_in (nmli))) in
	 MtSig(Ploc.dummy, List.map fst mli)
       with Sys_error _ -> <:module_type<sig end>>
   ]];

module_expr:
  [[ LIDENT "types"; "("; e = module_type; ")" -> 
       types [] [] e
  ]];
END

