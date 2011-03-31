 (*
  * Facet
  *
  * 03 30 2011
  *)

open Camlp4

module Id : Sig.Id =
struct
  let name = "pa_facet"
  let version = "0.1"
end




module Make (Syntax : Sig.Camlp4Syntax) =
struct
  open Sig
  include Syntax

  (* Misc helpers ********************************************************************)

  exception BadType
    
  (* Create the type that handles the weak pointers **********************************)

  let rec iter_over_fields_ _loc tp = 
    match tp with 
      | <:ctyp< $lid:_$ : string >> | <:ctyp< $lid:_$ : list string >> -> <:ctyp< Hashtbl.t string (Weak.t t) >>  
      | <:ctyp< $lid:_$ : bool >> -> <:ctyp< Hashtbl.t bool (Weak.t t) >>  
      | <:ctyp< $lid:_$ : bool; $tp$ >> -> <:ctyp< Hashtbl.t bool ($iter_over_fields_ _loc tp$) >>  

  let iter_over_fields _loc tp = 
    match tp with 
      | <:ctyp< { $list:l$ } >> -> iter_over_fields_ _loc l 
      | _ -> raise BadType

  let type_of_the_structure _loc tp = 
    match tp with 
      | Ast.TyDcl (loc, id, e, ty, []) -> Ast.TyDcl (loc, id ^ "_structure", e, iter_over_fields _loc ty, []) 
      | _ -> raise BadType

EXTEND Gram
    str_item: 
    [
      [ 
	 "type"; tds = type_declaration; "with" ; "facet" ->
	 
	 <:str_item<
	     (* add a _id field *) 
      type $tds$;  
      type $type_of_the_structure _loc tds$; 
	>>
      ]	  
    ];
  END

end

module M = Register.OCamlSyntaxExtension(Id)(Make)


