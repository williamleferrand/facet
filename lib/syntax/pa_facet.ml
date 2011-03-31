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

  (* Create the search function ****************************************************)
  (* ($lid:label$ = `All) *)
    (*
  let rec build_pattern _loc id ty = 
    match ty with 
      | <:ctyp< $lid:label$ : $lid:_$ >> -> [ <:patt< ? ($lid:label$ = `All) >> ] 
      | <:ctyp< $lid:label$ : $lid:_$; $ty$ >> -> [ <:patt< $build_pattern _loc id ty$ >> ] 
    *)										

    let rec create_search_by_field _loc id ty = 
    match ty with 
      | <:ctyp< $lid:label$ : $_$ >> -> <:expr< fun [ ? $lid:label$ -> () ] >>
      | <:ctyp< $lid:label$ : $_$ ; $ty$ >> -> <:expr< fun [ ? $lid:label$ -> $create_search_by_field _loc id ty$ ] >>
	
	
    let create_search _loc id ty = 
      <:str_item< value $lid:"search__"^id^"__"$ l =
      $create_search_by_field _loc id ty$ 
      >>

  (* Create the insert function ****************************************************)
    
  let rec create_insert_by_field _loc id ty = 
    match ty with 
      | <:ctyp< $lid:label$ : bool >> | <:ctyp< $lid:label$ : string >> -> 
	<:expr< fun e h -> Hashtbl.add h e.$lid:label$ e >>
      | <:ctyp< $lid:label$ : list string >> -> 
	<:expr< fun e h -> List.iter (fun s -> Hashtbl.add h s e) e.$lid:label$ >>
      |  <:ctyp< $lid:label$ : bool ; $ty$ >> -> 
      <:expr< 
	fun e h -> 
	  let h = 
	    try 
	      Hashtbl.find h e.$lid:label$ 
	    with [ Not_found -> let nt = Hashtbl.create 0 in do { Hashtbl.add h e.$lid:label$ nt; nt } ] in  
	  $create_insert_by_field _loc id ty$ e h >>

  let create_insert _loc id ty =
    	<:str_item< value $lid:("insert__"^id^"__")$ l h e = 
           $create_insert_by_field _loc id ty$ e h
	>>

	    
  (* Create the type that handles the weak pointers **********************************)

  let rec iter_over_fields_ _loc tp = 
    match tp with 
      | <:ctyp< $lid:_$ : string >> | <:ctyp< $lid:_$ : list string >> -> <:ctyp< Hashtbl.t string (Weak.t t) >>  
      | <:ctyp< $lid:_$ : bool >> -> <:ctyp< Hashtbl.t bool (Weak.t t) >>  
      | <:ctyp< $lid:_$ : bool; $tp$ >> -> <:ctyp< Hashtbl.t bool ($iter_over_fields_ _loc tp$) >>  
       | _ -> raise BadType

  let iter_over_fields _loc tp = 
    match tp with 
      | <:ctyp< { $list:l$ } >> -> l, iter_over_fields_ _loc l 
      | _ -> raise BadType

  let type_of_the_structure _loc tp = 
    match tp with 
      | Ast.TyDcl (loc, id, e, ty, []) -> 
	let fields, ty = iter_over_fields _loc ty in
	id, fields, Ast.TyDcl (loc, "structure__"^id^"__", e, ty, []) 
      | _ -> raise BadType

EXTEND Gram
    str_item: 
    [
      [ 
	 "type"; tds = type_declaration; "with" ; "facet" ->
	 let id, fields, structure_type = type_of_the_structure _loc tds in
	 <:str_item<
	   
      type $tds$;  
      (* Create the t_structure type *)
      type $structure_type$; 
      (* Builds the create function *)
      value $lid:"create__" ^ id ^"__"$ () = Hashtbl.create 0 ;
      (* Builds the insert function *) 
      $create_insert _loc id fields$ ; 
      (* Builds the search function *)
      $create_search _loc id fields$ ; 
	>>
      ]	  
    ];
  END

end

module M = Register.OCamlSyntaxExtension(Id)(Make)


