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


  let rec create_aux_by_field _loc id ty =
    match ty with 
    | <:ctyp< $lid:label$ : $_$ >> -> 
      <:expr< 
	 fun c r t -> 
	   match c with 
	       [ `All -> Hashtbl.fold (fun _ v acc -> S.add v acc) t r ] >>
    | <:ctyp< $lid:label$ : $_$ ; $ty$ >> ->
      <:expr< 
	  (fun c r t -> 
	    match c with 
		[ `All -> Hashtbl.fold (fun _ v acc -> $create_aux_by_field _loc id ty$ acc v ) t r ]) $lid:label$ >>
   
    let rec create_expr _loc id ty = 
      match ty with 
	| <:ctyp< $lid:label$ : bool >> | <:ctyp< $lid:label$ : string >> ->
	  <:expr<
	    fun h acc -> 
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v acc -> S.add v acc) h acc ] >>
	| <:ctyp< $lid:label$ : list string >> ->
	  <:expr<
	    fun h acc -> 
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v acc -> S.add v acc) h acc ] >>

	| <:ctyp< $lid:label$ : bool; $ty$ >> | <:ctyp< $lid:label$ : string ; $ty$ >> ->
	  <:expr< 
	    fun h acc -> 
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v acc -> $create_expr _loc id ty$ v acc) h acc ] >>

   let rec create_params params _loc id ty = 
     match ty with 
       | <:ctyp< $lid:label$ : $_$ >> -> <:expr< let f ? ($lid:label$ = `All) = S.elements ($create_expr _loc id params$ h S.empty) in f >>
       | <:ctyp< $lid:label$ : $_$; $ty$ >> -> <:expr< let f ? ($lid:label$ = `All) = $create_params params _loc id ty$ in f >>
	 
   let create_search _loc id ty = 
	  <:str_item< value $lid:"search__"^id^"__"$ l h =
             $create_params ty _loc id ty$ 
     
	
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

  let rec iter_over_fields_ _loc id tp = 
    match tp with 
      | <:ctyp< $lid:_$ : string >> | <:ctyp< $lid:_$ : list string >> -> <:ctyp< Hashtbl.t string (Weak.t $lid:id$) >>  
      | <:ctyp< $lid:_$ : bool >> -> <:ctyp< Hashtbl.t bool (Weak.t $lid:id$) >>  
      | <:ctyp< $lid:_$ : bool; $tp$ >> -> <:ctyp< Hashtbl.t bool ($iter_over_fields_ _loc id tp$) >>  
       | _ -> raise BadType

  let iter_over_fields _loc id tp = 
    match tp with 
      | <:ctyp< { $list:l$ } >> -> l, iter_over_fields_ _loc id l 
      | _ -> raise BadType

  let type_of_the_structure _loc tp = 
    match tp with 
      | Ast.TyDcl (loc, id, e, ty, []) -> 
	let fields, ty = iter_over_fields _loc id ty in
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
	
      module E = struct 
	type t = $lid:id$ ;
	value compare = compare;  
      end ;     
	
      module S = Set.Make (E) ; 

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


