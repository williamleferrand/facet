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

  (* Ocsigen UI **********************************************************************)
    
  (* Create the search function ******************************************************)
 
    let rec create_expr _loc id ty = 
      match ty with 
	| [ <:ctyp< $lid:label$ : bool >> ] ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 then ((pad-1),a) else if S.cardinal a <= d then (0, S.add v a) else acc) h acc
		  | `Exact k -> List.fold_left (fun ((pad, a) as acc) v -> if pad > 0 then ((pad-1), a) else if S.cardinal a <= d then (0, S.add v a) else acc) acc (Hashtbl.find_all h k) 
		  (* | `Exact k -> try ((c+1), S.add (Hashtbl.find h k) acc) with [ Not_found -> a ] *) ] >>

	| [ <:ctyp< $lid:label$ : string >> ] ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 then ((pad-1),a) else if S.cardinal a <= d then (0, S.add v a) else acc) h acc
		  | `Exact k -> List.fold_left (fun ((pad, a) as acc) v -> if pad > 0 then ((pad-1), a) else if S.cardinal a <= d then (0, S.add v a) else acc) acc (Hashtbl.find_all h k) 
		  | `Prefix s -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if Str.string_match (Str.regexp_string_case_fold s) k 0 then (if pad > 0 then ((pad-1, a)) else if S.cardinal a <= d then (0, S.add v a) else acc) else acc) h acc
		  | `Contains s -> Hashtbl.fold (fun k v ((pad, a) as acc) -> try ignore (Str.search_forward (Str.regexp_string_case_fold s) k 0) ; 
										if pad > 0 then ((pad-1),a) else if S.cardinal a <= d then (0, S.add v a) else acc with [ Not_found -> acc]) h acc ] >>

	| [ <:ctyp< $lid:label$ : list string >> ] ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 then ((pad-1),a) else if S.cardinal a <= d then (0, S.add v a) else acc) h acc
		  | `Exact k -> List.fold_left (fun ((pad, a) as acc) v -> if pad > 0 then ((pad-1), a) else if S.cardinal a <= d then (0, S.add v a) else acc) acc (Hashtbl.find_all h k) 
		  | `Or l -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if List.mem k l then (if pad > 0 then (pad-1, a) else if S.cardinal a <= d then (0, S.add v a) else acc) else acc) h acc 
		  | `OrWeak l -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if list_mem_weak k l then (if pad > 0 then (pad-1, a) else if S.cardinal a <= d then (0, S.add v a) else acc) else acc) h acc 
		  ] >>

	| [ <:ctyp< $lid:label$ : int >> ] ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 then ((pad-1),a) else if S.cardinal a <= d then (0, S.add v a) else acc) h acc
		  | `Exact k -> List.fold_left (fun ((pad, a) as acc) v -> if pad > 0 then ((pad-1), a) else if S.cardinal a <= d then (0, S.add v a) else acc) acc (Hashtbl.find_all h k) 
		  | `About (i, m) -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k <= i + m && k >=i-m then (if pad > 0 then (pad-1, a) else if S.cardinal a <= d then (0, S.add v a) else acc) else acc) h acc
		  | `Inf i -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k <= i then (if pad > 0 then (pad-1, a) else if S.cardinal a <= d then (0, S.add v a) else acc) else acc) h acc
		  | `Sup i -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k >= i then (if pad > 0 then (pad-1, a) else if S.cardinal a <= d then (0, S.add v a) else acc) else acc) h acc ] >>

	| [ <:ctyp< $lid:label$ : date >> ] ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 then ((pad-1),a) else if S.cardinal a <= d then (0, S.add v a) else acc) h acc
		  | `Exact k -> List.fold_left (fun ((pad, a) as acc) v -> if pad > 0 then ((pad-1), a) else if S.cardinal a <= d then (0, S.add v a) else acc) acc (Hashtbl.find_all h k)
		  | `Before i -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k <= i then (if pad > 0 then (pad-1, a) else if S.cardinal a <= d then (0, S.add v a) else acc) else acc) h acc
		  | `After i -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k >= i then (if pad > 0 then (pad-1, a) else if S.cardinal a <= d then (0, S.add v a) else acc) else acc) h acc ] >>

	| [ <:ctyp< $lid:label$ : period >> ] ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 then ((pad-1),a) else if S.cardinal a <= d then (0, S.add v a) else acc) h acc
 		  | `Overlaps (p1, p2) -> Hashtbl.fold (fun (v1, v2) v ((pad, a) as acc) -> if  not (p2 < v1 || p1 > v2) then (if pad > 0 then (pad-1, a) else if S.cardinal a <= d then (0, S.add v a) else acc) else acc) h acc 
		  ] >> 
	   
	| <:ctyp< $lid:label$ : bool >> :: ty -> 
	   <:expr<
	    fun h ((pad, a) as acc) -> 
	      let __nxt = $create_expr _loc id ty$ in
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 || S.cardinal a <= d then __nxt v acc else acc) h acc 		    
		  | `Exact k -> try __nxt (Hashtbl.find h k) acc with [ Not_found -> acc ] ] >> 

	| <:ctyp< $lid:label$ : string >> :: ty ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      let __nxt = $create_expr _loc id ty$ in
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 || S.cardinal a <= d then __nxt v acc else acc) h acc
		  | `Exact k -> try __nxt (Hashtbl.find h k) acc with [ Not_found -> acc ] 
		  | `Prefix s -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if Str.string_match (Str.regexp_string_case_fold s) k 0 && (pad > 0 || S.cardinal a <= d) then __nxt v acc else acc) h acc
		  | `Contains s -> Hashtbl.fold (fun k v ((pad, a) as acc) -> try ignore (Str.search_forward (Str.regexp_string_case_fold s) k 0) ; 
								  if pad > 0 || S.cardinal a <= d then __nxt v acc else acc with [ Not_found -> acc]) h acc ] >>

	| <:ctyp< $lid:label$ : list string >> :: ty ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      let __nxt = $create_expr _loc id ty$ in
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 || S.cardinal a <= d then __nxt v acc else acc) h acc
		  | `Exact k -> try __nxt (Hashtbl.find h k) acc with [ Not_found -> acc ] 
		  | `Or l -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if List.mem k l && (pad > 0 || S.cardinal a <= d) then __nxt v acc else acc) h acc 
		  | `OrWeak l -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k <> "" && list_mem_weak k l && (pad > 0 || S.cardinal a <= d) then __nxt v acc else acc) h acc ] >>

        | <:ctyp< $lid:label$ : int >> :: ty ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      let __nxt = $create_expr _loc id ty$ in
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 || S.cardinal a <= d then __nxt v acc else acc) h acc
		  | `Exact k -> try __nxt (Hashtbl.find h k) acc with [ Not_found -> acc ]
		  | `About (i, m) -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k <= i + m && k >=i-m && (pad > 0 || S.cardinal a <= d) then __nxt v acc else acc) h acc
		  | `Inf i -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k <= i && (pad > 0 || S.cardinal a <= d) then __nxt v acc else acc) h acc
		  | `Sup i -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k >= i && (pad > 0 || S.cardinal a <= d) then __nxt v acc else acc) h acc ] >>

	| <:ctyp< $lid:label$ : date >> :: ty ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      let __nxt = $create_expr _loc id ty$ in
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 || S.cardinal a <= d then __nxt v acc else acc) h acc
		  | `Exact k -> try __nxt (Hashtbl.find h k) acc with [ Not_found -> acc ]  
		  | `Before i -> Hashtbl.fold (fun k v ((pad, a) as acc) -> if k <= i && (pad > 0 || S.cardinal a <= d) then __nxt v acc else acc) h acc
		  | `After i -> Hashtbl.fold (fun k v  ((pad, a) as acc) -> if k >= i && (pad > 0 || S.cardinal a <= d) then __nxt v acc else acc) h acc ] >>

	| <:ctyp< $lid:label$ : period >> :: ty ->
	  <:expr<
	    fun h ((pad, a) as acc) -> 
	      let __nxt = $create_expr _loc id ty$ in
	      match $lid:label$ with 
		  [ `All -> Hashtbl.fold (fun _ v ((pad, a) as acc) -> if pad > 0 || S.cardinal a <= d then __nxt v acc else acc) h acc
 		  | `Overlaps (p1, p2) -> Hashtbl.fold (fun (v1, v2) v ((pad, a) as acc) -> if p2 >= v1 && p1 <= v2 && (pad > 0 || S.cardinal a <= d) then __nxt v acc else acc) h acc 
		  ] >>

	| _ -> failwith "1" 

   let rec create_params params _loc id  ty = 
     match ty with 
       | [ <:ctyp< $lid:label$ : $_$ >> ] -> <:expr< let f ~ $lid:label$ = let (_, wl) = ($create_expr _loc id params$ h (f, S.empty)) in 
									Lwt_list.map_p 
									  (fun 
									    [ (uid, w) -> 
									      match Weak.get w 0 with 
										  [ None -> ld uid 
										  | Some v -> Lwt.return v]]) (S.elements wl) in f >>

       | <:ctyp< $lid:label$ : $_$ >> :: ty -> <:expr< let f ~ $lid:label$ = $create_params params _loc id ty$ in f >>
       | _ -> failwith "2"

   let rec create_type _loc id ty = 
     match ty with 
       | [ <:ctyp< $lid:label$ : bool >> ] -> <:ctyp< ~ $lid:label$ : ([> `All | `Exact of bool ]) -> Lwt.t (list $lid:id$) >> 
       | [ <:ctyp< $lid:label$ : int >> ] -> <:ctyp< ~ $lid:label$ : ([> `All | `Exact of int | `About of (int * int) | `Inf of int | `Sup of int ]) -> Lwt.t (list $lid:id$) >> 
       | [ <:ctyp< $lid:label$ : string >> ] -> <:ctyp< ~ $lid:label$ : ([> `All | `Exact of string | `Contains of string | `Prefix of string ]) -> Lwt.t (list $lid:id$) >> 
       | [ <:ctyp< $lid:label$ : list string >> ] -> <:ctyp< ~ $lid:label$ : ([> `All | `Exact of string | `Or of list string | `OrWeak of list string ]) -> Lwt.t (list $lid:id$) >> 
       | [ <:ctyp< $lid:label$ : date >> ] -> <:ctyp< ~ $lid:label$ : ([> `All | `Exact of date | `Before of date | `After of date ]) -> Lwt.t (list $lid:id$) >> 
       | [ <:ctyp< $lid:label$ : period >> ] -> <:ctyp< ~ $lid:label$ : ([> `All | `Overlaps of period ]) -> Lwt.t (list $lid:id$) >> 
       | <:ctyp< $lid:label$ : bool >> :: ty  -> <:ctyp<  ~ $lid:label$ :  ([> `All | `Exact of bool ])  -> $create_type _loc id ty$ >>
       | <:ctyp< $lid:label$ : int >> :: ty  -> <:ctyp<  ~ $lid:label$ :  ([> `All | `Exact of int | `About of (int * int) | `Inf of int | `Sup of int ])  -> $create_type _loc id ty$ >>
       | <:ctyp< $lid:label$ : string >> :: ty -> <:ctyp< ~ $lid:label$ : ([> `All | `Exact of string | `Contains of string | `Prefix of string ]) -> $create_type _loc id ty$ >> 
       | <:ctyp< $lid:label$ : list string >> :: ty -> <:ctyp<  ~ $lid:label$ : ([> `All | `Exact of string | `Or of list string | `OrWeak of list string ]) -> $create_type _loc id ty$ >>
       | <:ctyp< $lid:label$ : date >> :: ty  -> <:ctyp<  ~ $lid:label$ :  ([> `All | `Exact of date | `Before of date | `After of date ])  -> $create_type _loc id ty$ >>
       | <:ctyp< $lid:label$ : period >> :: ty  -> <:ctyp<  ~ $lid:label$ :  ([> `All | `Overlaps of period ])  -> $create_type _loc id ty$ >>
       | l -> Printf.printf "Length of ty %d\n" (List.length l) ; failwith "9"  
 
   let create_search _loc id ty = 
     
     <:str_item< value $lid:"search__"^id^"__"$ l ld h f d : $create_type _loc id ty$  = 
             $create_params ty _loc id ty$ 
     
	
	  >>

  (* Create the insert function *******************************************************)
    
  let rec create_insert_by_field _loc id ty = 
    match ty with 
      | [ <:ctyp< $lid:label$ : bool >> ] | [ <:ctyp< $lid:label$ : string >> ] | [ <:ctyp< $lid:label$ : int >> ] | [ <:ctyp< $lid:label$ : date >> ] | [ <:ctyp< $lid:label$ : period >> ] -> 
	<:expr< fun e w h -> Hashtbl.add h e.$lid:label$ (e.uid, w) >>
      | [ <:ctyp< $lid:label$ : list string >> ] -> 
	<:expr< fun e w h -> List.iter (fun s -> Hashtbl.add h s (e.uid, w)) [ "" :: e.$lid:label$ ] >>
      | <:ctyp< $lid:label$ : bool >> :: ty | <:ctyp< $lid:label$ : string >> :: ty | <:ctyp< $lid:label$ : int >> :: ty | <:ctyp< $lid:label$ : date >> :: ty  | <:ctyp< $lid:label$ : period >> :: ty  -> 
        <:expr< 
	  fun e w h -> 
	    let h = 
	      try 
		Hashtbl.find h e.$lid:label$ 
	      with [ Not_found -> let nt = Hashtbl.create 0 in do { Hashtbl.add h e.$lid:label$ nt; nt } ] in  
	    $create_insert_by_field _loc id ty$ e w h >>
      | <:ctyp< $lid:label$ : list string >> :: ty -> 
      <:expr< 
	fun e w h ->
	  List.iter (fun s ->  
	    let h = 
	      try 
		Hashtbl.find h s
	      with [ Not_found -> let nt = Hashtbl.create 0 in 
		do { Hashtbl.add h s nt; nt } ]  in
	    
	    $create_insert_by_field _loc id ty$ e w h ) [ "" :: e.$lid:label$ ] 
	   >>
      
      | _ -> failwith "4"


  let create_insert _loc id ty =
    <:str_item< value $lid:("insert__"^id^"__")$ l h e = 
    let w = Weak.create 1 in
	do {
	  Facet.Lifesaver.insert l e ; 
	  Weak.set w 0 (Some e);  
          $create_insert_by_field _loc id ty$ e w h }
	>>
	     
	    
  (* Create the type that handles the weak pointers *************************************)

  let rec iter_over_fields_ _loc id index acc tp = 
    match tp with 
     
      | <:ctyp< $lid:l$ : $t$ >> -> if List.mem l index then  <:ctyp< $lid:l$ : $t$ >> :: acc else acc    
      | <:ctyp< $lid:l$ : $lid:t$; $tp'$ >> -> iter_over_fields_ _loc id index (if List.mem l index then (<:ctyp< $lid:l$ : $lid:t$ >>) :: acc else acc) tp'
      | <:ctyp< $lid:l$ : $t$ ; $tp'$ >> -> iter_over_fields_ _loc id index (if List.mem l index then (<:ctyp< $lid:l$ : $t$ >>) :: acc else acc) tp'
       | _ -> failwith "1"

  let iter_over_fields _loc id index tp = 
    match tp with 
      | <:ctyp< { $list:l$ } >> ->
	List.filter 
	  (fun e -> match e with 
	    <:ctyp< $lid:llb$ : $_$ >> -> List.mem llb index 
	  | _ -> false) (Ast.list_of_ctyp l [])
      | _ -> failwith "2" 

  let type_of_the_structure _loc index tp = 
    match tp with 
      | Ast.TyDcl (loc, id, e, ty, []) -> 
	let fields = iter_over_fields _loc id index ty in
	id, fields
      | _ -> failwith "3"

EXTEND Gram
    str_item: 
    [
      [ 
	"type"; tds = type_declaration; "with" ; "facet" ; "index" ; "("; index = LIST0 [ l = LIDENT -> l ] SEP "," ; ")" -> 
	let id, fields= type_of_the_structure _loc index tds in
       
	<:str_item<

      type $tds$;  
      (* A Utility function *)
	
      value test_prefix prefix = let l = String.length prefix in 
				 fun [ s -> String.length s >= l && String.sub s 0 l = prefix ]  ;

      value rec list_mem_weak k = fun [ [] -> False 
				      | [ t::q ] -> 
					try do { ignore (Str.search_forward (Str.regexp_string_case_fold t) k 0) ; True }
					with [ Not_found -> list_mem_weak k q ] ] ; 

      module E = struct 
	type t = (int * (Weak.t $lid:id$)) ;
	value compare = compare;  
      end ;     
	
      module S = Set.Make (E) ; 
  
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
