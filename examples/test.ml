(* Just a test *)

(* Best practise: write the first accessed labels first *)
 
module Lifesaver = 
  struct 
    type 'a t = unit 
  end

module type ELT = 
  sig
    type t
    type structure__t__ 

    val lifesaver : t Lifesaver.t 
    val insert__t__ : t Lifesaver.t -> structure__t__ -> t -> unit  
  end


type t = 
    {
      
      compensation : bool ; 
      keywords: string list ;
      
    } with facet
	
let lifesaver = ()
  
  (*
let s= create__t__ ()
  
let e = { compensation = true; 
	  keywords = [ "ocaml"; "cool" ] } ;; 

insert__t__ () s e ;;

create_test ~test:"coucou" () 
  *)

let c :int = search__t__ 
