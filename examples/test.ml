(* Just a test *)

(* Best practise: write the first accessed labels first *)
 
module Lifesaver = 
  struct 
    type 'a t = unit 
  end

type e = { compensation: bool ; keywords: string list } with facet
	
let test_prefix prefix = 
  let l = String.length prefix in fun s -> String.length s >= l && String.sub s 0 l = prefix  

let lifesaver = ()

let s = create__e__ () 

let e1 = { compensation = true ; keywords = [ "ocaml"; "cool" ] } ;; 
let e2 = { compensation = false ; keywords = [ "python"; "pascool" ] } ;; 

insert__e__ () s e1 ;;
insert__e__ () s e2 ;; 

let r = search__e__ () s ~compensation:`All ~keywords:`All ;; 

List.iter (fun e -> Printf.printf "compensation: %b ; keywords: %s\n" e.compensation (String.concat ", " e.keywords)) r
