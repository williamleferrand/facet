(* Simple usage example of facet *)

(* module FST =
struct *) 

type 'a outt = compensation:[ `All | `Exact of bool ] -> keywords:[ `All | `Exact of string ] -> 'a list 

  type e = { compensation: bool ; keywords: string list } with facet
  let l = ()
  let s = create__e__ () 
  let insert = insert__e__ l s
  let search = search__e__ l s
  let display e = Printf.printf "compensation: %b ; keywords: %s\n" e.compensation (String.concat ", " e.keywords)
(* end 

include (FST) 
*)

let e1 = { compensation = true ; keywords = [ "ocaml"; "cool" ] } ;; 
let e2 = { compensation = false ; keywords = [ "python"; "pascool" ] } ;; 

insert__e__ () s e1 ;;
insert__e__ () s e2 ;; 

let ra = search__e__  () s `All `All ;; 
let rb = search__e__  () s (`Exact true) `All ;; 

List.iter display ra;;
print_endline "\n"; 
List.iter display rb;;
 
