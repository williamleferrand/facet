(* Simple usage example of facet *)

module FST =
struct 
  type e = { compensation: bool ; keywords: string list } with facet
  let l = ()
  let s = create__e__ () 
  let insert = insert__e__ l s
  let search = search__e__ l s
end 

include (FST) 

let e1 = { compensation = true ; keywords = [ "ocaml"; "cool" ] } ;; 
let e2 = { compensation = false ; keywords = [ "python"; "pascool" ] } ;; 

insert e1 ;;
insert e2 ;; 

let r = search ~compensation:`All ~keywords:`All ;; 

List.iter (fun e -> Printf.printf "compensation: %b ; keywords: %s\n" e.compensation (String.concat ", " e.keywords)) r
