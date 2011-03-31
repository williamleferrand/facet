(* Simple usage example of facet *)

exception CantLoad

type 'a outt = compensation:[ `All | `Exact of bool ] -> keywords:[ `All | `Exact of string ] -> 'a list 
    
type e = { uid: string ; compensation: bool ; keywords: string list } with facet index (compensation, keywords)

let l = Facet.Lifesaver.create 100 400000000.0

  let s = create__e__ () 
  let insert = insert__e__ l s
  let search = search__e__ l (fun uid -> Lwt.fail CantLoad) s
  let display e = Printf.printf "compensation: %b ; keywords: %s\n" e.compensation (String.concat ", " e.keywords)
(* end 

include (FST) 
*)

let e1 = { uid = "1" ; compensation = true ; keywords = [ "ocaml"; "cool" ] } ;; 
let e2 = { uid = "2"; compensation = false ; keywords = [ "python"; "pascool" ] } ;; 

insert e1 ;;
insert e2 ;; 

open Lwt 

let _ = 
  search ~compensation:`All ~keywords:`All >>= Lwt_list.iter_s (fun e -> display e ; return ())
  >>= fun () -> print_endline ""; 
  search ~uid:`All ~compensation:(`Exact true) ~keywords: `All  >>= Lwt_list.iter_s (fun e -> display e ; return ()) 
