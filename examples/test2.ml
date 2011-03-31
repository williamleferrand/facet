(* Simple usage example of facet *)
(* module FST =
struct *)
type 'a outt =
  compensation: [ | `All | `Exact of bool ] ->
    keywords: [ | `All | `Exact of string ] -> 'a list

type e = { uid : string; compensation : bool; keywords : string list }

let test_prefix prefix =
  let l = String.length prefix
  in fun s -> ((String.length s) >= l) && ((String.sub s 0 l) = prefix)
  
module E = struct type t = e
                   let compare = compare
                      end
  
module S = Set.Make(E)
  
type structure__e__ =
  (string, (bool, (string, e Weak.t) Hashtbl.t) Hashtbl.t) Hashtbl.t

let create__e__ () = Hashtbl.create 0
  
let insert__e__ l h e =
  let w = Weak.create 1
  in
    (Facet.Lifesaver.insert l e;
     Weak.set w 0 (Some e);
     (fun e w h ->
        let h =
          try Hashtbl.find h e.uid
          with
          | Not_found ->
              let nt = Hashtbl.create 0 in (Hashtbl.add h e.uid nt; nt)
        in
          (fun e w h ->
             let h =
               try Hashtbl.find h e.compensation
               with
               | Not_found ->
                   let nt = Hashtbl.create 0
                   in (Hashtbl.add h e.compensation nt; nt)
             in
               (fun e w h ->
                  List.iter (fun s -> Hashtbl.add h s ((e.uid), w))
                    e.keywords)
                 e w h)
            e w h)
       e w h)
  
let search__e__ l ld h :
  uid: [> | `All | `Exact of string] ->
    compensation: [> | `All | `Exact of bool] ->
      keywords: [> | `All | `Exact of string] -> (S.elt list) Lwt.t =
  let f ~uid =
    let f ~compensation =
      let f ~keywords =
        let wl =
          S.elements
            ((fun h acc ->
                match uid with
                | `All ->
                    Hashtbl.fold
                      (fun _ v acc ->
                         (fun h acc ->
                            match compensation with
                            | `All ->
                                Hashtbl.fold
                                  (fun _ v acc ->
                                     (fun h acc ->
                                        match keywords with
                                        | `All ->
                                            Hashtbl.fold
                                              (fun _ v acc -> S.add v acc) h
                                              acc
                                        | `Exact k ->
                                            (try S.add (Hashtbl.find h k) acc
                                             with | Not_found -> acc))
                                       v acc)
                                  h acc
                            | `Exact k ->
                                (try
                                   (fun h acc ->
                                      match keywords with
                                      | `All ->
                                          Hashtbl.fold
                                            (fun _ v acc -> S.add v acc) h
                                            acc
                                      | `Exact k ->
                                          (try S.add (Hashtbl.find h k) acc
                                           with | Not_found -> acc))
                                     (Hashtbl.find h k) acc
                                 with | Not_found -> acc))
                           v acc)
                      h acc
                | `Exact k ->
                    (try
                       (fun h acc ->
                          match compensation with
                          | `All ->
                              Hashtbl.fold
                                (fun _ v acc ->
                                   (fun h acc ->
                                      match keywords with
                                      | `All ->
                                          Hashtbl.fold
                                            (fun _ v acc -> S.add v acc) h
                                            acc
                                      | `Exact k ->
                                          (try S.add (Hashtbl.find h k) acc
                                           with | Not_found -> acc))
                                     v acc)
                                h acc
                          | `Exact k ->
                              (try
                                 (fun h acc ->
                                    match keywords with
                                    | `All ->
                                        Hashtbl.fold
                                          (fun _ v acc -> S.add v acc) h acc
                                    | `Exact k ->
                                        (try S.add (Hashtbl.find h k) acc
                                         with | Not_found -> acc))
                                   (Hashtbl.find h k) acc
                               with | Not_found -> acc))
                         (Hashtbl.find h k) acc
                     with | Not_found -> acc))
               h S.empty)
        in
          Lwt_list.map_p
            (function | (uid, None) -> ld uid | (_, Some v) -> Lwt.return v)
            wl
      in f
    in f
  in f
  
let l = ()
    (String.concat ", " e.keywords)
  
(* end 

include (FST) 
*)
let e1 = { compensation = true; keywords = [ "ocaml"; "cool" ]; }
  
let e2 = { compensation = false; keywords = [ "python"; "pascool" ]; }
  
let _ = insert__e__ () s e1
  
let _ = insert__e__ () s e2
  
let ra = search__e__ () s `All `All
  
let rb = search__e__ () s (`Exact true) `All
  
let _ = List.iter display ra
  
let _ = (print_endline "\n"; List.iter display rb)
