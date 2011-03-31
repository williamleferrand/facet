type e = { compensation : bool; keywords : string list }

let test_prefix prefix =
  let l = String.length prefix
  in fun s -> ((String.length s) >= l) && ((String.sub s 0 l) = prefix)
  
module E = struct type t = e
                   let compare = compare
                      end
  
module S = Set.Make(E)
  
type structure__e__ = (bool, (string, e Weak.t) Hashtbl.t) Hashtbl.t

let create__e__ () = Hashtbl.create 0
  
let insert__e__ l h e =
  (fun e h ->
     let h =
       try Hashtbl.find h e.compensation
       with
       | Not_found ->
           let nt = Hashtbl.create 0 in (Hashtbl.add h e.compensation nt; nt)
     in (fun e h -> List.iter (fun s -> Hashtbl.add h s e) e.keywords) e h)
    e h
  
let search__e__ l h =
  let f ~compensation =
    let f ~keywords =
      S.elements
        ((fun h acc ->
            match compensation with
            | `All ->
                Hashtbl.fold
                  (fun _ v acc ->
                     (fun h acc ->
                        match keywords with
                        | `All ->
                            Hashtbl.fold (fun _ v acc -> S.add v acc) h acc
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
                          Hashtbl.fold (fun _ v acc -> S.add v acc) h acc
                      | `Exact k ->
                          (try S.add (Hashtbl.find h k) acc
                           with | Not_found -> acc))
                     (Hashtbl.find h k) acc
                 with | Not_found -> acc))
           h S.empty)
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
  
let ra = search__e__ () s ~compensation: `All ~keywords: `All
  
let rb = search__e__ () s ~compensation: (`Exact true) ~keywords: `All
  
let _ = List.iter display ra
  
let _ = (print_endline "\n"; List.iter display rb)
