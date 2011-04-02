(* Stress test for facet *)
module Index =
  struct
    type e = { uid : int; compensation : bool; keywords : string list }
    
    let test_prefix prefix =
      let l = String.length prefix
      in fun s -> ((String.length s) >= l) && ((String.sub s 0 l) = prefix)
      
    let rec list_mem_weak k =
      function
      | [] -> false__
      | t :: q ->
          (try
             (Str.search_forward (Str.regexp_string_case_fold k) t 0; true__)
           with | Not_found -> list_mem_weak k q)
      
    module E = struct type t = (int * (e Weak.t))
                       let compare = compare
                          end
      
    module S = Set.Make(E)
      
    let create__e__ () = Hashtbl.create 0
      
    let insert__e__ l h e =
      let w = Weak.create 1
      in
        (Facet.Lifesaver.insert l e;
         Weak.set w 0 (Some e);
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
                   ("" :: e.keywords))
                e w h)
           e w h)
      
    let search__e__ l ld h f d :
      compensation: [> | `All | `Exact of bool] ->
        keywords:
          [>
            | `All
            | `Exact of string
            | `Or of string list
            | `OrWeak of string list
          ] -> (e list) Lwt.t =
      let f ~compensation =
        let f ~keywords =
          let wl =
            (fun h a ->
               let __nxt h a =
                 match keywords with
                 | `All ->
                     Hashtbl.fold
                       (fun _ v a ->
                          if ((S.cardinal a) >= f) && ((S.cardinal a) <= d)
                          then S.add v a
                          else a)
                       h a
                 | `Exact k ->
                     List.fold_left
                       (fun a v ->
                          if ((S.cardinal a) >= f) && ((S.cardinal a) <= d)
                          then S.add v a
                          else a)
                       a (Hashtbl.find_all h k)
                 | `Or l ->
                     Hashtbl.fold
                       (fun k v a ->
                          if
                            (List.mem k l) &&
                              (((S.cardinal a) >= f) && ((S.cardinal a) <= d))
                          then S.add v a
                          else a)
                       h a
                 | `OrWeak l ->
                     Hashtbl.fold
                       (fun k v a ->
                          if
                            (list_mem_weak k l) &&
                              (((S.cardinal a) >= f) && ((S.cardinal a) <= d))
                          then S.add v a
                          else a)
                       h a
               in
                 match compensation with
                 | `All ->
                     Hashtbl.fold
                       (fun _ v a ->
                          if ((S.cardinal a) >= f) && ((S.cardinal a) <= d)
                          then __nxt v a
                          else a)
                       h a
                 | `Exact k ->
                     (try __nxt (Hashtbl.find h k) a with | Not_found -> a))
              h S.empty
          in
            Lwt_list.map_p
              (fun (uid, w) ->
                 match Weak.get w 0 with
                 | None -> ld uid
                 | Some v -> Lwt.return v)
              (S.elements wl)
        in f
      in f
      
    exception NoLoader
      
    let l : e Facet.Lifesaver.t =
      Facet.Lifesaver.create
        (try int_of_string Sys.argv.(2) with | _ -> 10000000) 400000000000.0
      
    let s = create__e__ ()
      
    let insert = insert__e__ l s
      
    let search = search__e__ l (fun uid -> Lwt.fail NoLoader) s
      
    let display e =
      Printf.printf "compensation: %b ; keywords: %s\n" e.compensation
        (String.concat ", " e.keywords)
      
  end
  
include Index
  
let _ = Random.init 0
  
let keywords =
  [| "ocaml"; "cours"; "telecharger"; "caml"; "mli"; "ocaml"; "objective";
    "ocaml inria"; "ocaml manual"; "erlang c"; "inria ocaml";
    "tutoriel ocaml"; "tutorial ocaml"; "da ocaml"; "ocaml windows";
    "compiler ocaml"; "tuto ocaml"; "ocaml string"; "graphics ocaml";
    "type ocaml"; "ocaml liste"; "ocaml array"; "ide ocaml"
  |]
  
let rec generate_random_keywords =
  function
  | 0 -> []
  | n -> keywords.(21) :: (generate_random_keywords (n - 1))
  
let generate_random i =
  {
    uid = i;
    compensation = Random.bool ();
    keywords =
      generate_random_keywords
        (Random.int (try int_of_string Sys.argv.(3) with | _ -> 5));
  }
  
let memorize = ref []
  
let rec populate =
  function
  | 0 -> ()
  | n ->
      let e = generate_random n
      in (memorize := e :: !memorize; insert e; populate (n - 1))
  
open Lwt
  
let _ =
  Lwt_main.run
    (let size = int_of_string Sys.argv.(1) in
     let t1 = Unix.times ()
     in
       (populate size;
        let t2 = Unix.times ()
        in
          (search ~compensation: `All ~keywords: (`OrWeak [ "ocaml" ]) 0 15)
            >>=
            (fun r ->
               let t3 = Unix.times ()
               in
                 (Printf.printf "%d documents inserted in %f seconds\n" size
                    (t2.Unix.tms_stime -. t1.Unix.tms_stime);
                  Printf.printf
                    "%d documents founds among %d documents in %f seconds\n"
                    (List.length r) size
                    (t3.Unix.tms_stime -. t2.Unix.tms_stime);
                  return ()))))
  
