(* Stress test for facet *)



module Index = 
  struct 
    type e = 
	{ 
	  uid: int ;
	  compensation: bool ; 
	  keywords: string list
	  } with facet index (compensation, keywords) 
	    
    exception NoLoader

    let l : e Facet.Lifesaver.t = Facet.Lifesaver.create (try int_of_string (Sys.argv.(2)) with _ -> 10000000) 400000000000.0
    let s = create__e__ () 
    let insert = insert__e__ l s 
    let search = search__e__ l (fun uid -> Lwt.fail NoLoader) s
   (* let display e = Printf.printf "compensation: %b ; keywords: %s\n" e.compensation (String.concat ", " e.keywords)
   *) 
  end

include (Index)

Random.init 0 ;; 

let keywords = [|
"ocaml"; 
"cours"; 
"telecharger";
"caml";
"mli";
"ocaml";
"objective";
"ocaml inria";
"ocaml manual";
"erlang c";
"inria ocaml";
"tutoriel ocaml";
"tutorial ocaml";
"da ocaml";
"ocaml windows";
"compiler ocaml";
"tuto ocaml";
"ocaml string";
"graphics ocaml";
"type ocaml";
"ocaml liste";
"ocaml array";
"ide ocaml" |] 

let rec generate_random_keywords = function  
  | 0 -> [] 
  | n -> keywords.(21) :: (generate_random_keywords (n-1))

let generate_random i = 
  {
    uid = i; 
    compensation = Random.bool () ; 
    keywords = generate_random_keywords (Random.int (try int_of_string Sys.argv.(3) with _ -> 5))
  }

let memorize = ref [] 
let rec populate = function 
  | 0 -> () 
  | n -> let e = generate_random n in memorize := e :: !memorize ; insert e;  populate (n-1)

open Lwt 

let _ = 
  Lwt_main.run (
    let size = int_of_string Sys.argv.(1) in
    let t1 = Unix.times () in 
    populate size; 
    let t2 = Unix.times () in 


  search ~compensation: (`All) ~keywords: (`OrWeak [ "ocaml" ]) 0 15
    >>= fun r ->
  let t3 = Unix.times () in 
  Printf.printf "%d documents inserted in %f seconds\n" size (t2.Unix.tms_stime -. t1.Unix.tms_stime); 
  Printf.printf "%d documents founds among %d documents in %f seconds\n" (List.length r) size (t3.Unix.tms_stime -. t2.Unix.tms_stime); 
  return ()) 
    
