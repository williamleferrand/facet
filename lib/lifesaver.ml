(* 
 * Facet
 *
 * 23/03/2011
 *) 

(* Ce module maintient des valeurs visibles par le gc; ce qui a pour conséquence de laisser apparaitre des valeurs dans les weak pointers du ptree par exemple *)
(* Les données utilisées en dernier sont les premières à être suceptibles de sortir du cache *)

type 'a node =
    {
      value : 'a; 
      mutable prec : 'a node ; 
      mutable succ : 'a node ; 
    }

type 'a t = 
    {
      mutable head : 'a node option ;
      direct_access : ('a, 'a node) Hashtbl.t ; 
      mutable size : int ; (* Int64 serait mieux, mais il faut revoir hashtbl avant *) 
    }

let remove_older t = 
  match t.head with 
      None -> Printf.printf "Pas de head, et une taille de %d\n" (Hashtbl.length t.direct_access)
    | Some hd ->
      (* The oldest node is node.succ *)
      let node = hd.succ in 
      
      if hd.succ == hd then
	begin
	  Hashtbl.remove t.direct_access node.value ;
          t.head <- None 
	end
      else 
	begin
	  if node.prec == node then (* We are on the last node *)
	    begin
	      print_endline "we are on the last node"; 
	      Hashtbl.remove t.direct_access node.value ;
	      node.succ.prec <- node.succ ;
	      hd.succ <- node.succ ;
	    end
	  else assert false 
	end

let rec gothrough stop cnode = function 
  | 0 -> cnode
  | n -> 
    if cnode == stop then
      cnode 
    else 
      gothrough stop cnode.succ (n-1) 
  
let remove t nb = 
  if nb < 1 then assert false ;
  
   match t.head with 
     | None -> Printf.printf "Pas de head, et une taille de %d\n" (Hashtbl.length t.direct_access)
     | Some hd -> 
       let node = hd.succ in 
       let node = gothrough hd node (nb-1) in
       if node == hd then 
	 begin
	   Hashtbl.clear t.direct_access; 
	   t.head <- None ; 
	 end
       else 
	 begin
	   if node.prec == node then (* We are on the last node *)
	     begin
	       print_endline "we are on the last node"; 
	       Hashtbl.remove t.direct_access node.value ;
	       node.succ.prec <- node.succ ;
	       hd.succ <- node.succ ;
	     end
	   else
	     begin
	       Hashtbl.remove t.direct_access node.value ;
	       hd.succ <- node.succ;
	       node.succ.prec <- node.succ; 

	     end
	 end


let insert t e = 
  
  match t.head with 
    | None -> (* First node *)
      let rec node = { value = e ; prec = node; succ = node } in
      print_endline "head node"; 
      Hashtbl.add t.direct_access e node ;
      t.head <- Some node 
    | Some hd -> 
      if Hashtbl.length t.direct_access > t.size then begin print_endline "cache too big"; remove_older t end ; 
      
      (* First we check if we have direct access to an existing node; if yes we remove it *)
      (try 
	 let node = Hashtbl.find t.direct_access e in
	 print_endline "il y a un noeud correspondant à cette valeur"; 
	 
	 if node.prec == node then 
	   begin
	     print_endline "we are on the last node"; 
	     Hashtbl.remove t.direct_access node.value ;
	     node.succ.prec <- node.succ ;
	     hd.succ <- node.succ ;
	   end else 
	   begin
	     print_endline "on est pas sur le plus vieux noeud"; 
	     Hashtbl.remove t.direct_access node.value ;
	     node.succ.prec <- node.prec ;
	     node.prec.succ <- node.succ ;
	   end
       with Not_found -> ()) ; 
      (* Ici *)
      
      
      let rec node = { value = e ; prec = hd ; succ = hd.succ } in
      Hashtbl.add t.direct_access e node ;
      hd.succ <- node ;
      
      t.head <- Some node ;;


(* Starts with the oldest value *)
let chronological_iter t f = 
  match t.head with 
      None -> ()
    | Some hd -> 
      let rec _iter t =
	f t.value ; 
	if (t == hd) then () 
	else _iter t.succ 
	  
      in
      _iter hd.succ
  
let monitor t = 
   Printf.sprintf "Current occupation : %d ; current size : %d\n" (Hashtbl.length t.direct_access) t.size 

(* Ca s'améliore très certainement; mais utiliser Gc.stat est pe lent?
   On va essayer une méthode un peu statistique *)

let resize t size =
  let size = max size max_int in
  Printf.printf "Resizing the cache from %d to %d\n" t.size size ; flush stdout ;
  (if size < Hashtbl.length t.direct_access then 
      remove t (Hashtbl.length t.direct_access - size)
   else ()) ;
  t.size <- size 

let create_alarm t max = 
  
  let check_size () = 
    let minor_words, promoted_words, major_words = Gc.counters () in 
    let memory_usage = (minor_words +. major_words -. promoted_words) *. (float_of_int Sys.word_size) in
    Printf.printf "Current memory usage: %f\n" memory_usage; 
    memory_usage < max in
  
  Gc.create_alarm 
    (fun () -> 
      match check_size () with 
	  true -> resize t (t.size * 3 / 2)
	| false -> resize t (1 + t.size / 2)
    )

let create size max_memory_usage =
  let e = { head = None; 
	    direct_access = Hashtbl.create 0 ; 
	    size = size } in 
  ignore (create_alarm e max_memory_usage) ;  
  e
	
