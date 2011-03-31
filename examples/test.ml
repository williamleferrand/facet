(* Just a test *)

(* Best practise: write the first accessed labels first *)


module type ELT = 
  sig
    type t
    type t_structure 
  end

module E =
  struct 
    type t = 
	{
	  
	  compensation: bool; 

	  keywords : string list ; 
	  	  
	} with facet
	    
  end
