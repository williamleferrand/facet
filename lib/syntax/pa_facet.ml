 (*
  * Facet
  *
  * 03 30 2011
  *)

open Camlp4

module Id : Sig.Id =
struct
  let name = "pa_facet"
  let version = "0.1"
end


module Make (Syntax : Sig.Camlp4Syntax) =
struct
  open Sig
  include Syntax


EXTEND Gram
    str_item: 
    [
      [ 
	 "type"; tds = type_declaration; "with" ; "facet" ->
	 
	 <:str_item<
	     (* add a _id field *) 
              type $tds$;   >>
      ]	  
    ];
  END

end

module M = Register.OCamlSyntaxExtension(Id)(Make)


