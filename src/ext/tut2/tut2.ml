open Cil
open Feature

module E = Errormsg


open Tututil 


class assignRmVisitor (vname : string) = object(self)
  inherit nopCilVisitor 

  method vinst (i : instr) = 
    match i with
    | Set((Var vi, NoOffset), _, loc) when vi.vname = vname && vi.vglob ->
      E.log "%a: Assignment deleted: %a\n" d_loc loc d_instr i;
      ChangeTo [] 
    | _ -> SkipChildren 

end


let processFunction ((tf, tv) : string * string) (fd : fundec) (loc : location) : unit =
  if fd.svar.vname <> tf then () else begin
    let vis = new assignRmVisitor tv in
    ignore(visitCilFunction vis fd)
  end


let tut2 (funvar : string * string) (f : file) : unit =
  funvar |> processFunction |> onlyFunctions |> iterGlobals f

let feature = 
  { fd_name = "tut2";
    fd_enabled = false;
    fd_description = "tut2";
    fd_extraopt = [];
    fd_doit = tut2 ("foo", "bar");
    fd_post_check = true;
  } 

let () = Feature.register feature
