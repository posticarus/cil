(*

A version of GOLF which greatly simplifies the implementation of
polymorphism.  Instead of having a different instantiation relation,
we just use indexed subtyping. Essentially, we are just eliminating the
well-formedness condition on subtypes.

*)


(***********************************************************************)
(*                                                                     *)
(* Exceptions                                                          *)
(*                                                                     *)
(***********************************************************************)
exception Inconsistent (* raised if constraint system is inconsistent *)
exception WellFormed   (* raised if types are not well-formed *)
exception NoContents   
exception APFound      (* raised if an alias pair is found, a control
			  flow exception *)


module U = Uref 
module S = Setp
module H = Hashtbl
module Q = Queue


(** Subtyping kinds *)
type polarity = Pos
	     | Neg 
	     | Sub
		  
(** Path kinds, for CFL reachability *)		
type pkind = Positive
	     | Negative
	     | Match
	     | Seed

(** Context kinds -- open or closed *)
type context = Open
	     | Closed

(* A configuration is a context (open or closed) coupled with a pair
   of stamps representing a state in the cartesian product DFA.
*)
type configuration = context * int * int

module ConfigHash = 
struct
  type t = configuration
  let equal t t' = t = t'
  let hash t = Hashtbl.hash t
end

module CH = H.Make(ConfigHash)

type config_map = unit CH.t

(** Generic bounds *)
type 'a bound = {index : int; info : 'a U.uref} 

(** For label paths. *)
type 'a path = {
  kind : pkind;
  reached_global : bool;
  head : 'a U.uref; 
  tail : 'a U.uref
}

module Bound =
struct 
  type 'a t = 'a bound
  let compare (x : 'a t) (y : 'a t) = 
    if (U.equal(x.info,y.info))
    then (x.index - y.index)
    else Pervasives.compare (U.deref x.info) (U.deref y.info)
end

module Path =
struct
  type 'a t = 'a path
  let compare (x : 'a t) (y : 'a t) =
    if (U.equal (x.head,y.head))
    then 
      (if (U.equal (x.tail,y.tail))
       then 
	 (if x.reached_global = y.reached_global
	  then
	    Pervasives.compare x.kind y.kind
	  else 
	    Pervasives.compare x.reached_global y.reached_global
	 )
       else
	 Pervasives.compare (U.deref x.tail) (U.deref y.tail)
      )
    else
      Pervasives.compare (U.deref x.head) (U.deref y.head)
end

module B = S.Make(Bound)

module P = S.Make(Path)

type 'a boundset = 'a B.t

type 'a pathset = 'a P.t

(** Constants, which identify elements in points-to sets *)
type constant = int * string

module Constant =
struct
  type t = constant

  let compare ((xid,_) : t) ((yid,_) : t) =
    Pervasives.compare xid yid
end
module C = Set.Make(Constant)

(** Sets of constants. Set union is used when two labels containing 
  constant sets are unified *)
type constantset = C.t

type lblinfo = {
  mutable l_name: string;
  (** either empty or a singleton, the initial location for this label *)
  loc : constantset;
  (** Name of this label *)
  l_stamp : int;
  (** Unique integer for this label *)
  mutable l_global : bool;
  (** True if this location is globally accessible *)
  mutable aliases: constantset;
  (** Set of constants (tags) for checking aliases *)
  mutable p_lbounds: lblinfo boundset; 
  (** Set of umatched (p) lower bounds *)
  mutable n_lbounds: lblinfo boundset; 
  (** Set of unmatched (n) lower bounds *)
  mutable p_ubounds: lblinfo boundset; 
  (** Set of umatched (p) upper bounds *)
  mutable n_ubounds: lblinfo boundset; 
  (** Set of unmatched (n) upper bounds *)
  mutable m_lbounds: lblinfo boundset;
  (** Set of matched (m) lower bounds *)
  mutable m_ubounds: lblinfo boundset;
  (** Set of matched (m) upper bounds *)

  mutable m_upath: lblinfo pathset;
  mutable m_lpath: lblinfo pathset;
  mutable n_upath: lblinfo pathset;
  mutable n_lpath: lblinfo pathset;
  mutable p_upath: lblinfo pathset;
  mutable p_lpath: lblinfo pathset;

  mutable l_seeded : bool;
  mutable l_ret : bool;
  mutable l_param : bool;
}

(** Constructor labels *)
and label = lblinfo U.uref 

(** The type of lvalues. *)
type lvalue = {
  l: label; 
  contents: tau
}

and vinfo = {
  v_stamp : int;
  v_name : string;
  
  mutable v_hole : (int,unit) H.t;
  mutable v_global : bool;
  mutable v_mlbs : tinfo boundset;
  mutable v_mubs : tinfo boundset;
  mutable v_plbs : tinfo boundset;
  mutable v_pubs : tinfo boundset;
  mutable v_nlbs : tinfo boundset;
  mutable v_nubs : tinfo boundset
}

and rinfo = {
  r_stamp : int;
  rl : label;
  points_to : tau;
  mutable r_global: bool;
}

and finfo = {
  f_stamp : int;
  fl : label;
  ret : tau;
  mutable args : tau list;
  mutable f_global : bool;
}

and pinfo = {
  p_stamp : int;
  ptr : tau;
  lam : tau;
  mutable p_global : bool;
}

and tinfo = Var of vinfo
	    | Ref of rinfo
	    | Fun of finfo
	    | Pair of pinfo

and tau = tinfo U.uref

type tconstraint = Unification of tau * tau
		   | Leq of tau * (int * polarity) * tau

 
(** Association lists, used for printing recursive types. The first element 
  is a type that has been visited. The second element is the string
  representation of that type (so far). If the string option is set, then 
  this type occurs within itself, and is associated with the recursive var
  name stored in the option. When walking a type, add it to an association 
  list. 

  Example : suppose we have the constraint 'a = ref('a). The type is unified
  via cyclic unification, and would loop infinitely if we attempted to print
  it. What we want to do is print the type u rv. ref(rv). This is accomplished
  in the following manner:
  
  -- ref('a) is visited. It is not in the association list, so it is added
  and the string "ref(" is stored in the second element. We recurse to print
  the first argument of the constructor.

  -- In the recursive call, we see that 'a (or ref('a)) is already in the
  association list, so the type is recursive. We check the string option,
  which is None, meaning that this is the first recurrence of the type. We
  create a new recursive variable, rv and set the string option to 'rv. Next,
  we prepend u rv. to the string representation we have seen before, "ref(",
  and return "rv" as the string representation of this type.

  -- The string so far is "u rv.ref(". The recursive call returns, and we
  complete the type by printing the result of the call, "rv", and ")"

  In a type where the recursive variable appears twice, e.g. 'a = pair('a,'a),
  the second time we hit 'a, the string option will be set, so we know to 
  reuse the same recursive variable name.
*)
type association = tau * string ref * string option ref
  
module PathHash = 
struct
  type t = int list
  let equal t t' = t = t'
  let hash t = Hashtbl.hash t
end

module PH = H.Make(PathHash)

(***********************************************************************)
(*                                                                     *)
(* Global Variables                                                    *)
(*                                                                     *)
(***********************************************************************)

(** Print the instantiations constraints. *)
let print_constraints : bool ref = ref false

(** If true, print all constraints (including induced) and show additional 
 debug output. *)
let debug = ref false			       

(** Just debug all the constraints (including induced) *)
let debug_constraints = ref false

(** Debug smart alias queries *)
let debug_aliases = ref false

let smart_aliases = ref false

(** If true, make the flow step a no-op *)
let no_flow = ref false

(** If true, disable subtyping (unification at all levels) *)
let no_sub = ref false

(** If true, treat indexed edges as regular subtyping *)
let analyze_mono = ref true

(** A counter for generating unique integers. *)
let counter : int ref = ref 0

let stamp : int ref = ref 0

(** A list of equality constraints. *)
let eq_worklist : tconstraint Q.t = Q.create()

(** A list of leq constraints. *)
let leq_worklist : tconstraint Q.t = Q.create()

let path_worklist : (lblinfo path) Q.t = Q.create()

let path_hash : (lblinfo path) PH.t = PH.create 32

(** A count of the constraints introduced from the AST. Used for debugging. *)
let toplev_count = ref 0

let appsite_index = ref 0

(** A hashtable containing stamp pairs of labels that must be aliased. *)
let cached_aliases : (int * int,unit) H.t = H.create 64

(***********************************************************************)
(*                                                                     *)
(* Utility Functions                                                   *)
(*                                                                     *)
(***********************************************************************)

let find = U.deref

let die s = 
  begin
    Printf.printf "*******\nAssertion failed: %s\n*******" s;
    print_newline(); 
    assert(false)
  end

(** First of a pair *)
let fst (x,y) = x

(** Second of a pair *)
let snd (x,y) = y

let fresh_appsite () : int =
  incr appsite_index;
  ! appsite_index

(** Generate a unique integer. *)
let fresh_index () : int = 
  incr counter; 
  !counter

let fresh_stamp () : int = 
  incr stamp;
  !stamp

(** Return a unique integer representation of a tau *)
let get_stamp (t : tau) : int = 
  match find t with
    | Var v -> v.v_stamp
    | Ref r -> r.r_stamp
    | Pair p -> p.p_stamp
    | Fun f -> f.f_stamp

(** Negate a polarity. *)
let negate (p : polarity) : polarity =
  match p with
    | Pos -> Neg
    | Neg -> Pos
    | Sub -> die "negate"  false

(** Consistency checks for inferred types *)
let  pair_or_var (t : tau) = 
  match (find t) with
    | Pair _ -> true
    | Var _ -> true
    | _ -> false

let ref_or_var (t : tau) =
   match (find t) with
     | Ref _ -> true
     | Var _ -> true
     | _ -> false

let fun_or_var (t : tau) =
  match (find t) with
    | Fun _ -> true
    | Var _ -> true
    |  _ -> false



(** Apply [f] structurally down [t]. Guaranteed to terminate, even if [t]
  is recursive *)
let iter_tau f t = 
  let visited : (int,tau) H.t = H.create 4 in
  let rec iter_tau' t =
    if (H.mem visited (get_stamp t)) then () else
      begin
	f (t);
	H.add visited (get_stamp t) t;
	match U.deref t with
	  |Pair p -> 
	      begin 
		iter_tau' p.ptr;
		iter_tau' p.lam
	      end
	  | Fun f ->
	      begin
		List.iter iter_tau' (f.args);
		iter_tau' f.ret;
	      end
	  | Ref r ->
	      begin
		iter_tau' r.points_to
	      end
	  | _ -> ()
      end
  in
    iter_tau' t

(* Extract a label's bounds according to [positive] and [upper]. *)    
let get_bounds (p :polarity ) (upper : bool) (l : label) : lblinfo boundset =
  let li = find l in
    match p with
      | Pos -> if (upper) then li.p_ubounds else li.p_lbounds 
      | Neg -> if (upper) then li.n_ubounds else li.n_lbounds
      | Sub -> if (upper) then li.m_ubounds else li.m_lbounds

let equal_tau (t : tau) (t' : tau) =
  (get_stamp t) = (get_stamp t')

let get_label_stamp (l : label) : int =
  (find l).l_stamp
     
(** Return true if [t] is global (treated monomorphically) *)
let get_global (t : tau) : bool =
  match find t with
    | Var v -> v.v_global
    | Ref r -> r.r_global
    | Pair p -> p.p_global
    | Fun f -> f.f_global 

let is_ret_label l = ( (find l).l_ret  || (find l).l_global) (* todo - check *)

let is_param_label l = ( (find l).l_param || (find l).l_global)

let is_global_label l = (find l).l_global

let is_seeded_label l = (find l).l_seeded

let set_global_label (l : label) (b : bool) : unit =
  begin
    assert ( (not (is_global_label(l))) || b);
    (U.deref l).l_global <- b
  end

(** Aliases for set_global *)
let global_tau = get_global


(** Get_global for lvalues *)
let global_lvalue lv = get_global lv.contents



(***********************************************************************)
(*                                                                     *)
(* Printing Functions                                                  *)
(*                                                                     *)
(***********************************************************************)

let string_of_configuration (c,i,i') =
  let context = match c with
    | Open -> "O"
    | Closed -> "C"
  in
    Printf.sprintf "(%s,%d,%d)" context i i'

let string_of_polarity p = 
  match p with
    | Pos  -> Printf.sprintf "+"
    | Neg  -> Printf.sprintf "-"
    | Sub -> "M"

(** Convert a label to a string, short representation *)
let string_of_label (l : label) : string =
  "\"" ^ (find l).l_name ^ "\""

(** Return true if the element [e] is present in the association list,
 according to uref equality *)
let rec assoc_list_mem (e : tau) (l : association list) =
  match l with
    | [] -> None
    | (h,s,so) :: t -> 
	if (U.equal (h,e)) then (Some (s,so)) else assoc_list_mem e t

(** Given a tau, create a unique recursive variable name. This should always
  return the same name for a given tau *)
let fresh_recvar_name (t : tau) : string =	    
  match find t with
    | Pair p -> "rvp" ^ string_of_int(p.p_stamp) 
    | Ref r -> "rvr" ^ string_of_int(r.r_stamp) 
    | Fun f -> "rvf" ^ string_of_int(f.f_stamp)
    | _ -> die "fresh_recvar_name" false

	
(** Return a string representation of a tau, using association lists. *)
let string_of_tau (t : tau ) : string = 
  let tau_map : association list ref = ref [] in
  let rec string_of_tau' t = 
    match (assoc_list_mem t (!tau_map)) with
      | Some (s,so) -> (* recursive type. see if a var name has been set *)
	  begin
	    match (!so) with
	      | None ->
		  begin
		    let rv = fresh_recvar_name(t) in
		      s := "u " ^ rv ^ "." ^ (!s);
		      so := Some (rv);
		      rv
		  end
	      | Some rv -> rv
	  end
      | None -> (* type's not recursive. Add it to the assoc list and cont. *)
	  let s = ref "" in
	  let so : string option ref = ref None in
	    begin
	      tau_map := (t,s,so) :: (!tau_map);

	      (match (find t) with
		 | Var v -> s := v.v_name;
		| Pair p -> 
		    begin
		      assert (ref_or_var(p.ptr));
		      assert (fun_or_var(p.lam));
		      s := "{";
		      s := (!s) ^ (string_of_tau' p.ptr);
		      s := (!s) ^ ",";
		      s := (!s) ^ (string_of_tau' p.lam);
		      s := (!s) ^"}"
		
		    end
		| Ref r ->
		    begin
		      assert(pair_or_var(r.points_to));
		      s := "ref(|";
		      s := (!s) ^ (string_of_label r.rl);
		      s := (!s) ^ "|,";
		      s := (!s) ^ (string_of_tau' r.points_to);
		      s := (!s) ^ ")"
		    
		    end
		| Fun f ->
		    begin
		      assert(pair_or_var(f.ret));
		      let rec string_of_args = function
			| h :: [] ->
			    begin
			      assert(pair_or_var(h));
			      s := (!s) ^ (string_of_tau' h)
			    end
			| h :: t -> 
			    begin
			      assert(pair_or_var(h));
			      s := (!s) ^ (string_of_tau' h) ^ ",";
			      string_of_args t
			    end
			| [] -> ()
		      in
			s := "fun(|";
			s := (!s) ^ (string_of_label f.fl);
			s := (!s) ^ "|,";
			s := (!s) ^ "<";
			if (List.length (f.args) > 0) 
			then
			  string_of_args (f.args)
			else
			  s := (!s) ^ "void";
			s := (!s) ^">,";
			s := (!s) ^ (string_of_tau' f.ret);
			s := (!s) ^ ")"
		    end);
	      tau_map := List.tl (!tau_map);
	      !s
	    end
  in
    string_of_tau' t

(** Convert an lvalue to a string *)	
let rec string_of_lvalue (lv : lvalue) : string =
  let contents = (string_of_tau(lv.contents)) in
  let l = (string_of_label lv.l) in
    assert(pair_or_var(lv.contents)); (* do a consistency check *)
    Printf.sprintf "[%s]^(%s)" contents l

let print_path (p : lblinfo path) : unit = 
  let string_of_context (k,i) = 
    match k with
      | Open -> Printf.sprintf "{O,%d}" i
      | Closed -> Printf.sprintf "{C,%d}" i
  in
  let string_of_pkind = function
    | Positive -> "p"
    | Negative -> "n"
    | Match -> "m"
    | Seed -> "s"
  in
    Printf.printf "%s --%s--> %s (%d) : " (string_of_label p.head)
      (string_of_pkind p.kind) (string_of_label p.tail) (PathHash.hash p)

(** Print a list of tau elements, comma separated *)
let rec print_tau_list (l : tau list) : unit = 
  let t_strings = List.map string_of_tau l in
  let rec print_t_strings = function
    | h :: [] -> print_string h; print_newline();
    | h :: t -> print_string h; print_string ", "; print_t_strings t
    | [] -> ()
  in
    print_t_strings t_strings

let print_constraint (c : tconstraint) =
  (
    match c with
      | Unification (t,t') ->
	  let lhs = string_of_tau t in
	  let rhs = string_of_tau t' in 
	    Printf.printf "%s == %s" lhs rhs
      | Leq (t,(i,p),t') ->
	  let lhs = string_of_tau t in
	  let rhs = string_of_tau t' in
	    Printf.printf "%s <={%d,%s} %s" lhs i (string_of_polarity p) rhs
  ); print_newline()

(***********************************************************************)
(*                                                                     *)
(* Type Operations -- these do not create any constraints              *)
(*                                                                     *)
(***********************************************************************)

(** Create an lvalue with label [lbl] and tau contents [t]. *)
let make_lval (lbl,t : label * tau) : lvalue = 
  {l = lbl; contents = t}
  
let make_label_int (is_global : bool) (name :string) (as_emp : bool) : label =
  let loc_const = (fresh_index(),name) in   
  U.uref {
    l_name = name;
    l_global = is_global;
    l_stamp = fresh_stamp();
    loc = 
	    if (not as_emp)
	    then (C.add loc_const C.empty)
	    else C.empty;
    aliases = 
	    if (not as_emp) 
	    then (C.add loc_const C.empty)
	    else C.empty;
    p_ubounds = B.empty;   
    p_lbounds = B.empty; 
    n_ubounds = B.empty;
    n_lbounds = B.empty;
    m_ubounds = B.empty;
    m_lbounds = B.empty;
    m_upath = P.empty;
    m_lpath = P.empty;
    n_upath = P.empty;
    n_lpath = P.empty;
    p_upath = P.empty;
    p_lpath = P.empty;
    l_seeded = false;
    l_ret = false;
    l_param = false;
  }

(** Create a new label with name [name]. Also adds a fresh constant
 with name [name] to this label's aliases set. *)  
let make_label (is_global : bool) (name : string) : label =
  make_label_int is_global name false
    
(** Create a new label with an unspecified name and an empty alias set. *) 
let fresh_label (is_global : bool) : label =
  let index = fresh_index() in
    make_label_int is_global ("l_" ^ (string_of_int index)) true
      
(** Create a fresh bound (edge in the constraint graph). *)
let make_bound (i,a : int * label) : lblinfo bound = 
  {index = i; info = a }

let make_tau_bound (i,a : int * tau) : tinfo bound = 
  {index = i; info = a }
  
(** Create a fresh named variable with name '[name]. *)
let make_var (b: bool) (name : string) : tau =
  U.uref (Var {v_name = ("'" ^name);
	       v_hole = H.create 8;
	       v_stamp = fresh_index();
	       v_global = b; 
	       v_mlbs = B.empty;
	       v_mubs = B.empty;
	       v_plbs = B.empty;
	       v_pubs = B.empty;
	       v_nlbs = B.empty;
	       v_nubs = B.empty}
	 )

(** Create a fresh unnamed variable (name will be 'fv). *)
let fresh_var (is_global : bool) : tau =
  make_var is_global ("fv" ^ (string_of_int (fresh_index())) )
  
(** Create a fresh unnamed variable (name will be 'fi). *)
let fresh_var_i (is_global : bool) : tau = 
  make_var is_global ("fi" ^ (string_of_int (fresh_index())) )
    
(** Create a Fun constructor. *)
let make_fun (lbl,a,r : label * (tau list) * tau) : tau =
  U.uref (Fun {fl = lbl; 
	       f_stamp = fresh_index();
	       f_global = false; 
	       args = a; 
	       ret = r }
	 )
    
(** Create a Ref constructor. *)
let make_ref (lbl,pt : label * tau) : tau =
  U.uref (Ref {rl = lbl; 
	       r_stamp = fresh_index();
	       r_global = false; 
	       points_to = pt}
	 )

(** Create a Pair constructor. *)
let make_pair (p,f : tau * tau) : tau =
  U.uref (Pair {ptr = p; 
		p_stamp = fresh_index();
		p_global = false;
		lam = f}
	 )

(** Copy the toplevel constructor of [t], putting fresh variables in each
  argement of the constructor. *)
let copy_toplevel (t : tau) : tau = 
  match find t with
    | Pair _ -> 
	make_pair (fresh_var_i false, fresh_var_i false)
    | Ref  _ -> 
	make_ref (fresh_label false,fresh_var_i false)
    | Fun  f -> 
	let fresh_fn = fun _ -> fresh_var_i false
	in
	  make_fun (fresh_label false, 
		    List.map fresh_fn (f.args) , fresh_var_i false)
    | _ -> die "copy_toplevel" false


let has_same_structure (t : tau) (t' : tau) =
  match find t, find t' with
    | Pair _, Pair _ -> true
    | Ref _, Ref _ -> true
    | Fun _, Fun _ -> true
    | Var _, Var _ -> true
    | _ -> false


let pad_args (f, f' : finfo * finfo) : unit =
  let padding = ref ((List.length f.args) - (List.length f'.args))
  in
    if (!padding == 0) then ()
    else
      let to_pad = if (!padding > 0) then f' else (padding := -(!padding);f)
      in
	for i = 1 to (!padding) do
	  to_pad.args <- to_pad.args @ [fresh_var false]
	done
	

let pad_args2 (fi, tlr : finfo * tau list ref) : unit = 
  let padding = ref ((List.length fi.args) - (List.length (!tlr)))
  in
    if (!padding == 0) then ()
    else 
      if (!padding > 0) then 
	for  i = 1 to (!padding) do
	  tlr := (!tlr) @ [fresh_var false]
	done
      else
	begin
	  padding := -(!padding);
	  for i = 1 to (!padding) do
	    fi.args <- fi.args @ [fresh_var false]
	  done
	end

(***********************************************************************)
(*                                                                     *)
(* Constraint Generation/ Resolution                                   *)
(*                                                                     *)
(***********************************************************************)


(** Make the type a global type *)
let set_global (t : tau) (b : bool) : unit =
  let set_global_down t = 
    match (find t) with
      | Var v -> v.v_global <- true 
      | Ref r -> set_global_label r.rl true
      | Fun f -> set_global_label f.fl true
      | _ -> ()
  in
    begin
      if (!debug && b) then 
	begin
	  Printf.printf "Set global: %s" (string_of_tau t);
	  print_newline()
	end;
      assert( (not (get_global(t)) ) || b );
      if (b) then (iter_tau set_global_down t);
      (match find t with
	 | Var v -> v.v_global <- b
	 | Ref r -> r.r_global <- b
	 | Pair p -> p.p_global <- b
	 | Fun f -> f.f_global <- b)
    end
    

let rec unify_int (t, t' : tau * tau) : unit =
  if (equal_tau t t') then ()
  else
    let ti,ti' = find t, find t' in
      begin
	U.unify combine (t,t');
	match ti,ti' with
	  | Var v, Var v' ->
	      begin
		set_global t' (v.v_global || get_global t');
		merge_vholes (v,v');
		merge_vlbs (v,v');
		merge_vubs (v,v')
	      end
	  | Var v, _ ->
	      begin
		set_global t' (v.v_global || get_global t');
		trigger_vhole v t';
		notify_vlbs t v;
		notify_vubs t v
	      end
	  | _, Var v ->
	      begin
		set_global t (v.v_global || get_global t);
		trigger_vhole v t;
		notify_vlbs t' v;
		notify_vubs t' v
	      end
	  | Ref r, Ref r' ->
	      begin
		set_global t (r.r_global || r'.r_global);
		unify_ref (r,r')
	      end
	  | Fun f, Fun f' ->
	      begin
		set_global t (f.f_global || f'.f_global);
		unify_fun (f,f')
	      end
	  | Pair p, Pair p' ->
	      begin
	      end
	  |  _ -> raise Inconsistent
      end

and notify_vlbs (t : tau) (vi : vinfo) : unit =
  let notify p bounds = 
    List.iter (fun b -> 
		 add_constraint (Unification (b.info,copy_toplevel t));
		 add_constraint (Leq (b.info,(b.index,p),t))
	      ) bounds
  in
    notify Sub (B.elements vi.v_mlbs);
    notify Pos (B.elements vi.v_plbs);
    notify Neg (B.elements vi.v_nlbs)

and notify_vubs (t : tau) (vi : vinfo) : unit =
  let notify p bounds = 
    List.iter (fun b -> 
		 add_constraint (Unification (b.info,copy_toplevel t));
		 add_constraint (Leq (t,(b.index,p),b.info))
	      ) bounds
  in
    notify Sub (B.elements vi.v_mubs);
    notify Pos (B.elements vi.v_pubs);
    notify Neg (B.elements vi.v_nubs)

and unify_ref (ri,ri' : rinfo * rinfo) : unit =
  add_constraint (Unification (ri.points_to, ri'.points_to))
  
and unify_fun (fi, fi' : finfo * finfo) : unit =
   let rec union_args  = function
    | _, [] -> false
    | [], _ -> true
    | h :: t, h' :: t' -> 
	add_constraint (Unification (h,h')); union_args(t,t') 
  in
    begin
      unify_label(fi.fl,fi'.fl);
      add_constraint (Unification (fi.ret,fi'.ret));
      if (union_args(fi.args,fi'.args)) 
      then fi.args <- fi'.args;
    end

and unify_label (l,l' : label * label) : unit = 
  let pick_name (li,li' : lblinfo * lblinfo) = 
    if ( (String.length li.l_name) > 1 && (String.sub (li.l_name) 0 2) = "l_") 
    then 
      li.l_name <- li'.l_name
    else ()
  in
  let combine_label (li,li' : lblinfo *lblinfo) : lblinfo =
    let rm_self b =
      not (li.l_stamp = (get_label_stamp b.info))
    in
      begin
	pick_name(li,li');
	li.l_global <- li.l_global || li'.l_global;
	li.aliases <- C.union (li.aliases) (li'.aliases);
	li.p_ubounds <- B.union li.p_ubounds li'.p_ubounds;
	li.p_lbounds <- B.union li.p_lbounds li'.p_lbounds;
	li.n_ubounds <- B.union li.n_ubounds li'.n_ubounds;
	li.n_lbounds <- B.union li.n_lbounds li'.n_lbounds;
	li.m_ubounds <- B.union li.m_ubounds (B.filter rm_self li'.m_ubounds);
	li.m_lbounds <- B.union li.m_lbounds (B.filter rm_self li'.m_lbounds);
	li.m_upath <- P.union li.m_upath li'.m_upath;
	li.m_lpath<- P.union li.m_lpath li'.m_lpath;
	li.n_upath <- P.union li.n_upath li'.n_upath;
	li.n_lpath <- P.union li.n_lpath li'.n_lpath;
	li.p_upath <- P.union li.p_upath li'.p_upath;
	li.p_lpath <- P.union li.p_lpath li'.p_lpath;
	li.l_seeded <- li.l_seeded || li'.l_seeded;
	li.l_ret <- li.l_ret || li'.l_ret;
	li.l_param <- li.l_param || li'.l_param;
	li
      end
  in
    (if (!debug_constraints) then
       Printf.printf "%s == %s\n" (string_of_label l) (string_of_label l'));
    U.unify combine_label (l,l')
  
and merge_vholes (vi,vi' : vinfo * vinfo) : unit =
  H.iter (fun i -> fun _ -> H.replace vi'.v_hole i ()) vi.v_hole

and merge_vlbs (vi,vi' : vinfo * vinfo) : unit =
  vi'.v_mlbs <- B.union vi.v_mlbs vi'.v_mlbs;
  vi'.v_plbs <- B.union vi.v_plbs vi'.v_plbs;
  vi'.v_nlbs <- B.union vi.v_nlbs vi'.v_nlbs    

and merge_vubs (vi,vi' : vinfo * vinfo) : unit =
  vi'.v_mubs <- B.union vi.v_mubs vi'.v_mubs;
  vi'.v_pubs <- B.union vi.v_pubs vi'.v_pubs;
  vi'.v_nubs <- B.union vi.v_nubs vi'.v_nubs  

and trigger_vhole (vi : vinfo) (t : tau) =
  let add_self_loops (t : tau) : unit = 
    match (find t) with
      | Var v -> 
	  H.iter (fun i -> fun _ -> H.replace v.v_hole i () ) vi.v_hole
      | Ref r -> 
	  H.iter (fun i -> fun _ ->
		    leq_label(r.rl,(i,Pos),r.rl);
		    leq_label(r.rl,(i,Neg),r.rl)
		 ) vi.v_hole
      | Fun f ->
	  H.iter (fun i -> fun _ ->
		    leq_label(f.fl,(i,Pos),f.fl);
		    leq_label(f.fl,(i,Neg),f.fl)
		 ) vi.v_hole
      | _ -> ()
  in
    iter_tau add_self_loops t
    

(** Pick the representative info for two tinfo's. This function prefers the
  first argument when both arguments are the same structure, but when
  one type is a structure and the other is a var, it picks the structure. 
  All other actions (e.g., updating the info) is done in unify_int *)
and combine (ti,ti' : tinfo * tinfo) : tinfo = 
  match ti,ti' with
    | Var _, _ -> ti'
    | _,_ -> ti 
	
and leq_int (t,(i,p),t') : unit = 
  if (equal_tau t t') then ()
  else
    let ti,ti' = find t, find t' in
      begin
	match ti,ti' with
	  | Var v, Var v' ->
	      begin
		match p with
		  | Pos -> 
		      v.v_pubs <- B.add (make_tau_bound(i,t')) v.v_pubs;
		      v'.v_plbs <- B.add (make_tau_bound(i,t)) v'.v_plbs
		  | Neg ->
		      v.v_nubs <- B.add (make_tau_bound(i,t')) v.v_nubs;
		      v'.v_nlbs <- B.add (make_tau_bound(i,t)) v'.v_nlbs
		  | Sub ->
		      v.v_mubs <- B.add (make_tau_bound(i,t')) v.v_mubs;
		      v'.v_mlbs <- B.add (make_tau_bound(i,t)) v'.v_mlbs
	      end
	  | Var v, _ ->
	      begin
		add_constraint (Unification (t, copy_toplevel t'));
		add_constraint (Leq (t,(i,p),t'))
	      end
	  | _, Var v ->
	      begin
		add_constraint (Unification (t', copy_toplevel t));
		 add_constraint (Leq (t,(i,p),t'))
	      end
	  | Ref r, Ref r' ->
	      begin
		leq_ref(r,(i,p),r')
	      end
	  | Fun f, Fun f' ->
	      add_constraint (Unification (t,t'))
	  | Pair pr, Pair pr' ->
	      begin
		add_constraint (Leq (pr.ptr,(i,p),pr'.ptr)); 
		add_constraint (Leq (pr.lam,(i,p),pr'.lam))
	      end
	  | _ -> raise Inconsistent
      end

and leq_ref (ri,(i,p),ri') : unit = 
  begin
    let add_self_loops (t : tau) : unit = 
      match (find t) with
	| Var v -> 
	    H.replace v.v_hole i ()
	| Ref r -> 
	    leq_label(r.rl,(i,Pos),r.rl);
	    leq_label(r.rl,(i,Neg),r.rl)
	| Fun f ->
	    leq_label(f.fl,(i,Pos),f.fl);
	    leq_label(f.fl,(i,Neg),f.fl)
	| _ -> ()
    in
      iter_tau (add_self_loops) ri.points_to;
      add_constraint (Unification (ri.points_to,ri'.points_to));
      leq_label(ri.rl,(i,p),ri'.rl) 
  end

and leq_label (l,(i,p),l') : unit = 
  if (!debug_constraints) then
    Printf.printf "%s <={%d,%s} %s\n"
      (string_of_label l) i (string_of_polarity p) (string_of_label l');
  let li,li' = find l,find l' in
    match p with
      | Pos ->
	  li.l_ret <- true;
	  li.p_ubounds <- B.add (make_bound(i,l')) li.p_ubounds;
	  li'.p_lbounds <- B.add (make_bound(i,l)) li'.p_lbounds
      | Neg ->
	  li'.l_param <- true;
	  li.n_ubounds <- B.add (make_bound(i,l')) li.n_ubounds;
	  li'.n_lbounds <- B.add (make_bound(i,l)) li'.n_lbounds
      | Sub ->
	  if ( U.equal (l,l')) then () 
	       else
		 begin
		   li.m_ubounds <- B.add (make_bound(0,l')) li.m_ubounds;
		   li'.m_lbounds <- B.add (make_bound(0,l)) li'.m_lbounds
		 end

and add_constraint_int (c : tconstraint) (toplev : bool) =
  if (!debug_constraints && toplev)
  then  
    begin
      Printf.printf "%d:>" (!toplev_count); 
      print_constraint c;
      incr toplev_count
    end
  else
    if (!debug_constraints)
    then 
      print_constraint c
    else (); 
  begin
    match c with
      | Unification _ ->
	  Q.add c eq_worklist
      | Leq _ ->
	  Q.add c leq_worklist
  end;
  solve_constraints()

and add_constraint (c : tconstraint) = 
  add_constraint_int c false

and add_toplev_constraint (c : tconstraint) = 
  if (!print_constraints && not (!debug_constraints)) 
  then 
    ((Printf.printf "%d:>" (!toplev_count));
     incr toplev_count; print_constraint c) else ();
  add_constraint_int c true

and fetch_constraint () : tconstraint option =
  try
    Some (Q.take eq_worklist)
  with
    | Q.Empty ->
	begin
	  try 
	    Some (Q.take leq_worklist)
	  with
	    | Q.Empty ->
		None
	end
	
(** The main solver loop. *)
and solve_constraints () : unit = 
  match fetch_constraint () with
    | Some c ->
	begin
	  (match c with
	     | Unification (t,t') -> unify_int (t,t')
	     | Leq (t,(i,p),t') -> 
		 if (!no_sub) 
		 then unify_int(t,t') 
		 else (
		   if (!analyze_mono) then
		     leq_int (t,(0,Sub),t')
		   else
		     leq_int(t,(i,p),t')
		 )
	  );
	  solve_constraints()
	end
    | None -> ()


(***********************************************************************)
(*                                                                     *)
(* Interface Functions                                                 *)
(*                                                                     *)
(***********************************************************************)

(** Return the contents of the lvalue. *)
let rvalue (lv : lvalue) : tau = 
  lv.contents

(** Dereference the rvalue. If it does not have enough structure to support
  the operation, then the correct structure is added via new unification
  constraints. *)
let rec deref (t : tau) : lvalue =
  match U.deref t with 
    | Pair p ->
	(
	  match U.deref (p.ptr) with
	    | Var _ ->
		begin 
		  let is_global = global_tau p.ptr in
		  let points_to = fresh_var is_global in 
		  let l = fresh_label is_global in
		  let r = make_ref(l,points_to)
		  in
		    add_toplev_constraint (Unification (p.ptr,r));
		    make_lval(l, points_to)
		end
	    | Ref r -> make_lval(r.rl, r.points_to)
	    | _ -> raise WellFormed
	)
    | Var v -> 
	begin
	  let is_global = global_tau t in
	    add_toplev_constraint 
	      (Unification (t,make_pair(fresh_var is_global,
					fresh_var is_global)) );
	    deref t
	end
    | _ -> raise WellFormed

(** Form the union of [t] and [t']. *)
let join (t : tau) (t' : tau) : tau = 
  let t''  = fresh_var false in
    add_toplev_constraint (Leq (t,(0,Sub),t''));
    add_toplev_constraint (Leq (t',(0,Sub),t''));
    t''

(** Form the union of a list [tl], expected to be the initializers of some
  structure or array type. *)
let join_inits (tl : tau list) : tau = 
  let t' = fresh_var false in
    begin
      List.iter (function t -> add_toplev_constraint (Leq(t,(0,Sub),t'))) tl;
      t'
    end

(** Take the address of an lvalue. Does not add constraints. *)
let address (lv  : lvalue) : tau = 
  make_pair (make_ref (lv.l, lv.contents), fresh_var false )

(** For this version of golf, instantiation is handled at [apply] *)
let instantiate (lv : lvalue) (i : int) : lvalue = 
  lv

(** Constraint generated from assigning [t] to [lv]. *)
let assign (lv : lvalue) (t : tau) : unit = 
  add_toplev_constraint (Leq (t,(0,Sub),lv.contents))

let assign_ret (i : int) (lv : lvalue) (t : tau) : unit =
  add_toplev_constraint (Leq (t,(i,Pos),lv.contents))

(** Project out the first (ref) component or a pair. If the argument [t] has
  no discovered structure, raise NoContents. *)
let proj_ref (t : tau) : tau = 
  match U.deref t with
    | Pair p -> p.ptr
    | Var v -> raise NoContents
    | _ -> raise WellFormed

(* Project out the second (fun) component of a pair. If the argument [t] has
 no discovered structure, create it on the fly by adding constraints. *)
let proj_fun (t : tau) : tau = 
  match U.deref t with
    | Pair p -> p.lam
    | Var v -> 
	let p,f = fresh_var false, fresh_var false in
	  add_toplev_constraint (Unification (t,make_pair(p,f)));
	  f
    | _ -> raise WellFormed

let get_args (t : tau) : tau list =
  match U.deref t with 
    | Fun f -> f.args
    | _ -> raise WellFormed

let get_finfo (t : tau) : finfo =
  match U.deref t with
    | Fun f -> f
    | _ -> raise WellFormed

(** Function type [t] is applied to the arguments [actuals]. Unifies the 
  actuals with the formals of [t]. If no functions have been discovered for 
  [t] yet, create a fresh one and unify it with t. The result is the return
  value of the function plus the index of this application site. *)
let apply (t : tau) (al : tau list) : (tau * int) = 
  let i = fresh_appsite () in
  let f = proj_fun(t) in
  let actuals = ref al in
  let fi,ret = 
    match U.deref f with
      | Fun fi -> fi,fi.ret
      | Var v -> 
	  let new_l,new_ret,new_args = 
	    fresh_label false, fresh_var false, 
	    List.map (function _ -> fresh_var false) (!actuals) 
	  in
	  let new_fun = make_fun(new_l,new_args,new_ret) in
	    add_toplev_constraint (Unification(new_fun,f));
	    (get_finfo new_fun,new_ret)
      | _ -> raise WellFormed
  in
    pad_args2(fi,actuals);
    List.iter2 (fun actual -> fun formal -> 
		  add_toplev_constraint (Leq (actual,(i,Neg),formal))
	       ) !actuals fi.args;
    (ret,i)
    
(** Create a new function type with name [name], list of formal arguments 
  [formals], and return value [ret]. Adds no constraints. *)
let make_function (name : string) (formals : lvalue list) (ret : tau) : tau = 
  let 
    f = make_fun (make_label false name, List.map (fun x -> rvalue x
						  ) formals,ret) 
  in
    make_pair(fresh_var false,f)

(** Create an lvalue. If [is_global] is true, the lvalue will be treated 
    monomorphically. *)
let make_lvalue (is_global : bool) (name : string) : lvalue = 
  if (!debug && is_global) 
  then 
    Printf.printf "Making global lvalue : %s\n" name
  else ();
  make_lval(make_label is_global name, make_var is_global name) 
 
(** Create a fresh non-global named variable. *)
let make_fresh (name : string) : tau =
  make_var false (name)

(** The default type for constants. *)
let bottom () : tau = 
  make_var false ("bottom")

(** Unify the result of a function with its return value. *)
let return (t : tau) (t' : tau) =
  add_toplev_constraint (Leq (t',(0,Sub),t))

(***********************************************************************)
(*                                                                     *)
(* Query/Extract Solutions                                             *)
(*                                                                     *)
(***********************************************************************)

let make_summary = leq_label

let path_signature k l l' b : int list = 
  let ksig = 
    match k with
      | Positive -> 1
      | Negative -> 2
      | _ -> 3
  in
    [ksig;get_label_stamp l; get_label_stamp l'; if b then 1 else 0]

let make_path (k, l, l', b) =
  let new_path_fn () = {kind = k; head = l; tail = l'; reached_global = b}
  in
  let psig = (path_signature k l l' b) in
  let li,li' = find l, find l' in
    if (PH.mem path_hash psig) then ()
    else
      begin
	let new_path = new_path_fn() in
	  PH.add path_hash psig new_path; 
	  Q.add (new_path) path_worklist;
	  (match k with
	     | Positive -> 
		 li.p_upath <- P.add new_path li.p_upath;
		 li'.p_lpath <- P.add new_path li'.p_lpath
	     | Negative ->
		 li.n_upath <- P.add new_path li.n_upath;
		 li'.n_lpath <- P.add new_path li'.n_lpath
	     | _ -> 
		 li.m_upath <- P.add new_path li.m_upath;
		 li'.m_lpath <- P.add new_path li'.m_lpath
	  );
	  if (!debug) then 
	    begin
	      print_string "Discovered path : "; 
	      print_path new_path;
	      print_newline()
	    end 
      end

let backwards_tabulate (l : label) : unit =
  let rec loop () = 
    try
      let p = Q.take path_worklist in
      let rule1() =
	if (!debug) then begin print_string "rule1"; print_newline() end;
	B.iter (fun lb -> make_path(Match,lb.info,p.tail,
				    p.reached_global || 
				    is_global_label(p.head))
	       ) (find p.head).m_lbounds 
      in
      let rule2() =
	if (!debug) then begin print_string "rule2"; print_newline() end;
	B.iter (fun lb -> make_path(Negative,lb.info,p.tail,
				    p.reached_global || 
				    is_global_label(p.head))
	       ) (find p.head).n_lbounds
      in
      let rule2m() = 
	if (!debug) then begin print_string "rule2m"; print_newline() end;
	B.iter (fun lb -> make_path(Match,lb.info,p.tail,
				    p.reached_global || 
				    is_global_label(p.head))
	       ) (find p.head).n_lbounds
      in
      let rule3() =
	if (!debug) then begin print_string "rule3"; print_newline() end;
	B.iter (fun lb -> make_path(Positive,lb.info,p.tail,
				    p.reached_global || 
				    is_global_label(p.head))
	       ) (find p.head).p_lbounds
      in
      let rule4() = 
	if (!debug) then begin print_string "rule4"; print_newline() end;
	B.iter (fun lb -> make_path(Negative,lb.info,p.tail,
				    p.reached_global || 
				    is_global_label(p.head))
	       ) (find p.head).m_lbounds
      in
      let rule5() = 
	if (!debug) then begin print_string "rule5"; print_newline() end;
	B.iter (fun lb -> make_path(Positive,lb.info,p.tail,
				    p.reached_global || 
				    is_global_label(p.head))
	       ) (find p.head).m_lbounds
      in
      let rule6() = 
	if (!debug) then begin print_string "rule6"; print_newline() end;
	B.iter (fun lb -> 
		  if (is_seeded_label lb.info) 
		  then () 
		  else
		    begin
		      (find lb.info).l_seeded <- true; (* set seeded *)
		      make_path(Seed,lb.info,lb.info,
				is_global_label(lb.info))
		    end
	       ) (find p.head).p_lbounds
      in
      let rule7() = 
	if (!debug) then begin print_string "rule7"; print_newline() end;
	if ( not ((is_ret_label p.tail) && (is_param_label p.head)) )
	then ()
	else
	  B.iter (fun lb -> 
		    B.iter (fun ub -> 
			      if (lb.index = ub.index)
			      then
				begin
				  if (!debug) then
				    begin
				      Printf.printf "New summary : %s %s"
					(string_of_label lb.info)
					(string_of_label ub.info);
				      print_newline()
				    end;
				  make_summary(lb.info,(0,Sub),ub.info);
				  (* rules 1,4, and 5 *)
				  P.iter (fun ubp -> (* rule 1 *)
					    make_path(Match,lb.info,
						      ubp.tail,
						     ubp.reached_global);
					 ) (find ub.info).m_upath;
				  P.iter (fun ubp -> (* rule 4 *)
					    make_path(Negative,lb.info,
						      ubp.tail,
						     ubp.reached_global)
					 ) (find ub.info).n_upath;
				  P.iter (fun ubp -> (* rule 5 *)
					    make_path(Positive,lb.info,
						      ubp.tail,
						     ubp.reached_global)
					 ) (find ub.info).p_upath
				end
			   ) (find p.tail).p_ubounds
		 ) (find p.head).n_lbounds

      in
      let matched_backward_rules () = 
	rule1();
	if (p.reached_global) then rule2m() else rule2();
	rule3();
	rule6();
	rule7()
      in
      let negative_backward_rules () =
	rule2();
	rule3();
	rule4();
	rule6();
	rule7()
      in
      let positive_backward_rules () = 
	rule3();
	rule5();
	rule6();
	rule7()
      in
	if (!debug) then
	  begin
	    print_string "Processing path : "; 
	    print_path p;
	    print_newline()
	  end;
	(match p.kind with
	   | Positive -> if (is_global_label p.tail)
	     then matched_backward_rules()
	     else positive_backward_rules()
	   | Negative -> negative_backward_rules()
	   | _ -> matched_backward_rules()
	); loop()
    with 
      | Q.Empty -> ()
  in
    if (!debug) then
      begin
	Printf.printf "Tabulating for %s..." (string_of_label l);
	if (is_global_label l) then Printf.printf ("(global)");
	print_newline()
      end;
    make_path(Seed,l,l,is_global_label l);
    loop()
      
(*    
let make_path (k,l,l') = 
  let 
    new_path_fn () = {kind = k;  head = l; tail = l'}
  in
  let psig = (path_signature k l l') in
  let li,li' = find l,find l' in
    if (PH.mem path_hash psig) then ()
    else
      begin
	let new_path = new_path_fn() in
	  PH.add path_hash psig new_path; 
	  Q.add (new_path) path_worklist;
	  (match k with
	     | Positive -> 
		 li.p_upath <- P.add new_path li.p_upath;
		 li'.p_lpath <- P.add new_path li'.p_lpath
	     | Negative ->
		 li.n_upath <- P.add new_path li.n_upath;
		 li'.n_lpath <- P.add new_path li'.n_lpath
	     | _ -> 
		 li.m_upath <- P.add new_path li.m_upath;
		 li'.m_lpath <- P.add new_path li'.m_lpath
	  );
	  if (!debug) then 
	    begin
	      print_string "Discovered path : "; 
	      print_path new_path;
	      print_newline()
	    end 
      end

let backwards_tabulate (l : label) : unit =
  let rec loop () = 
    try
      let p = Q.take path_worklist in
      let rule1 () = 
	if (!debug) then begin print_string "rule1"; print_newline() end;
	B.iter (fun lb -> make_path(Match,lb.info,p.tail)
	       ) (find p.head).m_lbounds
      in
      let rule2 () =
	if (!debug) then begin print_string "rule2"; print_newline() end;
	B.iter (fun lb -> make_path(Negative,lb.info,p.tail)
	       ) (find p.head).n_lbounds
	in
      let rule3 () = 
	if (!debug) then begin print_string "rule3"; print_newline() end;
	B.iter (fun lb -> make_path(Positive,lb.info,p.tail)
	       ) (find p.head).p_lbounds
      in
      let rule5 () = 
	if (!debug) then begin print_string "rule5"; print_newline() end;
	B.iter (fun lb -> make_path(Negative,lb.info,p.tail)
	       ) (find p.head).m_lbounds
      in
      let rule6 () =
	if (!debug) then begin print_string "rule6"; print_newline() end;
	B.iter (fun lb -> make_path(Positive,lb.info,p.tail)
	       ) (find p.head).m_lbounds
      in
      let rule7 () = 
	if (!debug) then begin print_string "rule7"; print_newline() end;
	if ( not ((is_ret_label p.tail) && (is_param_label p.head)) )
	then ()
	else
	  B.iter (fun lb -> 
		    B.iter (fun ub -> 
			      if (lb.index = ub.index)
			      then
				begin
				  if (!debug) then
				    begin
				      Printf.printf "New summary : %s %s"
					(string_of_label lb.info)
					(string_of_label ub.info);
				      print_newline()
				    end;
				  make_summary(lb.info,(0,Sub),ub.info);
				  (* rules 1,5, and 6 *)
				  P.iter (fun ubp -> (* rule 1,5,6 *)
					    make_path(Match,lb.info,ubp.tail);
					    make_path(Negative,lb.info,
						      ubp.tail);
					    make_path(Positive,lb.info,
						      ubp.tail);
					 ) (find ub.info).m_upath;
				  P.iter (fun ubp -> (* rule 5 *)
					    make_path(Negative,lb.info,
						      ubp.tail)
					 ) (find ub.info).n_upath;
				  P.iter (fun ubp -> (* rule 6 *)
					    make_path(Positive,lb.info,
						      ubp.tail)
					 ) (find ub.info).p_upath
				end
			   ) (find p.tail).p_ubounds
		 ) (find p.head).n_lbounds
      in
      let rule8 () = 
	if (!debug) then begin print_string "rule8"; print_newline() end;
	B.iter (fun lb -> 
		  if (is_seeded_label lb.info) 
		  then () 
		  else
		    begin
		      (find lb.info).l_seeded <- true; (* set seeded *)
		      make_path(Seed,lb.info,lb.info)
		    end
	       ) (find p.head).p_lbounds
      in
      let rule9 () = 
	if (!debug) then begin print_string "rule9"; print_newline() end;
	if ((is_global_label p.head) or (is_global_label p.tail) ) then
	  B.iter (fun lb -> make_path(Match,lb.info,p.tail)
		 ) (find p.head).n_lbounds
      in
      let rule10 () = 
	if (!debug) then begin print_string "rule10"; print_newline() end;
	if (is_global_label p.head) then
	  make_path(Match,p.head,p.tail)
      in
	if (!debug) then
	  begin
	    print_string "Processing path : "; 
	      print_path p;
	      print_newline()
	  end;
	(match p.kind with
	    | Positive -> rule3();rule6();rule8();rule10()
	    | Negative -> rule2();rule3();rule5();rule8();rule9()
	    | _ -> rule1();rule2();rule3();rule5();rule6();rule7();rule8();
		rule9()
	); loop()
    with 
      | Q.Empty -> ()
  in
    if (!debug) then
      begin
	Printf.printf "Tabulating for %s..." (string_of_label l);
	if (is_global_label l) then Printf.printf ("(global)");
	print_newline()
      end;
    make_path(Seed,l,l);
    loop()
*)


let collect_ptsets (l : label) : constantset = (* todo -- cache aliases *)
  let aliases : constantset ref = ref C.empty in 
  let li = find l in
  let collect_aliases = 
    P.iter (fun p -> aliases := C.union (!aliases) (find p.head).aliases)
  in
    collect_aliases li.m_lpath;
    collect_aliases li.n_lpath;
    collect_aliases li.p_lpath;
    !aliases

let extract_ptlabel (lv : lvalue) : label option =
  try
    (match (find (proj_ref lv.contents)) with
       | Var v -> None
       | Ref r ->  Some r.rl;
       | _ -> raise WellFormed)
  with 
    | NoContents -> None
	  
let points_to ( lv : lvalue) : string list = 
  try
    (match (find (proj_ref lv.contents)) with
      | Var v -> []
      | Ref r -> 
	  begin
	    backwards_tabulate r.rl;
	    List.map snd (C.elements (collect_ptsets r.rl))
	  end
      | _ -> raise WellFormed)
  with 
    | NoContents -> []
	  
let smart_alias_query (l : label) (l' : label) : bool = 
  (* Set of dead configurations *) 
  let dead_configs : config_map = CH.create 16 in
    (* the set of discovered configurations *)
  let discovered : config_map = CH.create 16 in
  let rec filter_match (i : int) =
    B.filter (fun (b : lblinfo bound) -> 
		  if ( i = b.index ) 
		  then true else false)
  in
  let rec simulate c l l' =
    let config = (c,get_label_stamp l, get_label_stamp l') in
      if (U.equal (l,l')) 
      then 
	begin
	  if (!debug) then
	    begin
	      Printf.printf "%s and %s are aliased" (string_of_label l) 
		(string_of_label l');
	      print_newline();
	    end;
	  raise APFound
	end
      else if (CH.mem discovered config)
      then ()
      else
	begin 
	  if (!debug_aliases) then
	    begin
	      Printf.printf "Exploring configuration %s" 
		(string_of_configuration config);
	      print_newline();
	    end;
	  CH.add discovered config ();
	  B.iter (fun lb -> simulate c lb.info l'
		 ) (get_bounds Sub false l); (* epsilon closure of l *)
	  B.iter (fun lb -> simulate c l lb.info
		 ) (get_bounds Sub false l'); (* epsilon closure of l' *)
	  B.iter (fun lb ->
		    let matching = 
		      filter_match lb.index (get_bounds Pos false l')
		    in
		      B.iter (fun b -> simulate Closed lb.info b.info
			     ) matching;
		      if (is_global_label l') (* positive self-loops on l' *)
		      then (simulate Closed lb.info l') 
		 ) (get_bounds Pos false l); (* positive transitions on l *)
	  if (is_global_label l) 
	  then
	    B.iter (fun lb -> (* positive self-loops on l *)
		      simulate Closed l lb.info
		   ) (get_bounds Pos false l');
	  (match c with  (* negative transitions on l, only if Open *)
	    | Open ->
		begin 
		  B.iter (fun lb -> 
			    let matching = 
			      filter_match lb.index (get_bounds Neg false l')
			    in
			      B.iter (fun b -> simulate Open lb.info b.info) 
				matching ;
			      if (is_global_label l')(* neg self-loops on l' *)
			      then (simulate Open lb.info l')
			 ) (get_bounds Neg false l);
		  if (is_global_label l) 
		  then
		    B.iter (fun lb -> (* negative self-loops on l *)
			      simulate Open l lb.info
			   ) (get_bounds Neg false l')
		end
	    | _ -> ());
	  CH.add dead_configs config (); (* if we got this far, then
					    the configuration was not
					    used *)
					    
	end
  in
    try
      begin
	if (H.mem cached_aliases (get_label_stamp l, get_label_stamp l')) 
	then 
	  true
	else
	  begin
	    simulate Open l l';
	    if (!debug) then
	      Printf.printf "%s and %s are NOT aliased\n" (string_of_label l) 
		(string_of_label l');
	    false
	  end
      end
    with
      | APFound -> 
	  begin
	    CH.iter ( fun config -> fun _ -> 
			if (not (CH.mem dead_configs config)) 
			then H.add cached_aliases (get_label_stamp l,
						   get_label_stamp l') ()
		    ) discovered;
	    true
	  end
	  
let alias_query (b : bool) (lvl : lvalue list) : int * int = 
  let naive_count = ref 0 in
  let smart_count = ref 0 in
  let lbls = List.map extract_ptlabel lvl in (* label option list *)
  let ptsets = List.map (fun lo ->
			   match lo with
			     | Some l -> collect_ptsets l
			     | None -> C.empty) lbls in
  let record_alias s lo s' lo' =
    match lo,lo' with
      | Some l,Some l' ->
	  if (C.is_empty (C.inter s s'))
	  then ()
	  else
	    begin
	      incr naive_count;
	      if (!smart_aliases && (smart_alias_query l l')) then
		incr smart_count
	    end
      | _ -> ()
  in
  let rec check_alias sets labels = 
    match sets,labels with
      | s :: st, l :: lt -> 
	  begin
	    List.iter2 (record_alias s l) ptsets lbls; 
	    check_alias st lt
	  end
      | [],[] -> ()
      | _ -> die "check_alias"
  in
    check_alias ptsets lbls;
    (!naive_count,!smart_count)

let alias_frequency (lvl : (lvalue * bool) list) : int * int =
  let extract_lbl (lv,b : lvalue * bool) = 
    (lv.l,b)
  in
  let naive_count = ref 0 in
  let smart_count = ref 0 in
  let lbls = List.map extract_lbl lvl in
  let ptsets = List.map (fun  (lbl,b) ->
			   if b then (find lbl).loc (* symbol access *)
			   else collect_ptsets lbl
			) lbls in
  let record_alias s (l,b) s' (l',b') = 
    if (C.is_empty (C.inter s s'))
    then ()
    else
      begin
	if (!debug_aliases) then 
	  begin
	    Printf.printf "%s and %s are aliased naively... " 
	      (string_of_label l) (string_of_label l');
	    print_newline()
	  end;
	incr naive_count;
	if ( (!smart_aliases) )
	then
	  begin
	    if ( (b || b') || (smart_alias_query l l'))
	    then
		incr smart_count
	    else
	      begin
		Printf.printf "%s and %s are not aliased by smart queries... " 
		  (string_of_label l) (string_of_label l');
		print_newline()
	      end
	  end
      end
  in
  let rec check_alias sets labels =
    match sets, labels with
      | s :: st, l :: lt ->
	  begin
	    List.iter2 (record_alias s l) ptsets lbls;
	    check_alias st lt
	  end
      | [],[] -> ()
      | _ -> die "check_alias"
  in check_alias ptsets lbls;
    (!naive_count,!smart_count)


(** an interface for extracting abstract locations from this analysis *)

type absloc = label

let abslocLvalue (l : lvalue) : absloc = l.l
let abslocEq = smart_alias_query
let absloc_print_name = ref true
let d_absloc () (p : absloc) = 
  let a = find p in 
    if !absloc_print_name then Pretty.dprintf "%s" a.l_name
    else Pretty.dprintf "%d" a.l_stamp

let phonyAddrOf (lv : lvalue) : lvalue =
  make_lval (fresh_label true, address lv)
