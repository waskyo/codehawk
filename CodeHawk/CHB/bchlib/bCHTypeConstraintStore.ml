(* =============================================================================
   CodeHawk Binary Analyzer
   Author: Henny Sipma
   ------------------------------------------------------------------------------
   The MIT License (MIT)

   Copyright (c) 2024  Aarno Labs LLC

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
   ============================================================================= *)

(* chlib *)
open CHPretty
open CHUtils

(* chutil *)
open CHLogger

(* bchlib *)
open BCHBasicTypes
open BCHBCFiles
open BCHBCTypePretty
open BCHTypeConstraintGraph
open BCHTypeConstraintUtil
open BCHBCTypes
open BCHBCTypeUtil
open BCHCPURegisters
open BCHLibTypes

module H = Hashtbl

let bd = BCHDictionary.bdictionary
let tcd = BCHTypeConstraintDictionary.type_constraint_dictionary


class type_constraint_store_t: type_constraint_store_int =
object (self)

  val store = H.create 5

  (* constraints that involve function type, indexed by faddr *)
  val functiontypes = H.create 5

  (* constraints that involve a particular reglhs:
     faddr -> reg-index -> iaddr -> constraint list *)
  val reglhss = H.create 5

  (* constraints that involve a particular stacklhs:
     faddr -> stack-offset -> iaddr -> constraint list *)
  val stacklhss = H.create 5

  (* constraints that involve a global address *)
  val data_address_types = H.create 5

  (* constraints that involve a global variable *)
  val gvartypes = H.create 5

  method add_constraint (c: type_constraint_t) =
    let index = tcd#index_type_constraint c in
    (* index the constraint *)
    if H.mem store index then
      ()
    else
      begin
        H.add store index c;

        (* index the base variables *)
        (match c with
         | TySub (t1, t2)
           | TyGround (t1, t2) ->
            begin
              self#add_term_constraint_xref t1 index;
              self#add_term_constraint_xref t2 index;
            end
         | TyZeroCheck t ->
            self#add_term_constraint_xref t index
         | _ -> ())
      end

  method private add_term_constraint_xref (t: type_term_t) (c: int) =
    match t with
    | TyConstant _ -> ()
    | TyVariable tv ->
       (match tv.tv_basevar with
        | FunctionType faddr -> self#add_function_type_xref faddr c
        | DataAddressType gaddr -> self#add_data_address_type_xref gaddr c
        | GlobalVariableType gaddr -> self#add_gvar_type_xref gaddr c
        | RegisterLhsType (reg, faddr, iaddr) ->
           self#add_reglhs_xref reg faddr iaddr c
        | LocalStackLhsType (offset, faddr, iaddr) ->
           self#add_stacklhs_xref offset faddr iaddr c)

  method private add_function_type_xref (faddr: string) (c: int) =
    let entry =
      if H.mem functiontypes faddr then H.find functiontypes faddr else [] in
    H.replace functiontypes faddr (c :: entry)

  method private add_data_address_type_xref (gaddr: string) (c: int) =
    let entry =
      if H.mem data_address_types gaddr then
        H.find data_address_types gaddr
      else
        [] in
    H.replace data_address_types gaddr (c :: entry)

  method private add_gvar_type_xref (gaddr: string) (c: int) =
    let entry = if H.mem gvartypes gaddr then H.find gvartypes gaddr else [] in
    H.replace gvartypes gaddr (c :: entry)

  method private add_reglhs_xref
                   (reg: register_t)
                   (faddr: string)
                   (iaddr: string)
                   (c: int) =
    let rindex = bd#index_register reg in
    let regentry =
      if H.mem reglhss faddr then
        H.find reglhss faddr
      else
        let newentry = H.create 5 in
        begin
          H.add reglhss faddr newentry;
          newentry
        end in
    let iaddrentry =
      if H.mem regentry rindex then
        H.find regentry rindex
      else
        let newentry = H.create 5 in
        begin
          H.add regentry rindex newentry;
          newentry
        end in
    let entry =
      if H.mem iaddrentry iaddr then H.find iaddrentry iaddr else [] in
    H.replace iaddrentry iaddr (c :: entry)

  method private add_stacklhs_xref
                   (offset: int)
                   (faddr: string)
                   (iaddr: string)
                   (c: int) =
    let offsetentry =
      if H.mem stacklhss faddr then
        H.find stacklhss faddr
      else
        let newentry = H.create 5 in
        begin
          H.add stacklhss faddr newentry;
          newentry
        end in
    let iaddrentry =
      if H.mem offsetentry offset then
        H.find offsetentry offset
      else
        let newentry = H.create 5 in
        begin
          H.add offsetentry offset newentry;
          newentry
        end in
    let entry =
      if H.mem iaddrentry iaddr then H.find iaddrentry iaddr else [] in
    H.replace iaddrentry iaddr (c :: entry)

  method add_var_constraint (tyvar: type_variable_t) =
    self#add_constraint (TyVar (TyVariable tyvar))

  method add_term_constraint (t: type_term_t) =
    match t with
    | TyVariable tv -> self#add_var_constraint tv
    | _ -> ()

  method add_zerocheck_constraint (tyvar: type_variable_t) =
    begin
      self#add_var_constraint tyvar;
      self#add_constraint (TyZeroCheck (TyVariable tyvar))
    end

  method add_subtype_constraint (t1: type_term_t) (t2: type_term_t) =
    begin
      self#add_term_constraint t1;
      self#add_term_constraint t2;
      self#add_constraint (TySub (t1, t2))
    end

  method add_ground_constraint (t1: type_term_t) (t2: type_term_t) =
    begin
      self#add_term_constraint t1;
      self#add_term_constraint t2;
      self#add_constraint (TyGround (t1, t2))
    end

  method get_function_type_constraints (faddr: string): type_constraint_t list =
    if H.mem functiontypes faddr then
      List.map tcd#get_type_constraint (H.find functiontypes faddr)
    else
      []

  method get_reglhs_constraints
           (reg: register_t)
           (faddr: string)
           (iaddr: string): type_constraint_t list =
    if H.mem reglhss faddr then
      let regentry = H.find reglhss faddr in
      let rindex = bd#index_register reg in
      if H.mem regentry rindex then
        let iaddrentry = H.find regentry rindex in
        if H.mem iaddrentry iaddr then
          List.map tcd#get_type_constraint (H.find iaddrentry iaddr)
        else
          []
      else
        []
    else
      []

  method evaluate_reglhs_type
           (reg: register_t) (faddr: string) (iaddr: string)
         :(type_variable_t list * type_constant_t list) list =
    let konstraints = self#get_reglhs_constraints reg faddr iaddr in
    let termset = new IntCollections.set_t in
    let constraintset = new IntCollections.set_t in
    let _ =
      List.iter (fun c ->
          begin
            termset#addList
              (List.map tcd#index_type_term (type_constraint_terms c));
            constraintset#add (tcd#index_type_constraint c)
          end) konstraints in
    let changed = ref true in
    while !changed do
      begin
        changed := false;
        H.iter (fun ixc c ->
            if constraintset#has ixc then
              ()
            else
              let cterms = type_constraint_terms c in
              let prefixterms =
                List.concat (List.map type_term_prefix_closure cterms) in
              let cterms = List.map tcd#index_type_term prefixterms in
              match cterms with
              | [] -> ()
              | [c] -> ()
              | _ ->
                 if List.for_all termset#has cterms then
                   ()
                 else if List.exists termset#has cterms then
                   begin
                     List.iter termset#add cterms;
                     constraintset#add ixc;
                     changed := true
                   end
                 else
                   ()) store
      end
    done;
    let tygraph = mk_type_constraint_graph () in
    begin
      tygraph#initialize (List.map tcd#get_type_term termset#toList);
      let newgraph =
        constraintset#fold (fun g ixc ->
            let c = tcd#get_type_constraint ixc in
            g#add_constraint c) tygraph in
      let newgraph = newgraph#saturate in
      let newgraph = newgraph#saturate in
      let partition = newgraph#partition in
      List.fold_left (fun acc s ->
          let terms = List.map tcd#get_type_term s#toList in
          let reglhsvars =
            List.fold_left (fun acc t ->
                match t with
                | TyVariable tv when has_reg_lhs_basevar reg faddr iaddr t ->
                   tv :: acc
                | _ -> acc) [] terms in
          let tyconsts =
            List.fold_left (fun acc t ->
                match t with
                | TyConstant c -> c :: acc
                | _ -> acc) [] terms in
          match (reglhsvars, tyconsts) with
          | ([], _) -> acc
          | (_, []) -> acc
          | _ -> (reglhsvars, tyconsts) :: acc) [] partition
    end

  method resolve_reglhs_type
           (reg: register_t) (faddr: string) (iaddr: string): btype_t option =
    let evaluation = self#evaluate_reglhs_type reg faddr iaddr in
    let result =
      List.fold_left (fun acc (vars, consts) ->
          match acc with
          | Some _ -> acc
          | _ ->
             match (vars, consts) with
             | (v :: _, [c]) ->
                (match v.tv_capabilities with
                 | [] -> Some (type_constant_to_btype c)
                 | [Deref | Load | Store] ->
                    Some (t_ptrto (type_constant_to_btype c))
                 | _ -> None)
             | _ -> None) None evaluation in
    let _ =
      match result with
      | Some _ -> ()
      | _ ->
         chlog#add
           ("reglhs resolution not successful:" ^ faddr)
           (LBLOCK [
                STR iaddr;
                STR " - ";
                STR (register_to_string reg);
                STR ": ";
                pretty_print_list
                  evaluation
                  (fun (vars, consts) ->
                    LBLOCK [
                        STR "vars: ";
                        pretty_print_list
                          vars
                          (fun v -> STR (type_variable_to_string v))
                          "[" "; " "]";
                        STR "; consts: ";
                        pretty_print_list
                          consts
                          (fun c -> STR (type_constant_to_string c))
                          "[" "; " "]";
                        NL]) "[[" " -- " "]]"]) in
    result

  method resolve_reglhs_types (faddr: string):
           (register_t * string * btype_t option) list =
    let result = ref [] in
    let _ =
      if H.mem reglhss faddr then
        let regs = H.find reglhss faddr in
        H.iter (fun ixreg iaddrs ->
            let reg = bd#get_register ixreg in
            H.iter (fun iaddr _ ->
                let optty = self#resolve_reglhs_type reg faddr iaddr in
                result := (reg, iaddr, optty) :: !result) iaddrs) regs
      else
        () in
    !result

  method toPretty =
    let constraints = ref [] in
    let _ =
      H.iter
        (fun _ v ->
          constraints :=
            (type_constraint_to_string v) :: !constraints) store in
    let constraints = List.sort Stdlib.compare !constraints in
    STR (String.concat "\n" constraints)

end


let mk_type_constraint_store () = new type_constraint_store_t
