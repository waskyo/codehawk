(* =============================================================================
   CodeHawk Binary Analyzer
   Author: Henny Sipma
   ------------------------------------------------------------------------------
   The MIT License (MIT)

   Copyright (c) 2005-2019 Kestrel Technology LLC
   Copyright (c) 2020      Henny Sipma
   Copyright (c) 2021-2024 Aarno Labs LLC

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
open CHNumerical
open CHLanguage

(* chutil *)
open CHLogger
open CHPrettyUtil
open CHTraceResult

(* bchlib *)
open BCHBasicTypes
open BCHDoubleword
open BCHLibTypes

module H = Hashtbl


let memory_base_to_string (b: memory_base_t): string =
  match b with
  | BLocalStackFrame -> "stack"
  | BRealignedStackFrame -> "realigned"
  | BAllocatedStackFrame -> "allocated-stack"
  | BGlobal -> "global"
  | BaseVar v -> "var-" ^ v#getName#getBaseName
  | BaseUnknown s -> "unknown-" ^ s


let memory_base_to_pretty b = STR (memory_base_to_string b)


let rec memory_offset_to_string offset =
  match offset with
  | NoOffset -> ""
  | ConstantOffset (n,subOffset) ->
     "[" ^ n#toString ^ "]" ^ (memory_offset_to_string subOffset)
  | FieldOffset ((fname, _), subOffset) ->
     "." ^ fname ^ (memory_offset_to_string subOffset)
  | IndexOffset (v,size,subOffset) ->
    "[" ^ v#getName#getBaseName ^ ":" ^ (string_of_int size) ^ "]" ^
      (memory_offset_to_string subOffset)
  | UnknownOffset -> "?offset?"


let rec is_unknown_offset offset =
  match offset with
  | UnknownOffset -> true
  | ConstantOffset (_, suboffset) -> is_unknown_offset suboffset
  | IndexOffset (_, _, suboffset) -> is_unknown_offset suboffset
  | _ -> false


let rec is_constant_offset offset =
  match offset with
  | NoOffset -> true
  | ConstantOffset (_, suboffset) -> is_constant_offset suboffset
  | _ -> false


let rec is_field_offset (offset: memory_offset_t): bool =
  match offset with
  | FieldOffset (_, NoOffset) -> true
  | FieldOffset (_, suboffset) -> is_field_offset suboffset
  | ConstantOffset (_, suboffset) -> is_field_offset suboffset
  | IndexOffset (_, _, suboffset) -> is_field_offset suboffset
  | _ -> false


let rec is_index_offset (offset: memory_offset_t): bool =
  match offset with
  | IndexOffset (_, _, NoOffset) -> true
  | IndexOffset (_, _, suboffset) -> is_index_offset suboffset
  | ConstantOffset (_, suboffset) -> is_index_offset suboffset
  | FieldOffset (_, suboffset) -> is_index_offset suboffset
  | _ -> false


let get_index_offset_variables (offset: memory_offset_t): variable_t list =
  let rec aux (o: memory_offset_t) (vlst: variable_t list) =
    match o with
    | IndexOffset (v, _, suboffset) -> aux suboffset (v :: vlst)
    | ConstantOffset (_, suboffset)
      | FieldOffset (_, suboffset) -> aux suboffset vlst
    | _ -> vlst in
  aux offset []


let rec get_constant_offsets offset =
  match offset with
  | NoOffset -> [ numerical_zero ]
  | ConstantOffset (n, suboffset) -> n :: (get_constant_offsets suboffset)
  | _ ->
     raise
       (BCH_failure
          (LBLOCK [
               STR "offset ";
               STR (memory_offset_to_string offset);
               STR " is not constant"]))

let get_total_constant_offset offset =
  List.fold_left (fun acc n ->
      acc#add n) numerical_zero (get_constant_offsets offset)

let memory_offset_to_pretty offset = STR (memory_offset_to_string offset)

let constant_offset_to_suffix_string n =
  fixed_length_int_string n#toString 4

let constant_offset_to_neg_suffix_string n =
  fixed_length_int_string n#neg#toString 4

let stack_offset_to_name offset =
  match offset with
  | ConstantOffset (n,NoOffset) when n#gt numerical_zero ->
     "arg." ^ (constant_offset_to_suffix_string n)
  | ConstantOffset (n,NoOffset) when n#lt numerical_zero ->
     "var." ^ (constant_offset_to_neg_suffix_string n)
  | ConstantOffset (n,NoOffset) when n#equal numerical_zero ->
     "var.0000"
  | _ -> "var.[" ^ (memory_offset_to_string offset) ^ "]"


let global_offset_to_name (size: int) (offset: memory_offset_t) =
  let prefix =
    match size with
    | 1 -> "gvb_"
    | 2 -> "gvh_"
    | _ -> "gv_" in
  try
    match offset with
    | ConstantOffset (n, s) when n#gt numerical_zero ->
       log_tfold_default
         (mk_tracelog_spec "global_offset_to_name")
         (fun dw ->
           prefix ^ dw#to_hex_string ^ (memory_offset_to_string s))
         ("gv_illegal_address")
         (numerical_to_doubleword n)
    | _ -> prefix ^ (memory_offset_to_string offset)
  with
  | BCH_failure p ->
     raise
       (BCH_failure
          (LBLOCK [STR "global_offset_to_name: "; p]))


let realigned_stack_offset_to_name offset =
  match offset with
  | ConstantOffset (n, NoOffset) when n#gt numerical_zero ->
     begin
       ch_error_log#add "realigned-stack location above zero" n#toPretty ;
       "vrrp." ^ (constant_offset_to_suffix_string n)
     end
  | ConstantOffset (n,NoOffset) when n#leq numerical_zero ->
     "vrr." ^ (constant_offset_to_neg_suffix_string n)
  | _ -> "vrr.[" ^ (memory_offset_to_string offset) ^ "]"


class memory_reference_t
    ~(index: int)
    ~(base: memory_base_t):memory_reference_int =
object (self:'a)

  method index = index

  method compare (other:'a) = Stdlib.compare self#index other#index

  method get_base = base

  method get_name =
    match base with
    | BaseVar v -> v#getName#getBaseName
    | BLocalStackFrame -> "var"
    | BRealignedStackFrame -> "varr"
    | BAllocatedStackFrame -> "vara"
    | BGlobal -> "gv"
    | BaseUnknown s -> "??" ^ s ^ "??"

  method get_external_base: variable_t traceresult =
    match base with
    | BaseVar v -> Ok v
    | _ ->
       Error [
           "get_external_base: not an external base: "
           ^ (memory_base_to_string base)]

  method has_external_base =
    match base with BaseVar _ -> true | _ -> false

  method is_stack_reference = match base with
    | BLocalStackFrame | BRealignedStackFrame | BAllocatedStackFrame -> true
    | _ -> false

  method is_realigned_stack_reference =
    match base with BRealignedStackFrame -> true | _ -> false

  method is_global_reference = match base with BGlobal -> true | _ -> false

  method is_unknown_reference =
    match base with BaseUnknown _ -> true | _ -> false

  method toPretty = LBLOCK [ memory_base_to_pretty base ]

end


class memory_reference_manager_t
        (vard:vardictionary_int):memory_reference_manager_int =
object (self)

  val table = H.create 3
  val vard = vard

  method reset = H.clear table

  method private mk_reference (base:memory_base_t) =
    let index = vard#index_memory_base base in
    if H.mem table index then
      H.find table index
    else
      let memref = new memory_reference_t ~index ~base in
      begin
        H.add table index memref;
        memref
      end

  method mk_local_stack_reference = self#mk_reference BLocalStackFrame

  method mk_realigned_stack_reference = self#mk_reference BRealignedStackFrame

  method mk_allocated_stack_reference = self#mk_reference BAllocatedStackFrame

  method mk_global_reference = self#mk_reference BGlobal

  method mk_basevar_reference v = self#mk_reference (BaseVar v)

  method mk_unknown_reference s = self#mk_reference (BaseUnknown s)

  method get_memory_reference (index: int): memory_reference_int traceresult =
    if H.mem table index then
      Ok (H.find table index)
    else
      Error [
          "get_memory_reference_int: index not found: " ^ (string_of_int index)]

  method is_unknown_reference (index: int): bool traceresult =
    let memref_r = self#get_memory_reference index in
    tmap
      ~msg:"is_unknown_reference"
      (fun memref -> memref#is_unknown_reference) memref_r

  method initialize =
    List.iter (fun (index, base) ->
        H.add table index (new memory_reference_t ~index ~base))
      vard#get_indexed_bases

end

let make_memory_reference_manager = new memory_reference_manager_t
