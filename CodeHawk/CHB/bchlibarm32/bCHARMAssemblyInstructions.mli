(* =============================================================================
   CodeHawk Binary Analyzer
   Author: Henny Sipma
   ------------------------------------------------------------------------------
   The MIT License (MIT)

   Copyright (c) 2021-2024  Aarno Labs, LLC

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

(* chutil *)
open CHTraceResult

(* bchlib *)
open BCHLibTypes

(* bchlibarm32 *)
open BCHARMTypes


(** Holds an array of all of the assembly instructions, made available
    through a class instance reference.

    This is the backing store of all assembly instructions.
 *)


(** Return a reference to the class wrapper of the array of assembly
    instructions.*)
val arm_assembly_instructions: arm_assembly_instructions_int ref


(** Create an array of the given size to hold the assembly instructions. *)
val initialize_arm_instructions:
  (string * doubleword_int * doubleword_int) list -> unit


(** Initialize the instruction array with instructions *)
val initialize_arm_assembly_instructions:
  (string * doubleword_int * doubleword_int) list
  -> data_block_int list
  -> unit


(** Replace instructions with data blocks *)
val set_data_references: doubleword_int list -> unit


(** [set_arm_assembly_instruction instr] enters [instr] at the address
    obtained from [instr#get_address] in arm_assembly_instructions store.
    If the obtained address is out-of-range an error message is logged
    and the instruction is further ignored. *)
val set_arm_assembly_instruction: arm_assembly_instruction_int -> unit


(** [get_arm_assembly_instruction a] returns the assembly instruction at virtual
    address [a]. If [a] is out-of-range or if there is no instruction at [a] None
    is returned.*)
val get_arm_assembly_instruction: doubleword_int -> arm_assembly_instruction_result


(** [get_next_valid_instruction_address a] returns the smallest address greater
    than [a] that is the address of a valid instruction, without intervening
    data blocks or jump tables. If [a] is out-of-range or no such address exists,
    None is returned. *)
val get_next_valid_instruction_address: doubleword_int -> doubleword_int traceresult


(** [has_next_valid_instruction a] returns true if there is an instruction with
    address higher than a with a valid instruction, without intervening data blocks
    or jump tables.*)
val has_next_valid_instruction: doubleword_int -> bool


(** records the aggregate in assembly_instructions and annotates the instructions
    that are part of the aggregate.
 *)
val set_aggregate: doubleword_int -> arm_instruction_aggregate_int -> unit


(** [has_aggregate iaddr] return true if an aggregate is registered at virtual
    address [iaddr].*)
val has_aggregate: doubleword_int -> bool


(** [get_aggregate iaddr] returns the aggregate registered at virtual address [iaddr].

    raise BCH_failure if no aggregate is registered at [iaddr].*)
val get_aggregate: doubleword_int -> arm_instruction_aggregate_int



val get_arm_jumptables: unit -> (doubleword_int * arm_jumptable_int) list


(** [get_associated_test_instr finfo iaddr] returns the location and instruction
    that provides the test that is associated with the condition code of the
    instruction at address [iaddr]

    If no test instruction is associated with the instruction at [iaddr] [None]
    is returned.
 *)
val get_associated_test_instr:
  function_info_int -> ctxt_iaddress_t -> (location_int * arm_assembly_instruction_int) option
