(* =============================================================================
   CodeHawk Binary Analyzer 
   Author: Henny Sipma
   ------------------------------------------------------------------------------
   The MIT License (MIT)
 
   Copyright (c) 2005-2019 Kestrel Technology LLC
   Copyright (c) 2021-2023 Aarno Labs LLC

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
open CHNumerical

(* bchlib *)
open BCHLibTypes


(** A constant value with a given size in bytes and signedness.*)


(** [make_immediate signed size v] returns an immediate value of [size]
    bytes and value [v]. If the value [v] is outside the range that can be
    represented by the bytes with the given signedness, [Error] is returned.*)
val make_immediate: bool -> int -> numerical_t -> immediate_result


(** [signed_immedidate_from_int size v] returns an immediate value of [size]
    bytes and value [v]. If the value [v] is outside the range that can be
    represented by a signed integer with width [size] bytes, [Error] is
    returned.*)
val signed_immediate_from_int: ?size:int -> int -> immediate_result


(** [imm0] is a signed immediate with size 4 bytes and value 0.*)
val imm0: immediate_int


(** [imm1] is a signed immediate with size 4 bytes and value 1.*)
val imm1: immediate_int
