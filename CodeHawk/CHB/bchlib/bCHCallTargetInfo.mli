(* =============================================================================
   CodeHawk Binary Analyzer
   Author: Henny Sipma
   ------------------------------------------------------------------------------
   The MIT License (MIT)

   Copyright (c) 2020      Henny B. Sipma
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

(* chutil *)
open CHXmlDocument

(* bchlib *)
open BCHLibTypes


(** Implementation of [call_target_info_int] class type.*)


(** [mk_call_target_info fintf fsem ctgt] returns a call-target-info for
    function interface [fintf] and function semantics [fsem].*)
val mk_call_target_info:
  function_interface_t
  -> function_semantics_t
  -> call_target_t -> call_target_info_int


(** [read_xml_call_target_info node] returns a call-target-info obtained
    from its standard xml representation.

    The standard xml representation consists of the following three
    attributes:
    - "fintf": holds the interface dictionary index to a function interface
    - "fsem": holds the interface dictionary index to a function semantics
    - "ictgt": holds the interface dictionary index to a call target
 *)
val read_xml_call_target_info: xml_element_int -> call_target_info_int
