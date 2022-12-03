(* =============================================================================
   CodeHawk Unit Testing Framework 
   Author: Henny Sipma
   Adapted from: Kaputt (https://kaputt.x9c.fr/index.html)
   ------------------------------------------------------------------------------
   The MIT License (MIT)
 
   Copyright (c) 2005-2019 Kestrel Technology LLC
   Copyright (c) 2020-2021 Henny Sipma
   Copyright (c) 2022      Aarno Labs LLC

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

(* bchlib *)
open BCHLibTypes

(* bchlibarm32 *)
open BCHARMTypes

module A = TCHAssertion

   
let equal_jumptable_targets
      ?(msg="")
      ~(expected: (string * int list) list)
      ~(received: arm_jumptable_int) =
    A.make_equal_list
      (fun (tgt1, ixs1) (tgt2, ixs2) ->
             (tgt1 = tgt2)
             && ((List.length ixs1) = (List.length ixs2))
             && (List.for_all2 (fun i1 i2 -> i1 = i2) ixs1 ixs2))
      (fun (tgt, ixs) ->
        (tgt
         ^ ":["
         ^ (String.concat ", " (List.map string_of_int ixs))
         ^ "]"))
      ~msg
      expected
      (List.map (fun (dw, ixs) -> (dw#to_hex_string, ixs)) received#indexed_targets)