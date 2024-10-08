(* =============================================================================
   CodeHawk Binary Analyzer 
   Author: Henny Sipma
   ------------------------------------------------------------------------------
   The MIT License (MIT)
 
   Copyright (c) 2005-2020 Kestrel Technology LLC
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

(* chutil *)
open CHLogger
open CHTraceResult
open CHXmlDocument

(* bchlib *)
open BCHBasicTypes
open BCHByteUtilities
open BCHDoubleword
open BCHLibTypes
open BCHStreamWrapper
open BCHSystemInfo

(* bchlibelf *)
open BCHELFSection
open BCHELFTypes

module H = Hashtbl
module TR = CHTraceResult


let fail_traceresult (msg: string) (r: 'a traceresult): 'a =
  if Result.is_ok r then
    TR.tget_ok r
  else
    fail_tvalue
      (trerror_record (LBLOCK [STR "BCHELFProgramSection: "; STR msg])) r


class elf_program_section_t
        ?(is_got=false)
        (s:string)
        (vaddr:doubleword_int):elf_program_section_int =
object (self)

  inherit elf_raw_section_t s vaddr as super

  val got_table = H.create 3

  method is_got = is_got

  method read_got =
    if self#is_got then
      try
        let ch =
          make_pushback_stream ~little_endian:system_info#is_little_endian s in
        let n = (String.length s) / 4 in
        let c = ref 0 in
        let addr = ref vaddr in
        begin
          while !c < n do
            let entry = ch#read_doubleword in
            begin
              H.add got_table !addr#to_hex_string entry;
              c := !c + 1;
              addr := !addr#add_int 4
            end
          done;
          chlog#add
            "global offset table"
            (LBLOCK [STR "Number of entries: "; INT !c])
        end
      with
      | IO.No_more_input ->
         ch_error_log#add
           "no more input"
           (LBLOCK [STR "Unable to read the global offset table"])

  method get_got_value (a: doubleword_int) =
    if self#is_got then
      if H.mem got_table a#to_hex_string then
        H.find got_table a#to_hex_string
      else
        raise
          (BCH_failure
             (LBLOCK [
                  STR "Global offset table: no entry found for "; a#toPretty]))
    else
      raise
        (BCH_failure
           (LBLOCK [
                STR "Program section is not a global offset table"]))

  method get_got_values =
    if self#is_got then
      H.fold (fun k v a -> (k, v)::a) got_table []
    else
      []

  method get_value (a:doubleword_int) =
    try
      if super#includes_VA a then
        let offset =
          fail_traceresult
            "elf_program_section#get_value offset"
            (a#subtract_to_int vaddr) in
        let ch =
          make_pushback_stream ~little_endian:system_info#is_little_endian s in
        begin
          ch#skip_bytes offset;
          ch#read_doubleword
        end
      else
        raise
          (BCH_failure
             (LBLOCK [
                  STR "Address ";
                  a#toPretty ;
                  STR " is not included in section"]))
    with
    | BCH_failure p ->
       let msg =
         LBLOCK [STR "Error in getting value from program section: "; p] in
       raise (BCH_failure msg)
    | IO.No_more_input ->
       let msg =
         LBLOCK [
             STR "No more input in getting value from program section: ";
             a#toPretty;
             STR " (section va: ";
             vaddr#toPretty;
             STR ", section length: ";
             INT (String.length s);
             STR ")"] in
       raise (BCH_failure msg)

end


let mk_elf_program_section
      (s:string) (h:elf_section_header_int) (vaddr:doubleword_int) =
  let is_got = h#get_section_name = ".got" in
  let psection = new elf_program_section_t ~is_got s vaddr in
  let _ = if is_got then psection#read_got in
  psection


let read_xml_elf_program_section (node:xml_element_int) =
  let s = read_xml_raw_data (node#getTaggedChild "hex-data") in
  let vaddr = TR.tget_ok (string_to_doubleword (node#getAttribute "vaddr")) in
  let header = node#getTaggedChild "section-header" in
  let is_got = (header#getAttribute "name") = ".got" in
  let psection = new elf_program_section_t ~is_got s vaddr in
  let _ = if is_got then psection#read_got in
  psection
  
