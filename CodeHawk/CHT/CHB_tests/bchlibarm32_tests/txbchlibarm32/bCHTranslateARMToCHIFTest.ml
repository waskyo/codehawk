(* =============================================================================
   CodeHawk Unit Testing Framework 
   Author: Henny Sipma
   Adapted from: Kaputt (https://kaputt.x9c.fr/index.html)
   ------------------------------------------------------------------------------
   The MIT License (MIT)
 
   Copyright (c) 2022-2023  Aarno Labs LLC

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


module TS = TCHTestSuite

module A = TCHAssertion
module G = TCHGenerator
module S = TCHSpecification

module BA = TCHBchlibAssertion
module BG = TCHBchlibGenerator
module BS = TCHBchlibSpecification
module BU = TCHBchlibUtils

module ARMA = TCHBchlibarm32Assertion
module ARMU = TCHBchlibarm32Utils


(* chlib *)
open CHPretty
open CHLanguage

(* chutil *)
module TR = CHTraceResult

(* bchlib *)
module D = BCHDoubleword
module L = BCHLocation
module SI = BCHSystemInfo
module SW = BCHStreamWrapper
module U = BCHByteUtilities

(* bchlibarm32 *)
module ARMIS = BCHARMAssemblyInstructions
module R = BCHARMOpcodeRecords
module DT = BCHDisassembleARMInstruction
module TF = BCHTranslateARMToCHIF

open BCHFunctionData
open BCHBasicTypes
open BCHARMAssemblyBlock
open BCHARMAssemblyFunction
open BCHARMAssemblyFunctions
open BCHARMAssemblyInstruction
open BCHLibTypes
open BCHARMTypes
open BCHARMCodePC
open BCHAnalyzeApp
open BCHARMCHIFSystem
open BCHFunctionInfo


let testname = "bCHTranslateARMToCHIFTest"
let lastupdated = "2022-12-23"


let make_dw (s: string) = TR.tget_ok (D.string_to_doubleword s)


let codemax = make_dw "0x400000"


let make_stream ?(len=0) (s: string) =
  let bytestring = U.write_hex_bytes_to_bytestring s in
  let s = (String.make len ' ') ^ bytestring in
  SW.make_pushback_stream ~little_endian:true s


let translate_store () =
  let tests = [
      ("PUSHLR", "0x1b960", "04e02de500",
       [("var.0004", "LR_in"); ("SP", "sv__(SP_in - 4)__sv")]);
      ("PUSHR0_3", "0x19470", "0f002de900",
       [("var.0016", "R0_in");
        ("var.0012", "R1_in");
        ("var.0008", "R2_in");
        ("var.0004", "R3_in");
        ("SP", "sv__(SP_in - 16)__sv")]);
      ("STM", "0x3f7b8", "10128de800",
       [("var.0000", "R4_in"); ("arg.0004", "R9_in"); ("arg.0008", "R12_in")]);
      ("STMIB", "0x3ba4c", "10408de900",
       [("arg.0004", "R4_in"); ("arg.0008", "LR_in")]);
      ("STR", "0x1b4bc", "08608de500", [("arg.0008", "R6_in")]);
      ("STRBwb", "0x10208", "015062e500",
       [("R2_in[-1]", "sv__( lsb R5_in)__sv"); ("R2", "sv__(R2_in - 1)__sv")]);
      ("STRwb", "0x10568", "08402de500",
       [("var.0008", "R4_in"); ("SP", "sv__(SP_in - 8)__sv")]);
      ("STRDwb1", "0x1b4bc", "f0416de100",
       [("var.0016", "R4_in");
        ("var.0012", "R5_in");
        ("SP", "sv__(SP_in - 16)__sv")]);
      ("STRDwb2", "0x10ab8", "fc406de100",
       [("var.0012", "R4_in");
        ("var.0008", "R5_in");
        ("SP", "sv__(SP_in - 12)__sv")]);
      ("STRH", "0x1b4bc", "b031cde100", [("arg.0016", "R3_in")])      
    ] in
  begin
    TS.new_testsuite (testname ^ "_test_instr") lastupdated;

    SI.system_info#set_elf_is_code_address D.wordzero codemax;
    ARMIS.initialize_arm_instructions 1000;
    List.iter (fun (title, sfaddr, bytes, expectedcmds) ->

        TS.add_simple_test
          ~title
          (fun () ->
            let faddr = make_dw sfaddr in
            let fn = ARMU.arm_function_setup faddr bytes in
            let _ = analyze_arm_function faddr fn 0 in
            let _ = TF.translate_arm_assembly_function fn in
            let proc = arm_chif_system#get_arm_procedure faddr in
            (* remove the PC assignment at the end of the transaction *)
            let assigns = BU.extract_chif_cfg_assignments ~len:(-1) proc in
            let finfo = get_function_info faddr in
            BA.equal_assignments finfo ~expected:expectedcmds ~received:assigns)
      ) tests;

    TS.launch_tests ()
  end


let () =
  begin
    TS.new_testfile testname lastupdated;
    translate_store ();
    TS.exit_file ()
  end
        