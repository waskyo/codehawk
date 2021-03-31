(* =============================================================================
   CodeHawk Binary Analyzer 
   Author: Henny Sipma
   ------------------------------------------------------------------------------
   The MIT License (MIT)
 
   Copyright (c) 2021 Aarno Labs, LLC

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
open CHPrettyUtil
open CHXmlDocument

(* bchlib *)
open BCHBasicTypes
open BCHCPURegisters
open BCHFunctionData
open BCHLibTypes
open BCHSystemInfo

(* bchlibarm32 *)
open BCHARMOperand
open BCHARMTypes

(* Reference: Table A8-1, pg A8-286 *)
let get_cond_mnemonic_extension (c:arm_opcode_cc_t) =
  match c with
  | ACCEqual -> "EQ"
  | ACCNotEqual -> "NE"
  | ACCCarrySet -> "CS"
  | ACCCarryClear -> "CC"
  | ACCNegative -> "MI"
  | ACCNonNegative -> "PL"
  | ACCOverflow -> "VS"
  | ACCNoOverflow -> "VC"
  | ACCUnsignedHigher -> "HI"
  | ACCNotUnsignedHigher -> "LS"
  | ACCSignedGE -> "GE"
  | ACCSignedLT -> "LT"
  | ACCSignedGT -> "GT"
  | ACCSignedLE -> "LE"
  | ACCAlways -> ""
  | ACCUnconditional ->
     raise
       (BCH_failure
          (LBLOCK [ STR "Unexpected value for mnemonic extension" ]))

class type ['a] opcode_formatter_int =
  object
    method ops: string -> arm_operand_int list -> 'a
    method opscc: string -> arm_opcode_cc_t -> arm_operand_int list -> 'a
    method no_ops: string -> 'a
  end

type 'a opcode_record_t = {
    mnemonic: string;
    operands: arm_operand_int list;
    ccode: arm_opcode_cc_t option;
    ida_asm: 'a
  }

let get_record (opc:arm_opcode_t) =
  match opc with
  | Add (s,c,rd,rn,rm) -> {
      mnemonic = "ADD";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "ADD" c [rd;rn;rm])
    }
  | AddCarry (s,c,rd,rn,rm) -> {
      mnemonic = "ADC";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "ADC" c [rd;rn;rm])
    }
  | Adr (cc, rd, addr) -> {
      mnemonic = "ADR";
      operands = [ rd; addr ];
      ccode = Some cc;
      ida_asm = (fun f -> f#opscc "ADR" cc [ rd; addr ])
    }
  | ArithmeticShiftRight (s,c,rd,rn,rm) -> {
      mnemonic = "ASR";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "ASR" c [rd;rn;rm])
    }
  | BitwiseAnd (setflags, cc, rd,rn, imm) -> {
      mnemonic = "AND";
      operands = [ rd; rn; imm ];
      ccode = Some cc;
      ida_asm = (fun f -> f#opscc "AND" cc [ rd; rn; imm ])
    }
  | BitwiseBitClear (s,c,rd,rn,rm) -> {
      mnemonic = "BIC";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "BIC" c [rd;rn;rm])
    }
  | BitwiseExclusiveOr (s,c,rd,rn,rm) -> {
      mnemonic = "EOR";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "EOR" c [rd;rn;rm])
    }
  | BitwiseNot (s,c,rd,rm) -> {
      mnemonic = "MVN";
      operands = [rd;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "MVN" c [rd;rm])
    }
  | BitwiseOr (s,c,rd,rn,rm) -> {
      mnemonic = "ORR";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "ORR" c [rd;rn;rm])
    }
  | Branch (cc, addr) -> {
      mnemonic = "B";
      operands = [ addr ];
      ccode = Some cc;
      ida_asm = (fun f -> f#opscc "B" cc [ addr ])
    }
  | BranchExchange (c,addr) -> {
      mnemonic = "BX";
      operands = [ addr ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "BX" c [ addr ])
    }
  | BranchLink (cc, addr) -> {
      mnemonic = "BL";
      operands = [ addr ];
      ccode = Some cc;
      ida_asm = (fun f -> f#opscc "BL" cc [ addr ])
    }
  | BranchLinkExchange (c,addr) -> {
      mnemonic = "BLX";
      operands = [ addr ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "BLX" c [ addr ])
    }
  | ByteReverseWord (c,rd,rm) -> {
      mnemonic = "REV";
      operands = [rd;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "REV" c [rd;rm])
    }
  | Compare (c,rn,rm) -> {
      mnemonic = "CMP";
      operands = [rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "CMP" c [rn;rm])
    }
  | CompareNegative (cc,op1,op2) -> {
      mnemonic = "CMN";
      operands = [ op1; op2];
      ccode = Some cc;
      ida_asm = (fun f -> f#opscc "CMN" cc [ op1; op2 ])
    }
  | CountLeadingZeros (c,rd,rm) -> {
      mnemonic = "CLZ";
      operands = [rd;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "CLZ" c [rd;rm])
    }
  | LoadMultipleDecrementBefore (wb,c,rn,rl,mem) -> { 
      mnemonic = "LDMDB";
      operands = [ rn; rl; mem ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LDMDB" c [ rn; rl ])
    }
  | LoadMultipleIncrementAfter (wb,c,rn,rl,mem) -> { 
      mnemonic = "LDM";
      operands = [ rn; rl; mem ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LDM" c [ rn; rl ])
    }
  | LoadRegister (c,rt,rn,mem) -> {
      mnemonic = "LDR";
      operands = [ rt; rn; mem ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LDR" c [ rt; mem ])
    }
  | LoadRegisterByte (c,rt,rn,mem) -> {
      mnemonic = "LDRB";
      operands = [ rt; rn; mem ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LDRB" c [ rn; mem ])
    }
  | LoadRegisterDual (c,rt,rt2,rn,rm,mem) -> {
      mnemonic = "LDRD";
      operands = [rt;rt2;rn;rm;mem];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LDRD" c [rt;rt2;mem])
    }
  | LoadRegisterHalfword (c,rt,rn,rm,mem) -> {
      mnemonic = "LDRH";
      operands = [rt;rn;rm;mem];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LDRH" c [rt;mem])
    }
  | LoadRegisterSignedByte (c,rt,rn,rm,mem) -> {
      mnemonic = "LDRSB";
      operands = [ rt; rn; mem ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LDRSB" c [ rn; mem ])
    }
  | LoadRegisterSignedHalfword (c,rt,rn,rm,mem) -> {
      mnemonic = "LDRSH";
      operands = [ rt; rn; mem ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LDRSH" c [ rn; mem ])
    }
  | LogicalShiftLeft (s,c,rd,rn,rm) -> {
      mnemonic = "LSL";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LSL" c [rd;rn;rm])
    }
  | LogicalShiftRight (s,c,rd,rn,rm) -> {
      mnemonic = "LSR";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "LSR" c [rd;rn;rm])
    }
  | Move (s,c,rd,rm) -> {
      mnemonic = "MOV";
      operands = [rd;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "MOV" c [rd;rm])
    }
  | MoveTop (c,rd,imm) -> {
      mnemonic = "MOVT";
      operands = [ rd; imm ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "MOVT" c [ rd; imm ])
    }
  | MoveWide (c,rd,imm) -> {
      mnemonic = "MOVW";
      operands = [ rd; imm ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "MOVW" c [ rd; imm ])
    }
  | Multiply (s,c,rd,rn,rm) -> {
      mnemonic = "MUL";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "MUL" c [rd;rn;rm])
    }
  | MultiplyAccumulate (s,c,rd,rn,rm,ra) -> {
      mnemonic = "MLA";
      operands = [rd;rn;rm;ra];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "MLA" c [rd;rn;rm;ra])
    }
  | Pop (c,sp,rl) -> {
      mnemonic = "POP";
      operands = [ sp; rl ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "POP" c [ rl ])
    }
  | Push (c,sp,rl) -> {
      mnemonic = "PUSH";
      operands = [ sp; rl ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "PUSH" c [ rl ])
    }
  | ReverseSubtract (s,c,rd,rn,rm) -> {
      mnemonic = "RSB";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "RSB" c [rd;rn;rm])
    }
  | ReverseSubtractCarry (setflags, cc, rd, rn, rm) -> {
      mnemonic = "RSC";
      operands = [ rd; rn; rm ];
      ccode = Some cc;
      ida_asm = (fun f -> f#opscc "RSC" cc [ rd; rn; rm ])
    }
  | RotateRightExtend (s,c,rd,rm) -> {
      mnemonic = "RRX";
      operands = [rd;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "RRX" c [rd;rm])
    }
  | SignedExtendHalfword (c,rd,rm) -> {
      mnemonic = "SXTH";
      operands = [rd;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "SXTH" c [rd;rm])
    }
  | SignedMultiplyLong (s,c,rdlo,rdhi,rn,rm) -> {
      mnemonic = "SMULL";
      operands = [rdlo;rdhi;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "SMULL" c [rdlo;rdhi;rn;rm])
    }
  | SingleBitFieldExtract (c,rd,rn) -> {
      mnemonic = "SBFX";
      operands = [rd;rn];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "SBFX" c [rd;rn])
    }
  | StoreMultipleDecrementBefore (wb,c,rn,rl,mem) -> {
      mnemonic = "STMDB";
      operands = [ rn; rl ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "STMDB" c [ rn; rl ])
    }
  | StoreMultipleIncrementAfter (wb,c,rn,rl,mem) -> { 
      mnemonic = "STM";
      operands = [ rn; rl ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "STM" c [ rn; rl ])
    }
  | StoreMultipleIncrementBefore (wb,c,rn,rl,mem) -> { 
      mnemonic = "STMIB";
      operands = [ rn; rl ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "STMIB" c [ rn; rl ])
    }
  | StoreRegister (c,rt,rn,mem) -> {
      mnemonic = "STR";
      operands = [ rt; rn; mem ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "STR" c [ rt; mem ])
    }
  | StoreRegisterByte (c,rt,rn,mem) -> {
      mnemonic = "STRB";
      operands = [ rt; rn; mem ];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "STRB" c [ rt; mem ])
    }
  | StoreRegisterDual (c,rt,rt2,rn,rm,mem) -> {
      mnemonic = "STRD";
      operands = [rt;rt2;rn;rm;mem];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "STRD" c [rt;rt2;mem])
    }
  | StoreRegisterHalfword (c,rt,rn,rm,mem) -> {
      mnemonic = "STRH";
      operands = [rt;rn;rm;mem];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "STRH" c [rt;mem])
    }
  | Subtract (s,c,rd,rn,rm) -> {
      mnemonic = "SUB";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "SUB" c [rd;rn;rm])
    }
  | SubtractCarry (s,c,rd,rn,rm) -> {
      mnemonic = "SBC";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "SBC" c [rd;rn;rm])
    }
  | SupervisorCall (cc,imm) -> {
      mnemonic = "SVC";
      operands = [ imm ];
      ccode = Some cc;
      ida_asm = (fun f -> f#opscc "SVC" cc [ imm ])
    }
  | Test (c,rn,rm) -> {
      mnemonic = "TST";
      operands = [rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "TST" c [rn;rm])
    }
  | TestEquivalence (c,rn,rm) -> {
      mnemonic = "TEQ";
      operands = [rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "TEQ" c [rn;rm])
    }
  | UnsignedBitFieldExtract (c,rd,rn) -> {
      mnemonic = "UBFX";
      operands = [rd;rn];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "UBFX" c [rd;rn])
    }
  | UnsignedExtendAddHalfword (c,rd,rn,rm) -> {
      mnemonic = "UXTAH";
      operands = [rd;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "UXTAH" c [rd;rn;rm])
    }
  | UnsignedExtendByte (c,rd,rm) -> {
      mnemonic = "UXTB";
      operands = [rd;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "UXTB" c [rd;rm])
    }
  | UnsignedExtendHalfword (c,rd,rm) -> {
      mnemonic = "UXTH";
      operands = [rd;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "UXTH" c [rd;rm])
    }
  | UnsignedMultiplyLong (s,c,rdlo,rdhi,rn,rm) -> {
      mnemonic = "UMULL";
      operands = [rdlo;rdhi;rn;rm];
      ccode = Some c;
      ida_asm = (fun f -> f#opscc "UMULL" c [rdlo;rdhi;rn;rm])
    }
  | OpInvalid | NotCode _ -> {
      mnemonic = "invalid";
      operands = [];
      ccode = None;
      ida_asm = (fun f -> f#no_ops "invalid")
    }

class string_formatter_t (width:int): [string] opcode_formatter_int =
object (self)

  method ops (s:string) (operands:arm_operand_int list) =
    let s = fixed_length_string s width in
    let (_,result) =
      List.fold_left
        (fun (isfirst,a) op ->
          if isfirst then
            (false,s ^ " " ^ op#toString)
          else
            (false,a ^ ", " ^ op#toString)) (true,s) operands in
    result

  method opscc (s:string) (cc:arm_opcode_cc_t) (operands:arm_operand_int list) =
    let mnemonic = s ^ (get_cond_mnemonic_extension cc) in
    self#ops mnemonic operands

  method no_ops (s:string) = s
end
                           
let get_arm_operands (opc:arm_opcode_t) = (get_record opc).operands

let get_arm_opcode_name (opc:arm_opcode_t) = (get_record opc).mnemonic

let arm_opcode_to_string ?(width=8) (opc:arm_opcode_t) =
  let formatter = new string_formatter_t width in
  let default () = (get_record opc).ida_asm formatter in
  default ()

let get_operands_written (opc:arm_opcode_t) =
  List.filter (fun op -> op#is_write) (get_record opc).operands

let get_operands_read (opc:arm_opcode_t) =
  List.filter (fun op -> op#is_read) (get_record opc).operands