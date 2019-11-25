(* =============================================================================
   CodeHawk C Analyzer 
   Author: Henny Sipma
   ------------------------------------------------------------------------------
   The MIT License (MIT)
 
   Copyright (c) 2005-2019 Kestrel Technology LLC

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
open CHAtlas
open CHPretty

(* chutil *)
open CHGc
open CHLogger
open CHXmlDocument
open CHXmlReader

(* cchlib *)
open CCHFunctionSummary
open CCHSettings
open CCHUtilities

(* cchpre *)
open CCHCreatePrimaryProofObligations
open CCHInvariantFact
open CCHPreFileIO

(* cchanalyze *)
open CCHCheckValidity
open CCHGenerateAndCheck
open CCHGenerateLocalInvariants
   

(* cchcmdline *)
open CCHVersion

let _ = CHPretty.set_trace_level 0

let cmds = [ "version" ; "gc" ; "primary" ; "secondary" ; "localinvs" ; 
	     "globalinvs" ; "check" ; "generate_and_check" ]

let cmdchoices = String.concat ", " cmds 

let cmd = ref "version"
let setcmd s = if List.mem s cmds then
    cmd := s
  else
    begin
      pr_debug [ STR "Command " ; STR s ; STR " not recognized. Valid choices are: " ;
		 pretty_print_list cmds (fun s -> STR s) "[" ", " "]" ; NL ] ;
      exit 1
    end

let domains = ref []
let add_domain d = domains := d :: !domains
let set_domains s = 
  String.iter (fun c -> match c with
  | 'l' -> add_domain linear_equalities_domain
  | 'v' -> add_domain valueset_domain
  | 'i' -> add_domain intervals_domain
  | 's' -> add_domain symbolic_sets_domain
  | 'p' -> add_domain sym_pointersets_domain
  | 'r' -> add_domain pepr_domain
  (* | 'x' -> add_domain "state sets" *)
  | _ -> 
    begin
      pr_debug [ STR "Some characters were not recognized in the domain specification: " ; 
		 STR s ; NL ] ;
      exit 1
    end) s

let speclist = [
  ("-version", Arg.Unit (fun () -> ()), "show version information and exit") ;
  ("-gc", Arg.Unit (fun () -> cmd := "gc"), 
   "show ocaml garbage collector settings and exit") ;
  ("-summaries", Arg.String function_summary_library#add_summary_jar,
   "location of jar with library function summaries") ;
  ("-command", Arg.String setcmd, "choose action to perform: " ^ cmdchoices) ;
  ("-domains", Arg.String set_domains,
   "domains to be used in invariant generation: " ^
     "[l:lineq; v:valuesets; i:intervals; s:symbolicsets]") ;
  ("-cfile", Arg.String system_settings#set_cfilename,
   "relative filename of c source code file") ;
  ("-verbose", Arg.Unit (fun () -> system_settings#set_verbose true),
   "print status on proof obligations and invariants");
  ("-appname", Arg.String system_settings#set_application_name,
   "name of the application to report in results files");
   ("-nofilter", Arg.Unit (fun () -> system_settings#set_filterabspathfiles false),
    "do not filter out functions in files with absolute path names");
   ("-unreachability", Arg.Unit (fun () -> system_settings#set_use_unreachability),
    "use unreachability as a justification for discharging proof obligations") ;
   ("-wordsize", Arg.Int system_settings#set_wordsize,
    "set word size (e.g., 16, 32, or 64)") ;
   ("-contractpath", Arg.String system_settings#set_contractpath,"path to contract files")
]  

let usage_msg = "chc_analyze <options> <path to analysis directory>"
let read_args () = Arg.parse speclist system_settings#set_path usage_msg

let save_log_files (contenttype:string) =
  begin
    save_logfile ch_info_log contenttype "infolog" ;
    append_to_logfile ch_error_log contenttype "errorlog" ;
    save_logfile chlog contenttype "chlog"
  end

let main () =
  try
    let _ = read_args () in
    let _ = chlog#set_max_entry_size 1000 in
    if !cmd = "version" then
      begin
	pr_debug [ version#toPretty ; NL ] ;
	exit 0
      end

    else if !cmd = "gc" then
      begin
	pr_debug [ garbage_collector_settings_to_pretty () ; NL ] ;
	exit 0
      end

    else if !cmd = "primary" then 
      begin
	primary_process_file () ;
	save_log_files "primary"
      end
	  
    else if !cmd = "localinvs" then 
      begin
	invariants_process_file (List.rev !domains) ;
	save_log_files "localinvs"
      end

    else if !cmd = "globalinvs" then ()

    else if !cmd = "check" then 
      begin
	check_process_file () ;
	save_log_files "check"
      end

    else if !cmd = "generate_and_check" then
      begin
        generate_and_check_process_file (List.rev !domains) ;
        save_log_files "gencheck"
      end

    else
      begin
	pr_debug [ STR "Command " ; STR !cmd ; STR " not recognized" ; NL ] ;
	exit 1
      end

  with
  | CHXmlReader.XmlParseError (line,col,p)
  | XmlDocumentError (line,col,p) ->
     begin
       pr_debug [ STR "Xml error: (" ; INT line ; STR ", " ; INT col ; STR "): " ;
		  p ; NL ] ;
       exit 3
    end

  | CHCommon.CHFailure p
  | CCHFailure p ->
     begin
       ch_error_log#add "final failure" p ;
       save_log_files "failure" ;
       pr_debug [ STR "Failure: " ; p ; NL ] ;
       exit 1
     end

  | Invalid_argument s ->
     begin
       ch_error_log#add "final failure" (LBLOCK [ STR "Invalid argument: " ; STR s ]) ;
       save_log_files "failure" ;
       pr_debug [ STR "Error: " ; STR s ; NL ] ;
       exit 1
    end


let _ = Printexc.print main ()
