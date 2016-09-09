open Kirc_Ast
open Kirc

let (^>) =fun a b -> a ^ "\n" ^ b

let fflib () = Dl.dlopen ~filename:"./fastflow.so" ~flags:[Dl.RTLD_NOW]

let id = ref 0

open Ctypes
type farm
let farm : farm structure typ = structure "ff_farm"


type parallel_for
let parallel_for : parallel_for structure typ = structure "ParallelFor"

class virtual fastflow = object
  method virtual source : string
  method virtual create_accelerator : int -> (farm structure) ptr
  method virtual run_accelerator : unit -> unit
  method virtual wait : unit -> unit
end


let to_src (kern: ('a, 'b, 'c, 'd)kirc_function) =
  let module Ff = struct
    let target_name = "FastFlow"

    let global_function = ""
    let device_function = ""
    let host_function = ""

    let global_parameter = ""


    let global_variable = ""
    let local_variable = ""
    let shared_variable = ""

    let kern_start  = "// this is generated from Ocaml kern function\n"^>
                      "TASK * f(TASK * task) {"
    let kern_end = "\n     return task;\n   }\n"

    let parse_intrinsics (cuda,opencl) = opencl
  end
  in
  let
    module Kirc_ff = Gen.Generator(Ff)
  in
  let rec args_  = function
    | Params p ->
      (*Kirc.print_ast p;*)
      "typedef struct __task{\n"^args_ p
    | Concat (p1,p2)->
      (Kirc_ff.parse 1 p1 ^";\n "^
       args_ p2)
    | Empty -> "} TASK;"
    | _ -> assert false
  in
  let __ = "     " in
  let rec intro = function
    | Params p -> intro p
    | Concat (p1,p2) ->
      (intro p1 ^> intro p2)
    | Empty -> "\n"
    | VecVar (t, i, s) -> __ ^
                          (match t with
                           | Int _ ->"int *"
                           | Float _ -> "float *"
                           | Double _ -> "double *"
                           | Custom (n,_,ss) -> (("struct "^n^"_sarek *"))
                           | _ -> assert false
                          )^ " "^s ^" = task->"^s^";"
    | IntVar (i,s) -> __ ^"int" ^ " "^s ^" = task->"^s^";"
    | _ -> ""

  in

  let rec aux = function
    | Kern (args,body) -> (args_ args,
                           intro args,
                           Kirc_ff.parse 3 body)
    | a -> "",  "", Kirc_ff.parse 3 a
  in
  let ff_lib = begin
    "// this is needed for dlsym" ^>
    "extern \"C\" {

      void * userfun"^(string_of_int !id)^"(void *);

    }" ^>

    "// this is a wrapper
    void * userfun"^(string_of_int !id)^"(void * t) {
      TASK * res = f((TASK *) t);
      return ((void *) res);
    }"
  end
  in

  let ff_header = "#include <math.h> \n#include<stdio.h>"
  in
  (*create_task_cfun kern.body;*)
  Printf.printf "#################\nBefore rewrite : \n";
  Kirc.print_ast kern.funbody;
  Printf.printf "#################\nAfter rewrite : \n";
  let (task, intro, body_source) = aux (Kirc.rewrite kern.funbody) in
  Kirc.print_ast (Kirc.rewrite kern.funbody);
  Pervasives.flush stdout;
  ff_header ^ "\n\n" ^task ^"\n\n" ^ Ff.kern_start ^>
  intro ^ __ ^ body_source ^ Ff.kern_end ^"\n\n" ^ ff_lib



let to_lib ?cc_opt:(opts=["-O3"; "-lm"]) ?includes:(inc=["math.h"; "stdio.h"]) src =
  let fname = "userfun"^(string_of_int !id) in
  let channel = open_out (fname^".cpp") in
  output_string channel src;
  close_out channel;
  (*print_string("g++ -g -O3  -lm --shared -fPIC "^fname^".cpp -o "^fname^".so\n");*)
  ignore(Sys.command ("g++ -g -O3  "^(List.fold_left (fun a b -> a^" "^b) "" opts)^" --shared -fPIC "^fname^".cpp -o "^fname^".so"));
  let open Ctypes in
  let open Foreign in
  Dl.dlopen ~filename:"./fastflow.so" ~flags:[Dl.RTLD_NOW]


let to_farm (k : ('a,'b,'c, 'd) kirc_function) ?ordered:(o=false) n  =
  incr id;
  let open Ctypes in
  let src = to_src k in
  let o = if o then "o" else "" in
  let lib = to_lib src in
  let create_accelerator = Foreign.foreign ~from:lib ("create_"^o^"farm") (int @-> string @-> string @-> returning (ptr farm) ) in
  let run_accelerator = Foreign.foreign ~from:lib ("run_"^o^"farm") (ptr farm @-> returning void) in
  let wait = Foreign.foreign ~from:lib (o^"wait") (ptr farm @-> returning void) in
  object (self) inherit fastflow
(*let module M = struct*)
    val accelerator = create_accelerator n ("./userfun"^(string_of_int !id)^".so") ("userfun"^(string_of_int !id))
    method accelerator = (Ctypes.to_voidp accelerator)
    method create_accelerator n = accelerator
    method run_accelerator = fun () -> run_accelerator accelerator
    method wait = fun () -> wait accelerator
    method source = src
    (* from generated object *)
    method offload_task = k.fastflow_acc#offloadTask self#accelerator
    method no_more_tasks = k.fastflow_acc#noMoreTasks self#accelerator
    method get_result = k.fastflow_acc#getResult self#accelerator
  end


let to_parallel_for (k : ('a,'b,'c, 'd) kirc_function) n =
  incr id;
  let open Ctypes in
  let src = to_src k in
  let lib = to_lib src in
  let create_pf = Foreign.foreign ~from:lib "create_pf" (int @-> string @-> string @-> returning (ptr parallel_for) ) in
  object (self)
    val pf = create_pf n ("./userfun"^(string_of_int !id)^".so") ("userfun"^(string_of_int !id))
    method pf = Ctypes.to_voidp pf
    method parallel_for = ()
  end
