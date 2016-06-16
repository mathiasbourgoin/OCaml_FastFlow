open Kirc_Ast
open Kirc

let (^>) =fun a b -> a ^ "\n" ^ b

let fflib = Dl.dlopen ~filename:"./fastflow.so" ~flags:[Dl.RTLD_LAZY]


class virtual ['a] fastflow = object
  method virtual source : string
  method virtual create_accelerator : int -> unit
  method virtual run_accelerator : unit -> unit
  method virtual wait : unit -> unit
end


let to_src (_,kern) =
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

      void * userfun(void *);

    }" ^>

    "// this is a wrapper
    void * userfun(void * t) {
      TASK * res = f((TASK *) t);
      return ((void *) res);
    }"
  end
  in

  let ff_header = "#include <math.h>"
  in
  (*create_task_cfun kern.body;*)
  let (task, intro, body_source) = aux kern.body in
  ff_header ^ "\n\n" ^task ^"\n\n" ^ Ff.kern_start ^>
               intro ^ __ ^ body_source ^ Ff.kern_end ^"\n\n" ^ ff_lib

let to_lib src =
  let channel = open_out "userfun.cpp" in
  output_string channel src;
  close_out channel;
  ignore(Sys.command ("g++ -g -O3  -lm --shared -fPIC userfun.cpp -o userfun.so"));
  let open Ctypes in
  let open Foreign in
  Dl.dlopen ~filename:"./fastflow.so" ~flags:[Dl.RTLD_NOW]

let to_farm k =
  let src = to_src k in
  let lib = to_lib src in
  let open Ctypes in
  object (self) inherit [int] fastflow
    (*let module M = struct*)
    method source = src
    method create_accelerator = Foreign.foreign ~from:lib "create_accelerator" (int @-> returning void)
    method run_accelerator = Foreign.foreign ~from:lib "run_accelerator" (void @-> returning void)
    method wait = Foreign.foreign ~from:lib "wait" (void @-> returning void)
  end
