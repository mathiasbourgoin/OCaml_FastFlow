

open Spoc
open FastFlow

let cpt = ref 0

let tot_time = ref 0.


let measure_time s f =
  let t0 = Unix.gettimeofday () in
  let a = f () in
  let t1 = Unix.gettimeofday () in
  Pervasives.flush stdout;
  Printf.printf "time %s : %Fs\n%!" s (t1 -. t0);
  tot_time := !tot_time +.  (t1 -. t0);
  incr cpt;
  a;;

(* for future use *)
(*klet simple_incr = fun i a ->
    a.[<i>] <- 1.
*)
let d = (Spoc.Devices.init ~only:Devices.OpenCL ()).(0)

klet multi_sin =
  fun v x ->
  let mutable a = (Std.float x) in
    for i = 0 to x  do
      (*if (z mod 2) = 0 then*)
      a := Math.Float32.sin a
(*  else
      a := Math.Float32.cos a*)
    done;
    v.[<x>] <- a;
    $"//printf (\"worker : %d -> res %g\\n \", x, a)"$;()


let gpu_sin = kern x n ->
  let open Std in
  let i = thread_idx_x + block_dim_x * block_idx_x in
  if i < n then
  begin
    let mutable a = (Std.float i) in
    for c = 0 to i do
      (*      if (i mod 2) = 0 then *)
        a := Math.Float32.sin a;
        (*      else
                a := Math.Float32.cos a;*)
    done;
    x.[<i>] <- a;
  end

let sequential_sin l =
  for i = 0 to l do
    let x = ref (Pervasives.float i) in
    for j = 0 to i do
      x := Pervasives.sin !x;
    done;
  done
  (*


klet incrv = fun v1  v2 v3 start_ end_ ->
(* native code directly embedded into generated kernel *)
$"printf(\"start : %d, end : %d\\n\", start_, end_)"$;
  for i = start_ to end_  do
    v3.[<i>] <- 0.;
    for j = 1 to i do
      v3.[<i>] <- v3.[<i>] +. (v1.[<i>]/. (Std.float i) +.
                                v2.[<i>] /. (Std.float i))
    done;
  done


let fill v1 v2 a1 a2 =
for i = 0 to Vector.length v1 - 1  do
  Mem.set v1 i ((float_of_int i)/.3.);
  Mem.set v2 i ((float_of_int i) *.2./.3.);

  a1.(i) = ((float_of_int i)/.3.);
  a2.(i) = ((float_of_int i) *. 2. /.3.);
done
*)
let size = 10000


let example0 () =
  measure_time "FF_CPU" (fun () ->
  let v = Vector.create Vector.float32 size in
  let accelerator = (FastFlow.to_farm multi_sin  ~ordered:false 10) in
  accelerator#run_accelerator ();
  let rec offtask i =
    if i < size then
      (accelerator#offload_task (v,i);
       offtask (i + 1))
    else
      accelerator#no_more_tasks;
  and get_res i =
  if i < size then
    (accelerator#get_result (v,i);
     get_res (i + 1))
  in
  offtask 0;
  get_res 0;
  accelerator#wait())


let example0_gpu () =

  Printf.printf "Will use device : %s  with %d floats\n%!"
    d.Spoc.Devices.general_info.Spoc.Devices.name size;
  measure_time "GPU" (fun () -> let v = Vector.create Vector.float32 size in
  let threadsPerBlock = match d.Devices.specific_info with
    | Devices.OpenCLInfo clI ->
      (match clI.Devices.device_type with
       | Devices.CL_DEVICE_TYPE_CPU -> 1
       | _  ->   256)
    | _  -> 256 in
  let blocksPerGrid =
    (size + threadsPerBlock -1) / threadsPerBlock in
  let block0 = {Spoc.Kernel.blockX = threadsPerBlock;
                Spoc.Kernel.blockY = 1; Spoc.Kernel.blockZ = 1}
  and grid0= {Spoc.Kernel.gridX = blocksPerGrid;
              Spoc.Kernel.gridY = 1; Spoc.Kernel.gridZ = 1} in
  ignore(Kirc.gen ~only:Devices.OpenCL
                         gpu_sin);
  Kirc.run gpu_sin (v,size) (block0, grid0) 0 d;
  Devices.flush d ();
  (for i = 0 to size - 1 do
    Printf.printf "worker : %d -> res %g\n " i (Mem.get v i)
  done;
  Pervasives.flush stdout;))

let example0_seq () =
  measure_time "seq" (fun () -> sequential_sin size)
(*
let example1 () =
  Printf.printf "Starting\n%!";
  let size = 4096*2*2*2 and segment_size = 128 in

  let v1 = Vector.create Vector.float32 size in
  let v2 = Vector.create Vector.float32 size in
  let v3 = Vector.create Vector.float32 size in
  let a1 = Array.make size 0. in
  let a2 = Array.make size 0. in
  Printf.printf "Vectors created\n%!";

  fill v1 v2 a1 a2;
  Printf.printf "Vectors 1 and 2 initialized\n%!";

  measure_time "sarek+fastflow" (fun () ->
      let incrv = (FastFlow.to_farm incrv  ~ordered:false 10)
  (*and incrv2 = (FastFlow.to_farm incrv2 5)*)
  in
  Printf.printf "Farm generated\n%!";
  (*Printf.printf "%s\n" incrv#source;*)
  (*incrv#create_accelerator 1;*)
  Printf.printf "Accelerator created\n%!";
  incrv#run_accelerator ();
  (*  incrv2#run_accelerator ();*)
  Printf.printf "Accelerator launched\n%!";

  for x = 0 to Vector.length v1/segment_size - 1 do
    incrv#offload_task (v1, v2, v3, (x*segment_size), ((x+1)*segment_size-1));
  done;
  Printf.printf "Tasks offloaded to accelerator\n%!";
  incrv#no_more_tasks;
  Printf.printf "No more tasks\n%!";
  for x = 0 to Vector.length v1/segment_size - 1 do
    incrv#get_result (v1, v2, v3, 0, 0);
  done;
  Printf.printf "Results!\n%!";
  incrv#wait(););

  ignore(
    measure_time "sequential"
      (fun () -> Array.init size
          (fun i -> let tmp = ref 0. in
            for j = 0 to i do
              tmp := !tmp +. (a1.(i) /. (float_of_int i)) +.
                     (a2.(i)) /. (float_of_int i)
            done;
            !tmp))
  )


(*klet incr = fun a b c start_ end_ ->
  $"printf(\"Task : %d to %d starts\n\", start_, end_)"$;
  for i = start_ to end_ do
    c.[<i>] <- a.[<i>] + b.[<i>]
  done
(* ...  *)
let accelerator = FastFlow.to_farm incr 10 in
(* ... *)
accelerator#offload_task (v1, v2, vres, 0, 511);
(*... *)
accelerator#wait*)


(*
let rec map f s =
  Stream.slazy
    (fun _ ->
       let (__strm : _ Stream.t) = s
       in
       match Stream.peek __strm with
       | Some h ->
         (Stream.junk __strm;
          Stream.lcons (fun _ -> f h)
            (Stream.slazy (fun _ -> map f s)))
       | _ -> Stream.sempty)




let example2 () =
  stream_map (fun a ->
      print_char a;
      Pervasives.flush stdout;
      a)
    (Stream.of_channel stdin)

let _ =
  Stream.iter (print_char) (example2 ())
  *)
*)
let _ = example0 (); example0_gpu (); example0_seq ()
