

open Spoc
open FastFlow

let cpt = ref 0

let tot_time = ref 0.


let measure_time s f =
  let t0 = Unix.gettimeofday () in
  let a = f () in
  let t1 = Unix.gettimeofday () in
  Printf.printf "time %s : %Fs\n%!" s (t1 -. t0);
  tot_time := !tot_time +.  (t1 -. t0);
  incr cpt;
  a;;

klet simple_incr = kfun a ->
a + 1

klet incrv = kfun v1 v2 v3 start_ end_ ->
  $"printf(\"start : %d, end : %d\\n\", start_, end_)"$;
  for i = start_ to end_  do
    v3.[<i>] <- 0.;

    for j = 1 to i do
      v3.[<i>] <- v3.[<i>] +. (v1.[<i>]/. (Std.float i) +.
                               v2.[<i>] /. (Std.float i));
    done
  done



(* klet incrv2 = kfun v1 v2 v3 start_ end_ ->
  begin
    for i = start_ to end_  do
      v3.[<i>] <- (v1.[<i>]/. (Std.float i) +.
                   v2.[<i>] /. (Std.float i)) +. 1.;
    done
   end *)


let fill v1 v2 a1 a2 =
for i = 0 to Vector.length v1 - 1  do
  Mem.set v1 i ((float_of_int i)/.3.);
  Mem.set v2 i ((float_of_int i) *.2./.3.);

  a1.(i) = ((float_of_int i)/.3.);
  a2.(i) = ((float_of_int i) *. 2. /.3.);
done

let _ =
  Printf.printf "Starting\n%!";
  let size = 4096 and segment_size = 128 in

  let v1 = Vector.create Vector.float32 size in
  let v2 = Vector.create Vector.float32 size in
  let v3 = Vector.create Vector.float32 size in
  let a1 = Array.make size 0. in
  let a2 = Array.make size 0. in
  Printf.printf "Vectors created\n%!";

  fill v1 v2 a1 a2;
  Printf.printf "Vectors 1 and 2 initialized\n%!";

  measure_time "fastflow" (fun () ->
  let incrv = (FastFlow.to_farm incrv  200)
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

  let a3 =
    measure_time "Sequentiel"
      (fun () -> Array.init size
          (fun i -> let tmp = ref 0. in
            for j = 0 to i do
              tmp := !tmp +. (a1.(i) /. (float_of_int i)) +.
                     (a2.(i)) /. (float_of_int i)
            done;
            !tmp))
  in ();
  for i = 1 to 100 (*Vector.length v1 - 1*) do
    Printf.printf "v3[%d] = %g\n%!" i     (Mem.get v3 i)
  done;
  Unix.sleep 1;
