

open Spoc
open FastFlow

let incrv = kern v1 v2 v3 start_ end_ ->
  for i = start_ to end_ do
      v3.[<i>] <- v1.[<i>] +. v2.[<i>];
  done

open Task_0



let _ =
  let incrv = (FastFlow.to_farm incrv)
  in
  (*Printf.printf "%s\n" incrv#source;*)
  incrv#create_accelerator 2;
  incrv#run_accelerator ();
  let v1 = Vector.create Vector.float32 20480 in
  let v2 = Vector.create Vector.float32 20480 in
  let v3 = Vector.create Vector.float32 20480 in

  for i = 0 to Vector.length v1 - 1  do
    Mem.set v1 i (float_of_int i);
    Mem.set v2 i (float_of_int i);
  done;

  for x = 0 to Vector.length v1/512 do
    offloadTask (v1, v2, v3, (x*512), ((x+1)*512-1));
  done;

  accNomoretasks();
  for x = 0 to Vector.length v1/512 do
    getResult (v1, v2, v3, 0, 0);
  done;
  for i = 0 to Vector.length v1 - 1 do
      Printf.printf "%g + %g = %g\n" (Mem.get v1 i) (Mem.get v2 i) (Mem.get v3 i)
  done;
  Unix.sleep 1;
