

open Spoc
open FastFlow

klet incrv = kfun v1 v2 v3 start_ end_ ->
    begin
      for i = start_ to end_  do
        v3.[<i>] <- v1.[<i>] +. v2.[<i>];
        (*(v1.[<i>]/. (Std.float i) +.
                     v2.[<i>] /. (Std.float i));*)
      done
    end



klet incrv2 = kfun v1 v2 v3 start_ end_ ->
  begin
    for i = start_ to end_  do
      v3.[<i>] <- (v1.[<i>]/. (Std.float i) +.
                   v2.[<i>] /. (Std.float i)) +. 1.;
    done
  end


let _ =
  Printf.printf "Starting\n%!";
  let size = 1024 and segment_size = 512 in
  let incrv = (FastFlow.to_farm incrv2 3)
  and incrv2 = (FastFlow.to_farm incrv 5)
  in
  Printf.printf "Farm generated\n%!";
  (*Printf.printf "%s\n" incrv#source;*)
  (*incrv#create_accelerator 1;*)
  Printf.printf "Accelerator created\n%!";
  incrv#run_accelerator ();
  Printf.printf "Accelerator launched\n%!";
  let v1 = Vector.create Vector.float32 size in
  let v2 = Vector.create Vector.float32 size in
  let v3 = Vector.create Vector.float32 size in
  Printf.printf "Vectors created\n%!";
  for i = 0 to Vector.length v1 - 1  do
    Mem.set v1 i ((float_of_int i)/.3.);
    Mem.set v2 i ((float_of_int i) *.2./.3.);
  done;
  Printf.printf "Vectors 1 and 2 initialized\n%!";
  for x = 0 to Vector.length v1/segment_size - 1 do
    Incrv.offloadTask incrv#accelerator (v1, v2, v3, (x*segment_size), ((x+1)*segment_size-1));
  done;
  Printf.printf "Tasks offloaded to accelerator\n%!";
  Incrv.accNomoretasks incrv#accelerator;
  Printf.printf "No more tasks\n%!";
  for x = 0 to Vector.length v1/segment_size - 1 do
    Incrv.getResult incrv#accelerator (v1, v2, v3, 0, 0);
  done;
  Printf.printf "Results!\n%!";
  incrv#wait();
  for i = 0 to Vector.length v1 - 1 do
    Printf.printf "%g + %g = %g\n%!"
      ((Mem.get v1 i) /. (float_of_int i))
      ((Mem.get v2 i) /. (float_of_int i))
      (Mem.get v3 i)
  done;
  Unix.sleep 1;
