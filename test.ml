

open Spoc
open FastFlow

let height = ref 0l
let width = ref 0l
let max_iter = ref 0l


let incrv = kern v length ->
  for i = 0 to length do
    if (i mod 2) = 0 then
      v.[<i>] <- v.[<i>] +. 1.
    else
      v.[<i>] <- v.[<i>] -. 1.;
  done



let _ =
  let module IncrV =
    (val (FastFlow.to_farm  (incrv)))
  in
  Printf.printf "%s\n" IncrV.source;
  IncrV.create_accelerator 24;
  IncrV.run_accelerator ();
  Unix.sleep 10;
