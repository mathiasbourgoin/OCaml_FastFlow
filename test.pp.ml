open Spoc
  
open FastFlow
  
let height = ref 0l
  
let width = ref 0l
  
let max_iter = ref 0l
  
let incrv = let module Local_funs = struct  end
  in let open Local_funs
    in
      let module M =
        struct
          open Ctypes
            
          type task
          
          let task : (task structure) typ = structure "TASK"
            
          let task_length = field task "task_length" int
            
          let task_v = field task "task_v" (ptr float)
            
          let () = seal task
            
          let create_task (v, length) =
            let t = make task
            in (Ctypes.setf t task_v v; Ctypes.setf t task_length length; t)
            
          let exec_fun ((spoc_var0 : ('spoc_i, 'spoc_j) Vector.vector),
                        (spoc_var1 : int))
                       =
            Spoc.Kernel.exec
              [| Spoc.Kernel.VFloat32 (Spoc.Kernel.relax_vector spoc_var0);
                Spoc.Kernel.Int32 spoc_var1
              |]
            
          class ['a, 'b] kirc_class0 =
            object (self)
              inherit
                [(((float, Bigarray.float32_elt) Vector.vector) * int),
                  (('a, 'b) Kernel.kernelArgs) array] Spoc.Kernel.spoc_kernel
                  "kirc_kernel" "spoc_dummy"
              method exec = exec_fun
              method args_to_list =
                fun
                  ((spoc_var0 : ('spoc_i, 'spoc_j) Vector.vector),
                   (spoc_var1 : int))
                  ->
                  [|
                    Spoc.Kernel.VFloat32 (Spoc.Kernel.relax_vector spoc_var0);
                    Spoc.Kernel.Int32 spoc_var1
                  |]
              method list_to_args =
                function
                | [| Spoc.Kernel.VFloat32 spoc_var0;
                    Spoc.Kernel.Int32 spoc_var1 |] ->
                    ((spoc_var0 :
                       (float, Bigarray.float32_elt) Vector.vector),
                     (spoc_var1 : int))
                | _ -> failwith "spoc_kernel_extension error"
            end
            
        end
      in let open Kirc
        in
          ((new M.kirc_class0),
           {
             ml_kern =
               (fun v length ->
                  for spoc_tmp = Int32.to_int 0l to Int32.to_int length do
                    let i = Int32.of_int spoc_tmp
                    in
                      if (Int32.rem i 2l) = 0l
                      then
                        Spoc.Mem.set
                          (v :
                            (float, Bigarray.float32_elt) Spoc.Vector.vector)
                          (Int32.to_int i)
                          ((Spoc.Mem.get
                              (v :
                                (float, Bigarray.float32_elt) Spoc.Vector.
                                  vector)
                              (Int32.to_int i))
                             +. 1.)
                      else
                        Spoc.Mem.set
                          (v :
                            (float, Bigarray.float32_elt) Spoc.Vector.vector)
                          (Int32.to_int i)
                          ((Spoc.Mem.get
                              (v :
                                (float, Bigarray.float32_elt) Spoc.Vector.
                                  vector)
                              (Int32.to_int i))
                             -. 1.)
                  done);
             body =
               spoc_gen_kernel
                 (params
                    (concat (new_float_vec_var 0 "v")
                       (concat (new_int_var 1 "length") (empty_arg ()))))
                 (spoc_do (var 3 "i") (spoc_int32 0l) (var 1 "length")
                    (spoc_ife
                       (equals32 (spoc_mod (var 3 "i") (spoc_int32 2l))
                          (spoc_int32 0l))
                       (set_vect_var (get_vec (var 0 "v") (var 3 "i"))
                          (spoc_plus_float (get_vec (var 0 "v") (var 3 "i"))
                             (spoc_float 1.)))
                       (set_vect_var (get_vec (var 0 "v") (var 3 "i"))
                          (spoc_min_float (get_vec (var 0 "v") (var 3 "i"))
                             (spoc_float 1.)))));
             ret_val = ((return_unit ()), (Vector.Unit ((), ())));
             extensions = [| ExFloat32 |];
           })
  
let _ = let module IncrV = (val FastFlow.to_farm incrv)
  in
    (Printf.printf "%s\n" IncrV.source;
     IncrV.create_accelerator 24;
     IncrV.run_accelerator ();
     Unix.sleep 10)
  

