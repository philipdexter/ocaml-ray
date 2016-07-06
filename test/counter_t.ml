open Printf
module Scheduler = Sched_ws_affine.Make(struct let num_domains = 4 end)
module Reagents = Reagents.Make(Scheduler)
open Scheduler
open Reagents

module Sync = Reagents_sync.Make(Reagents)
module Lock = Sync.Lock
module CV = Sync.Condition_variable

let id_str () = sprintf "%d:%d" (Domain.self ()) (get_tid ())

let count = ref 0

let count_up_to l n =
    for i = 1 to n do
      Printf.eprintf "counting\n" ;
      run (Lock.acq l) () ;
      Printf.eprintf "count here is %d\n" !count ;
      count := !count + 1 ;
      run (Lock.rel l) () ;
    done

let main () =
  let l = Lock.create () in

  for i = 0 to 1 do
    fork (fun () -> count_up_to l 100)
  done ;

  Unix.sleep 3 ;

  Printf.eprintf "final count: %d\n" !count


let () = Scheduler.run main
