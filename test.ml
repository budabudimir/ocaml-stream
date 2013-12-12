
open Streams
open Printf

let s = from 2;;

let rec sieve s =
   let h = match hd s with Some x -> x | None -> 0 in
   let p x = x mod h > 0 in
   Cons (h, fun () -> sieve (filter p s))
;;


let prims = sieve (from 2);;
List.iter (printf "%d\n") (take 1000 prims)

