
let filter_stream p s =
   let rec next i =
      try
         let v = Stream.next s in
         if p v then Some v else next i
      with Stream.Failure -> None
   in Stream.from next
;;

let stream_from x = Stream.from (fun i -> Some (i + x));;

let rec sieve s =
   let v = Stream.next s in
   let f x = (x mod v) > 0 in
   sieve (filter_stream f s)
;;

let take x s =
   let rec aux acc x s =
      if x = 0 then acc
      else aux ((Stream.next s)::aux) (x-1) s
   in aux [] x s
;;

List.iter (printf "%d\n") take 10 (sieve (stream_from 2))
