
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream);;


let rec from x =
   Cons (x, fun () -> from (x + 1))
;;


let hd = function
   | Nil         -> None
   | Cons (x, _) -> Some x
;;


let tl = function
   | Nil         -> None
   | Cons (_, t) -> Some (t ())
;;


let take x s =
   let rec aux acc x  = function
      | Nil -> acc
      | Cons (v, t) -> if x > 0 then aux (v::acc) (x - 1) (t ()) else acc
   in List.rev (aux [] x s)
;;


let takeWhile p s =
   let rec aux acc = function
      | Nil -> acc
      | Cons (x, t) -> if p x then aux (x::acc) (t ()) else acc
   in List.rev (aux [] s)
;;


let rec drop x = function
   | Nil -> Nil
   | Cons (_, t) -> if x > 0 then drop (x - 1) (t ()) else (t ())
;;


let rec dropWhile p = function
   | Nil -> Nil
   | Cons (x, t) -> if p x then dropWhile p (t ()) else t ()
;;


let from_list l =
   List.fold_right (fun x s -> Cons (x, fun () -> s)) l Nil
;;


let rec kth k = function
   | Nil -> None
   | Cons (x, t) -> if k <= 0 then Some x else kth (k - 1) (t ())
;;


let rec map f = function
   | Nil -> Nil
   | Cons (x, t) -> Cons (f x, fun () -> map (f) (t ()))
;;


let rec filter p = function
   | Nil -> Nil 
   | Cons (x, t) when p x -> Cons (x, fun () -> filter (p) (t ()))
   | Cons (x, t)          -> filter p (t ())
;;


