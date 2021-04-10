let rec take n xs () = 
  match n, xs () with
  | 0, _ -> Seq.Nil
  | n, Seq.Cons (x, xs) -> Seq.Cons (x, take (pred n) xs)
  | n, Seq.Nil (* n  > 0 *) -> Seq.Nil 