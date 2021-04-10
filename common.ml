module Seq = struct 
    include Seq
    let rec take n xs () = 
        match n, xs () with
        | 0, _ -> Seq.Nil
        | n, Seq.Cons (x, xs) -> Seq.Cons (x, take (pred n) xs)
        | n, Seq.Nil (* n  > 0 *) -> Seq.Nil 
    
    let rec take_while f xs () = 
        match xs () with 
        | Seq.Cons (x, xs) -> 
            if f x then 
                Seq.Cons (x, take_while f xs) 
            else 
                Seq.Nil 
        | Seq.Nil -> Seq.Nil
end

(*

cylic modules?

let a = 10 

let f  x= x

let x = Common.f Common.a *)