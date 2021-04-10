
let rec take n xs () = 
    match n, xs () with
    | 0, _ -> Seq.Nil
    | n, Seq.Cons (x, xs) -> Seq.Cons (x, take (pred n) xs)
    | n, Seq.Nil (* n  > 0 *) -> Seq.Nil 


let calculate (x0, y0) (x1, y1) = 
    let dy = y1 - y0 
    and dx = x1 - x0 in 
    let steps = if abs dx > abs dy then abs dx else abs dy in 
    let x_inc = Float.of_int dx /. Float.of_int steps
    and y_inc = Float.of_int dy /. Float.of_int steps in 
    let round x = x |> Float.round |> Float.to_int in
    (Float.of_int x0, Float.of_int y0)
    |> Seq.unfold (fun (x, y) -> 
        let next = (x +. x_inc, y +. y_inc)
        and this = (round x, round y) in
        Some (this, next))
    |> take (succ steps)
    