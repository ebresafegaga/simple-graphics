open Common 
let calculate r =
    let square x = x * x in
    let initial = 1 - r 
    and next_values (x, y, p) =
        let (x1, y1) = if p >= 0 then  (succ x, pred y) else (succ x, y) in 
        let p1 = p + 2*(x + 1) + (square y1 - square y) - (y1 - y) + 1 in
        (x1, y1, p1)
    in
    (0, r, initial) 
    |> Seq.unfold (fun this -> Some (this, next_values this))
    |> Seq.take_while (fun (x, y, _) -> x < y)