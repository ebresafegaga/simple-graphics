open Common 

let calculate (x0, y0) (x1, y1) = 
    let dy = y1 - y0 and dx = x1 - x0 in
    let next_values (x, y, p) = 
        if p >= 0 
        then succ x, succ y, p + 2*dy - 2*dx 
        else succ x, y, p + 2*dy
    and initial = 2*dy - dx in
    (x0, y0, initial)
    |> Seq.unfold (fun this -> Some (this, next_values this))
    |> Seq.take_while (fun (x, _, _) -> x <= x1)


