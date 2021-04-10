(* open Common *)

let initial dy dx = 2*dy - dx

let determine_y p y dy dx =
    (* Printf.printf "old p = %d \n" p ; *)
    let p' =  p +  2*dy in
    (* Printf.printf "p + 2*dy = %d \n" p'; *)
    if p >= 0 then 
        begin 
        (* Printf.printf "p > 0 \n"; *)
        (* Printf.printf "%d + 2*%d - 2*%d = %d \n" p dy dx (p' - 2*dx) ; *)
        p' - 2*dx, succ y 
        end
    else 
        p', y

let calculate (x0, y0) (x1, y1) = 
    let dy = y1 - y0 
    and dx = x1 - x0 in 
    (* Printf.printf "dx = %d, dy  = %d\n" dx dy ; *)
    (x0, y0, initial dy dx)
    |> Seq.unfold (fun ((x, y, p) as this) -> 
        let (p', y') = determine_y p y dy dx in
        let next = (succ x, y', p') in
        if x <= x1 then 
            Some (this, next)
        else 
            None) 