1.

Let ultime_cifre x = 
    let y = abs(x mod 10)
    in let z = abs(x/10) mod 10
    in (z, y);;


Let ultime_cifre2 x = (abs(x/10) mod 10
                        abs(x mod 10))

2.
Let bellino x = match abs(x mod 10) with
    0 | 3 | 7 -> true
    | _ -> false

Let bello x =
    match abs(x mod 10) with
        0|3|7 -> (match (abs(x mod 100))/10 with
                    0|3|7 -> false
                    | _ -> true)
    | _ -> false

Let bello x = 
    match (abs x) < 10 with
    true -> bellino x
    | _ -> let (a,b) = ultime_cifre2 x
    in bellino b && not bellino a
    


