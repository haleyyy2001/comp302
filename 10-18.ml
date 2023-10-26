let rec (@) (l1 : 'a list) (l2 : 'a list) : 'a list =
    match l1 with
    | [] -> l2
    | x :: xs -> x :: (xs @ l2)

let rec rev (l : 'a list) : 'a list =
    match l with
    | [] -> []
    | x :: xs -> rev xs @ [x]
