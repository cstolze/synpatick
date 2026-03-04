let rec insert x l =
  match l with
  | [] -> [x]
  | y :: l -> if x = y then y :: l else
    if x < y then x :: y :: l else
      y :: (insert x l)

let empty = []

let is_in x = List.mem x

let for_all f = List.for_all f

let cmp x y =
  try
    if x < y then -1 else if x = y then 0 else 1
  with
    _ -> -1

let sort cmp l =
  List.sort_uniq cmp l

let to_string f l =
  let foo =
    match l with
    | [] -> ""
    | [x] -> f x
    | x :: l -> List.fold_left (fun s x -> s ^ ", " ^ (f x)) (f x) l
  in "[" ^ foo ^ "]"

let union cmp l = sort cmp @@ List.concat l

let (let+) x f = union cmp (List.map f x)
let (let*) x f = union Tts.compare_trans (List.map f x)
let return x = [x]
