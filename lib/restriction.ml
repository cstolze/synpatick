let minus l a =
  let open Tts in
  let test = function
    | Pos s | Neg s when Set.is_in s a -> false
    | _ -> true
  in List.filter test l

let restr_trans t a f =
  let open Set in
  let open Tts in
  let* { act; block; iota; cont } = t in
  let+ () =
    match act with
    | Pos s | Neg s when Set.is_in s a -> empty
    | _ -> return ()
  in
  let block = Set.sort Set.cmp @@ List.map (fun (c, l) -> (c, minus l a)) block in
  let iota c = minus (iota c) a in
  let cont = f cont in
  return {act; block; iota; cont}

let rec restriction l p =
  object
    method to_string = p#to_string ^ "\\\\" ^ (Set.to_string (fun x -> x) l)

    method clocks = p#clocks

    method ia = minus p#ia l

    method iastar c = minus (p#iastar c) l

    method trans = restr_trans p#trans l (restriction l)
  end
