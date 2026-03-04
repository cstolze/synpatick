let minus l c =
  let open Tts in
  let test = function
    | Clock s when Set.is_in s c -> false
    | _ -> true
  in List.filter test l

let minus' c1 c2 =
  List.filter (fun x -> not (Set.is_in x c2)) c1

let transition c' f Tts.{ act; block; iota; cont } =
  let open Tts in
  let act = match act with
    | Tau | Pos _ | Neg _ -> act
    | Clock s -> if Set.is_in s c' then Tau else act
  in
  let block = Set.sort Set.cmp @@ List.map
      (fun (c, l) -> (minus' c c', minus l c')) block in
  let iota c = minus (iota (minus' c c')) c' in
  let cont = f cont in
  { act; block; iota; cont }

let rec hiding l p =
  object
    method to_string = p#to_string ^ "/" ^ (Set.to_string (fun x -> x) l)

    method clocks = minus' p#clocks l

    method ia = minus p#ia l

    method iastar c = minus (p#iastar (minus' c l)) l

    method trans = List.map (transition l (hiding l)) p#trans
  end
