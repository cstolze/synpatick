let eschew (iota : Tts.prediction) (block : Tts.blockrel) : bool =
  let inter c l =
    List.filter (fun x -> Set.is_in (Tts.dual x) l) @@ iota c
  in
  Set.for_all (fun (c,l) -> inter c l = Set.empty) block

let no_comm act block iota cont iastar clk f =
  let open Set in
  let+ () = (* check that it is not a clock action in the clock set of the other process *)
    match act with
    | Tts.Clock s -> if is_in s clk then empty else return ()
    | _ -> return ()
  in
  if eschew iastar block then
    return Tts.{ act = act; block = block; iota = (fun c -> union cmp [iota c; iastar c]); cont = f cont}
  else empty

let comm act1 act2 block1 block2 iota1 iota2 cont1 cont2 f =
  let open Set in
  let open Tts in
  let+ act = match act1, act2 with
    | Tau, _ | _, Tau | Pos _, Pos _ | Pos _, Clock _ | Neg _, Neg _ | Neg _, Clock _
    | Clock _, Pos _ | Clock _, Neg _ -> empty
    | Pos a, Neg b | Neg b, Pos a -> if a = b then return Tau else empty
    | Clock a, Clock b -> if a = b then return (Clock a) else empty
  in
  if eschew iota1 block2 && eschew iota2 block1 then
    return { act = act; block = union cmp [block1; block2]; iota = (fun c -> union cmp [iota1 c; iota2 c]); cont = f cont1 cont2 }
  else empty

let compose l1 l2 iastar1 iastar2 clk1 clk2 f1 f2 f3 =
  let open Tts in
  let open Set in
  union compare_trans [
    begin
      let* { act = act1; block = block1; iota = iota1; cont = cont1 } = l1 in
      no_comm act1 block1 iota1 cont1 iastar2 clk2 f1
    end;
    begin
      let* { act = act2; block = block2; iota = iota2; cont = cont2 } = l2 in
      no_comm act2 block2 iota2 cont2 iastar1 clk1 f2
    end;
    begin
      let* { act = act1; block = block1; iota = iota1; cont = cont1 } = l1 in
      let* { act = act2; block = block2; iota = iota2; cont = cont2 } = l2 in
      comm act1 act2 block1 block2 iota1 iota2 cont1 cont2 f3
    end]

let rec parallel p q =
  object
    method to_string = "(" ^ p#to_string ^ "|" ^ q#to_string ^ ")"

    method clocks = Set.union Set.cmp [p#clocks; q#clocks]

    method ia = Set.union Set.cmp [p#ia; q#ia]

    method iastar c = Set.union Set.cmp [p#iastar c; q#iastar c]

    method trans = compose p#trans q#trans p#iastar q#iastar p#clocks q#clocks (fun p -> parallel p q) (fun q -> parallel p q) parallel
  end
