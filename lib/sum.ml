let sum p q =
  assert (p#clocks = q#clocks);
  object
    method to_string = "(" ^ p#to_string ^ "+" ^ q#to_string ^ ")"

    method clocks = p#clocks

    method ia = Set.union Set.cmp [p#ia; q#ia]

    method iastar c = Set.union Set.cmp [p#iastar c; q#iastar c]

    method trans =
      let update_iota ia t =
        Tts.{act = t.act;
             block = t.block;
             iota = (fun c -> Set.union Set.cmp [t.iota c; ia]);
             cont = t.cont
        }
      in
      Set.union Tts.compare_trans [List.map (update_iota q#ia) p#trans; List.map (update_iota p#ia) q#trans]
  end
