let prefix alpha l p =
  begin
    match alpha with
    | Tts.Clock s -> assert (Set.is_in s p#clocks)
    | _ -> ()
  end;
  object
    method to_string = Tts.action_to_string alpha ^ ":" ^ Set.to_string Tts.action_to_string l ^ "." ^ p#to_string

    method clocks = p#clocks

    method ia = [alpha]

    method iastar c =
      match alpha with
      | Clock s when Set.is_in s c -> [alpha]
      | Tau -> p#iastar c
      | _ -> Set.insert alpha @@ p#iastar c

    method trans = [Tts.{ act = alpha; block = [(p#clocks, l)]; iota = (fun _ -> []); cont = p }]
end


