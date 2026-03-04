let zero c = object
  method to_string = "0_" ^ Set.to_string (fun x -> x) c

  method clocks = c

  method ia = Set.empty

  method iastar _ = Set.empty

  method trans = Set.empty
end
