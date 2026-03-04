let name' n c =
  object
    val mutable process : Tts.process option = None

    method to_string = n ^ (Set.to_string (fun x -> x) c)

    method clocks = c

    method ia =
      match process with
      | None -> Set.empty
      | Some p ->
        begin
          process <- None;
          let res = p#ia in
          process <- Some p; res
        end

    method iastar c = if process = None then Set.empty else
      match process with
      | None -> Set.empty
      | Some p ->
        begin
          process <- None;
          let res = p#iastar c in
          process <- Some p; res
        end

    method trans = if process = None then Set.empty else
      match process with
      | None -> Set.empty
      | Some p ->
        begin
          process <- None;
          let res = p#trans in
          process <- Some p; res
        end

    method install p = process <- Some p
  end

let table = Hashtbl.create 64

let name n c =
  let c = Set.sort Set.cmp c in
  let p =
    if Hashtbl.mem table n then
      let p = Hashtbl.find table n in
      begin
        assert (c = p#clocks); p
      end
    else
      let p = name' n c in
      begin
        Hashtbl.add table n p; p
      end
  in (p :> Tts.process)

let install n p =
  if Hashtbl.mem table n then
    let q = Hashtbl.find table n in
    begin
      assert (q#clocks = p#clocks); q#install p
    end

let clear () = Hashtbl.clear table
