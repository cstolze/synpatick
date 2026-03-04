open Js_of_ocaml
open Synpa

let draw s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "draw")
    [| Js.Unsafe.inject s |]

let alert s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "alert")
    [| Js.Unsafe.inject (Js.string s) |]

let install g n =
  let n' = int_of_string (Js.to_string n) in
  match Graph.install_callback n' with
  | Some f ->
    begin
      g##.onclick := Dom_html.handler (fun _ ->
          f (); draw (Js.string (Graph.graphviz ())));
      g##.classList##add (Js.string "clickme")
    end
  | None -> ()

let _example () =
  let _p = Parallel.parallel (Prefix.prefix (Tts.Pos("a")) [] (Zero.zero [])) (Prefix.prefix (Tts.Neg("a")) [] (Zero.zero [])) in
  let cell = Sum.sum (Sum.sum (Prefix.prefix (Tts.Neg "r") [Tts.Neg "w"; Tts.Neg "r"] (Name.name "cell" ["sigma"])) (Prefix.prefix (Tts.Neg "w") [Tts.Neg "w"] (Prefix.prefix (Tts.Clock "sigma") [] (Name.name "cell" ["sigma"])))) (Prefix.prefix (Tts.Clock "sigma") [Tts.Neg "w"; Tts.Neg "r"] (Name.name "cell" ["sigma"])) in
  let cell_name = Name.name "cell" ["sigma"] in
  let p = Prefix.prefix (Tts.Pos "r") [] (Zero.zero ["sigma"]) in
  let q = Prefix.prefix (Tts.Pos "w") [] (Zero.zero ["sigma"]) in
  let total = Parallel.parallel (Parallel.parallel cell_name p) q in
  begin
    Name.install "cell" cell;
    Graph.new_graph total;
    Graph.graphviz ()
  end

let parse s =
  Name.clear ();
  match Parser.parse (Js.to_string s) with
  | Ok (d,p) ->
    begin
      List.iter (fun (n, p) -> Name.install n p) d;
      Graph.new_graph p;
      draw (Graph.graphviz ())
    end
  | Error err -> alert err

let () =
  begin
    Js.export "graphviz" Graph.graphviz;
    Js.export "new_graph" Graph.new_graph;
    Js.export "install" install;
    Js.export "parse" parse
  end

