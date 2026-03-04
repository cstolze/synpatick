open Js_of_ocaml
open Synpa

let draw s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "draw")
    [| Js.Unsafe.inject (Js.string s) |]

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
          f (); draw (Graph.graphviz ()));
      g##.classList##add (Js.string "clickme")
    end
  | None -> ()

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

