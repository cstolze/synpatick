let nodes : (int, Tts.process * bool) Hashtbl.t = Hashtbl.create 64

let edges : (int * string * int) list ref= ref []

let block_to_string b =
  let foo l = Set.to_string (fun x -> x) l in
  let bar l = Set.to_string Tts.action_to_string l in
  Set.to_string (fun (c,l) -> "(" ^ (foo c) ^ "," ^ (bar l) ^ ")") b

let develop n =
  if Hashtbl.mem nodes n then
    let p, b = Hashtbl.find nodes n in
    assert(b);
    let t = p#trans in
    begin
      Hashtbl.replace nodes n (p,false); (* has been clicked *)
      List.iter (fun Tts.{act=act; block=block; iota=_; cont=cont} ->
          let lbl = (Tts.action_to_string act) ^ "/" ^ (block_to_string block) in
          let m = Hashtbl.hash cont#to_string in
          if (not (Hashtbl.mem nodes m)) then
            Hashtbl.add nodes m (cont, true); (* new node *)
          edges := (n, lbl, m) :: !edges
        ) t
    end

let graphviz () =
  let print_nodes n (p, _) t =
    (string_of_int n) ^ " [label=\"" ^ p#to_string ^ "\", shape=box]; " ^ t in
  let print_edges t (m, l, n) =
    (string_of_int m) ^ " -> " ^ (string_of_int n) ^ " [label=\"" ^ l ^ "\"]; " ^ t in
  let nodes = Hashtbl.fold print_nodes nodes "" in
  let edges = List.fold_left print_edges "" !edges in
  "digraph { " ^ nodes ^ edges ^ "}"

let new_graph p =
  Hashtbl.clear nodes;
  edges := [];
  Hashtbl.add nodes (Hashtbl.hash p#to_string) (p, true)

let install_callback n =
  assert(Hashtbl.mem nodes n);
  let (_, b) = Hashtbl.find nodes n in
  if b then Some (fun () -> develop n)
      else None
