open Angstrom

let is_alphabet c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
let is_digit c = c >= '0' && c <= '9'
let is_alphanum c = is_alphabet c || is_digit c
let is_alphanum' c = is_alphanum c || c = '\'' || c = '_' || c = '-'
let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false


let whitespace =
  take_while is_whitespace *> return ()

let id = consumed @@
  satisfy is_alphabet *> take_while is_alphanum'

(* returns a non-empty list, or fails *)
let listing' f c =
  begin
    let* x = f in
    let+ l = many @@ whitespace *> char c *> whitespace *> f in
    x :: l
  end

let listing f =
  char '[' *> whitespace *> (listing' f ',' <|> return []) <* whitespace <* char ']'

let polarity =
  char '!' *> return (fun x -> Tts.Pos x) <|>
  char '?' *> return (fun x -> Tts.Neg x) <|>
  char '#' *> return (fun x -> Tts.Clock x)

let act =
  string "tau" *> return Tts.Tau <|> (polarity <*> id)

let zero =
  let+ s = string "0_" *> listing id in
  Zero.zero (Set.sort Set.cmp s)

let name =
  let* n = id in
  let+ s = listing id in
  Name.name n (Set.sort Set.cmp s)

let prefix p =
  let* a = act in
  let* block = (whitespace *> char ':' *> whitespace *> listing act) <|> return [] in
  let+ p = whitespace *> char '.' *> whitespace *> p in
  Prefix.prefix a block p

let close_prefix p =
  fix (fun foo -> prefix foo <|> p)

let sum f =
  let+ l = listing' f '+' in
  List.fold_left (fun p x -> Sum.sum p x) (List.hd l) (List.tl l)

let parallel f =
  let+ l = listing' (sum f) '|' in
  List.fold_left (fun p x -> Parallel.parallel p x) (List.hd l) (List.tl l)

let hiding p =
  let+ l = whitespace *> char '/' *> whitespace *> listing id in
  Hiding.hiding (Set.sort Set.cmp l) p

let restriction p =
  let+ l = whitespace *> char '\\' *> whitespace *> listing id in
  Restriction.restriction (Set.sort Set.cmp l) p

let rec close p =
  (hiding p >>= close) <|> (restriction p >>= close) <|> return p

let main' f =
  let parens = char '(' *> whitespace *> f <* whitespace <* char ')' in
  let foo = close_prefix (zero <|> name <|> parens) in
  parallel foo >>= close

let main =
  whitespace *> fix (fun f -> main' f)

let decl =
  let* name = id in
  let+ p = whitespace *> string ":=" *> whitespace *> main <* whitespace <* char ';' <* whitespace in
  (name, p)

let program =
  whitespace *>
  let* d = many decl in
  let+ p = main in
  (d, p)

let parse s =
  try
    parse_string ~consume:Consume.Prefix program s
  with
  _ -> Error "ill-formed term"
