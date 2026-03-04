type action =
    Tau
  | Pos of string
  | Neg of string
  | Clock of string

let dual = function
  | Tau -> Tau
  | Pos a -> Neg a
  | Neg a -> Pos a
  | Clock a -> Clock a

let action_to_string = function
  | Tau -> "tau"
  | Pos s -> "!" ^ s
  | Neg s -> "?" ^ s
  | Clock s -> "#" ^ s

type clockset = string list
type actionset = action list

type prediction = clockset -> actionset
type blocking = clockset * actionset
type blockrel = blocking list

type transition =
  {
    act: action;
    block: blockrel;
    iota: prediction;
    cont: process
  }
and transset = transition list
and process = < clocks: clockset; ia: actionset; iastar: prediction; to_string: string; trans: transset >

let compare_trans t1 t2 =
  let { act = act1; block = block1; iota = iota1; cont = cont1 } = t1 in
  let { act = act2; block = block2; iota = iota2; cont = cont2 } = t2 in
  if act1 < act2 then -1
  else if act1 > act2 then 1
  else if block1 < block2 then -1
  else if block1 > block2 then 1
  else let s1, s2 = cont1#to_string, cont2#to_string in
    if s1 < s2 then -1
    else if s1 > s2 then 1
    else if iota1 [] = iota2 [] then 0
    else -1
