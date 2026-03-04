type action =
    Tau
  | Pos of string
  | Neg of string
  | Clock of string

val dual : action -> action

val action_to_string : action -> string

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

val compare_trans : transition -> transition -> int
