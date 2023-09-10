let rec join (sep : string) (s : string list) : string =
  match s with
  | [] -> ""
  | head :: [] -> head
  | head :: tail -> head ^ sep ^ join sep tail
