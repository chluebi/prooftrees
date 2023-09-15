let rec join (sep : string) (s : string list) : string =
  match s with
  | [] -> ""
  | head :: [] -> head
  | head :: tail -> head ^ sep ^ join sep tail

let crossproduct (xs : 'a list list) : 'a list list =
  match xs with
  | [] -> []
  | _ ->
      let f acc l =
        let g elt = List.map (fun x -> List.append x [ elt ]) acc in
        List.concat_map g l
      in
      List.fold_left f (List.map (fun x -> [ x ]) (List.hd xs)) (List.tl xs)
