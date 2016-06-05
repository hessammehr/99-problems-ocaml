open Core.Std

type cell = Black
          | Empty
          | Filled of char

type coord = {r:int; c:int}

type puzzle = {cells:cell array; size:coord}

type direction = Horizontal | Vertical

type cursor = {p:puzzle; start:coord; d:direction}

type range = {c:cursor; len:int}

let rows puz n =
  let {r;c} = puz.size in
  let idxs =  List.range ~stride:c 0 (r*c) in
  List.map ~f:(fun i->{p=puz; start={r=i; c=0}; d=Horizontal}) idxs

let get cur n =
  match cur.d with
  Horizontal -> None
  | Vertical -> None

let analyze puz = None

let len curs = None

let insert curs word = None
