type cell = Black
          | Empty
          | Filled of char

type coord = {r:int; c:int}

type puzzle = {cells:cell array; size:coord}

type direction = Horizontal | Vertical

type cursor = {p:puzzle; start:coord; d:direction}

let puz_cursor puz n dir =
  match dir with
    Horizontal -> {p:puzzle; r:n; c:0; d:Horizontal}
  | Vertical -> {p: puzzle; r:0; c:n; d:Vertical}

let get cur n =
  match cur.d with
  Horizontal -> Array.get cur.p ()

let analyze puz = None

let get p r c = Array.get p.cells (c+r*p.w)

let len curs = None

let insert curs word = None
