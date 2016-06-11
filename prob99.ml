open Core.Std
open Core_bench.Std

(* ---- Types ---- *)
type cell = Black
          | Empty
          | Filled of char
type coord = {r:int; c:int}
type puzzle = {cells:cell array; size:coord}
type direction = Horizontal | Vertical
type range = {dir: direction;
              loc: coord;
              len: int}

(* ---- Functions ---- *)
let random_blank size =
  let f = (fun n -> match Random.bool () with true -> Black | false -> Empty) in
  let cells = Array.init (size.r*size.c) f in
  {cells; size}

let row p n : range = {dir=Horizontal; loc={r=n;c=0}; len=p.size.c}
let col p n : range = {dir=Vertical; loc={r=0;c=n}; len=p.size.r}

let rows p : range list =
  List.range 0 p.size.r
  |> List.map ~f:(row p)

let cols p : range list =
  List.range 0 p.size.c
  |> List.map ~f:(col p)

let coords range n =
  match range with
    (Horizontal,{r;c},_) -> {r=r;c=c+n}
  | (Vertical,{r;c},_) -> {r=r+n;c=c}

let get p {r;c} =
  let col = p.size.c in
  let idx = r*col + c in
  p.cells.(idx)

let to_char = function Black -> '#'
                     | Empty -> '_'
                     | Filled c -> c

(* let print_range r = *)
(*   let (_,_,n) = r in *)
(*   List.range 0 n *)
(*   |> List.map  *)


(* let main () = *)
(*   let p = random_blank {r=4;c=5} in *)
(*   print_endline "Tests:\n"; *)
(*   (p, rows p, cols p); *)
(*   let (_,s) = Map.max_elt_exn words in *)
(*   print_endline(List.hd_exn(s)); *)
(*   () *)

(* let _ = main () *)

(* type cursor = {p:puzzle; start:coord; d:direction} *)

(* type range = {c:cursor; len:int} *)

(* let make_puzzle size = *)
(*   Array.create ~len:(size.r*size.c) Empty *)

(* let rows puz = *)
(*   let r = puz.size.r in *)
(*   let idxs =  List.range 0 r in *)
(*   List.map ~f:(fun i->{p=puz; start={r=i; c=0}; d=Horizontal}) idxs *)

(* let cols puz = *)
(*   let c = puz.size.c in *)
(*   let idxs = List.range 0 c in *)
(*   List.map ~f:(fun i->{p=puz; start={r=0;c=i}; d=Vertical}) idxs *)


(* let get_coords puz coords = *)
(*   let cols = puz.size.c in *)
(*   let {r;c} = coords in *)
(*   puz.cells.(r*cols+c) *)

(* let get puz cur n = *)
(*   let {r;c} = cur.start in *)
(*   match cur.d with *)
(*     Vertical -> get_coords puz {r=r+n;c} *)
(*   | Horizontal -> get_coords puz {r;c=c+n} *)

(* let analyze puz = None *)

(* let len curs = None *)

(* let insert curs word = None *)
