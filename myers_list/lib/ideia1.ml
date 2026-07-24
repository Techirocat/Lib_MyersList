(*
Ideia 1 : 

Colocar um ponteiro a mais no Wrap. Este novo ponteiro. shortcuts aponta para a maior sub-região direita.

Resultados: 

funciona, os resultados de complexidade de lookup foram mais eficientes que as outras variantes para um n de até 100_000 elementos


*)

type 'a cell = { next : 'a cell; jump : 'a cell; length : int; value : 'a }

type 'a wrap = { last : 'a cell; shortcut : 'a cell list}

type 'a t = 
  | Empty
  | Wrap of {head : 'a cell; last : 'a cell; shortcut : 'a cell }

let empty = Empty

let init' (v : 'a) : 'a cell =
  let rec c : 'a cell = { next = c; jump = c; length = 0; value = v } in c
[@@inline]

let init (v : 'a) : 'a t =
  let c : 'a cell = init' v in 
  Wrap { head = c; last = c; shortcut = c}
[@@inline]

let return (v : 'a) : 'a t = init v

let is_empty wrap = 
  match wrap with
  | Empty -> true
  | Wrap _ -> false

let rec lookup (c : 'a cell) (l : int) : 'a cell =
  if c.length = l then 
    c
  else 
    let j = c.jump in
    let c' = if j.length < l then c.next else j in
    lookup c' l

let rec lookup_cost (c : 'a cell) (l : int) (cost : int) : int =
  if c.length = l then 
    cost
  else 
    let j = c.jump in
    let c' = if j.length < l then c.next else j in
    lookup_cost c' l (cost + 1)

let index (wrap : 'a t) (i : int) : 'a cell = 
  match wrap with 
  | Empty -> invalid_arg "Empty List"
  | Wrap {head = h; last = _; shortcut = s} -> 
    let target = h.length - i in
    if target <= s.length then 
      lookup s target
    else 
      lookup h target
[@@inline]

let cons v wrap =
  match wrap with 
  | Empty -> init v
  | Wrap {head = h; last = l; shortcut = s} -> 
    let h_len = h.length in
    let h_hj = h_len - h.jump.length in
    if h_hj == 1 || h_hj == l.next.length - l.next.jump.length then 
        let c = { next = h; jump = l.next; length = h_len + 1; value = v } in 
        if (h.length - s.length = s.length + 1) then 
          Wrap { head = c; last = l.jump; shortcut = c}
        else
          Wrap {head = c; last = l.jump; shortcut = s} 
    else 
        let c = { next = h; jump = l; length = h_len + 1; value = v } in
        if (h.length - s.length = s.length + 1) then 
          Wrap { head = c; last = c; shortcut = c}
        else
          Wrap {head = c; last = c; shortcut = s} 
[@@inline]

let get_exn wrap i = 
  match wrap with 
  | Empty -> invalid_arg "Empty List"
  | Wrap {head = h; last = _; shortcut = s} -> 
    if i < 0 || i > h.length then 
      invalid_arg "Invalid Index"
    else 
      let target = h.length - i in
      if target <= s.length then 
        (lookup s target).value
      else
        (lookup h target).value 

let get_cost wrap i = 
  match wrap with 
  | Empty -> invalid_arg "Empty List"
  | Wrap {head = h; last = _; shortcut = s} -> 
    if i < 0 || i > h.length then 
      invalid_arg "Invalid Index"
    else 
      let target = h.length - i in
      if target <= s.length then 
        lookup_cost s target 0 
      else
        lookup_cost h target 0 

let get wrap i = try Some (get_exn wrap i) with Invalid_argument _ -> None

let () =
  let lmax = 100_000 in
  let rec run_tests current_list current_n =
    if current_n <= lmax then begin
      
      let rec find_max_cost i max_so_far =
        if i >= current_n then 
          max_so_far
        else
          let cost = get_cost current_list i in
          find_max_cost (i + 1) (max max_so_far cost)
      in
      
      let max_cost = find_max_cost 0 0 in
      Printf.printf "%d %d\n" current_n max_cost;
      
      let next_list = cons current_n current_list in
      run_tests next_list (current_n + 1)
    end
  in
  let initial_list = cons 0 empty in 
  run_tests initial_list 1
