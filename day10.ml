
type sink =
    Bot of int
  | Output of int
type switch = {value:int option*int option; low:sink option; high:sink option}
type inst =
    GoesTo of {value:int; sink:sink}
  | Gives of {input:int; low:sink; high:sink}

let convert_sink s v =
  match s with
  | "bot" -> Bot v
  | "output" -> Output v
  | _ -> invalid_arg s

let string_of_sink sink =
  match sink with
  | Some (Bot b) -> "bot " ^ (string_of_int b)
  | Some (Output o) -> "out " ^ (string_of_int o)
  | None -> "(null)"

let string_of_value bot =
  match bot with
  | Some b -> string_of_int b
  | None -> "(null)"

let read_input () : inst list =
  let rec f acc =
    try
      let line = input_line stdin in
      let inst =
        if line.[0] = 'v' then
          Scanf.sscanf line "value %d goes to bot %d"
            (fun value bot -> GoesTo {value; sink=Bot bot})
        else
          Scanf.sscanf line "bot %d gives low to %s %d and high to %s %d"
            (fun bot low_type low high_type high ->
               let low = convert_sink low_type low in
               let high = convert_sink high_type high in
               Gives {input=bot; low; high})
      in
      f (inst :: acc)
    with End_of_file -> acc
  in f []

module Sink =
struct
  type t = sink
  let compare a b =
    match a, b with
    | Output x, Output y -> Pervasives.compare x y
    | Bot x, Bot y -> Pervasives.compare x y
    | Bot _, Output _ -> -1
    | Output _, Bot _ -> 1
end

module SinkMap = Map.Make(Sink)

let find key state =
  try Some (SinkMap.find key state)
  with Not_found -> None

let upsert key value state =
  match find key state with
  | Some x when x = value -> (false, state)
  | _ -> (true, SinkMap.add key value state)

let merge ((l,r):int option*int option) (a:int) =
  match l,r with
  | Some x,None when x=a -> (l,r)
  | Some x,None -> if a < x then (Some a,l) else (l,Some a)
  | None,None -> (Some a,None)
  | Some x,Some y -> invalid_arg "should have been caught earlier; no room"
  | None,_ -> invalid_arg "right without left"

let rec process (changedp, state) instruction =
  let propagate {value;low;high} state =
    match value with
    | Some x, Some y ->
      let state = (match low with
          | Some sink ->
            process (changedp, state) (GoesTo {value=x; sink})
          | _ -> (changedp, state))
      in
      let state = (match high with
          | Some sink ->
            process state (GoesTo {value=y; sink})
          | _ -> state)
      in
      state
    | _ ->
      (true, state)
  in
  match instruction with
  | GoesTo {value=value; sink} ->
    let old = find sink state in
    (match old with
     | Some {value=(l,r)} when l=Some value || r=Some value -> (false || changedp, state)
     | Some old' ->
       (match old' with {value=(l,r); low; high} ->
          let value' = merge (l,r) value in
          let sw = {old' with value=value'} in
          let state = SinkMap.add sink sw state in
          propagate sw state)
     | None -> (true, SinkMap.add sink {value=(Some value,None); low=None; high=None} state))
  | Gives {input=bot; low; high} ->
    let key = Bot bot in
    let old = find key state in
    (match old with
     | Some {low=low'; high=high'} when Some low=low' && Some high=high' -> (false || changedp, state)
     | Some {value} ->
       begin
         let sw = {value; low=Some low; high=Some high} in
         propagate sw (SinkMap.add key sw state)
       end
     | None -> (true, SinkMap.add key {value=(None,None); low=Some low; high=Some high} state))

let interp_until_unchanged instrs =
  let rec loop (changedp, state) =
    if changedp then
      loop (List.fold_left process
              (false, state)
              instrs)
    else
      state
  in
  loop (true, SinkMap.empty)

let print_state state =
  SinkMap.iter (fun k {value=(l,r);low;high} ->
      Printf.printf "%s: %s %s <%s >%s\n"
        (string_of_sink (Some k))
        (string_of_value l)
        (string_of_value r)
        (string_of_sink low)
        (string_of_sink high))
    state

let multiply_outs state =
  Printf.printf "%d\n"
    (SinkMap.fold
       (fun k {value=(Some low,_)} acc ->
         match k with
         | Output n when n < 3 -> acc * low
         | _ -> acc)
        state 1)

let () =
  read_input ()
  |> interp_until_unchanged
  |> if Array.mem "--initial" Sys.argv then print_state else multiply_outs
