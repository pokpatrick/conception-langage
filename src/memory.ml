type key =
  | Memory of string
  | Address of string
;;

type memory = (key, float) Hashtbl.t;;

let initial_memory : memory = Hashtbl.create 1;;

let new_memory (memory : memory) (address : key) =
  Hashtbl.add memory address 0.0
;;

let get_memory (memory : memory) (address : key) =
  Hashtbl.find memory address
;;

let set_memory (memory : memory) (address : key) (value : float) =
  Hashtbl.add memory address value
;;
