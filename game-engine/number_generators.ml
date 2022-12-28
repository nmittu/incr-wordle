open Core

module type NumberGenerator = sig
  val gen : int -> int
  val gen2 : int -> int * int
end

module RandomGenerator = struct
  let gen = Random.int
  let gen2 range = Random.int range, Random.int range
end

module DeterministicGenerator () = struct
  let x = ref 0
  let init x' = x := x'

  let hash x =
    let x = Int.shift_right x 16 lxor x * 0x45d9f3b in
    let x = Int.shift_right x 16 lxor x * 0x45d9f3b in
    let x = Int.shift_right x 16 lxor x in
    x
  ;;

  let gen range = hash !x mod range

  let gen2 range =
    let h = hash !x in
    let x = h land 0xffff
    and y = Int.shift_right h 16 in
    x mod range, y mod range
  ;;
end
