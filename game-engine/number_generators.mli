open! Core

module type NumberGenerator = sig
  val gen:  int -> int
  val gen2: int -> int*int
end

module RandomGenerator: NumberGenerator

module DeterministicGenerator (): sig
  include NumberGenerator
  val init: int -> unit
end