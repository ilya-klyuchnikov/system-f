(*
 * Variables
 *)

(* Variables are represented as strings. *)
type t = string

(* Sets of variables. *)
module Set : Core.Set.S with type Elt.t = t

(* Removes any digits from the end of a string. *)
val remove_trailing_digits : t -> t

(* Given a suggested variable name and a set of names to avoid,
 * generates a fresh name. *)
val fresh : t -> Set.t -> t

(* Given a list of suggested variable names and a set of names to avoid,
 * generates a list of fresh names of the same length. *)
val fresh_list : t list -> Set.t -> t list

(* Generates a list of length `n0`, counting from `start` to `limit` and
 * repeating as necessary. *)
val count_cycle : ?start:int -> limit:int -> int -> int list

(* Makes a string list of length `n` that repeats the alphabet as many
 * times as necessary. *)
val make_list : int -> t list

(* Generates the given number of fresh variable names. *)
val fresh_n : int -> Set.t -> t list
