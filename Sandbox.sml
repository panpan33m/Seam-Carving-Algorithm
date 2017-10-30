(* Write anything you want in the structure below. To use it at the REPL, do
 * - CM.make "sandbox.cm"; open Sandbox; *)
structure Sandbox =
struct
  structure Seq = ArraySequence

  (* from MkSeamFind.sml *)
  structure SeamFind : SEAMFIND = MkSeamFind(structure Seq = Seq)

end
