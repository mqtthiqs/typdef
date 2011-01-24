include types of mli with 
  type t = int and 
  type w = bool and 
  type N.l = string and
  module M = struct 
    type t = C of int
    let x = C 0
  end

let x = A 42
let y = B true
let z = M.C 33
let w = N.P.W "coucou"
