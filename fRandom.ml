type t = { mutable s0 : int; mutable s1 : int }

let make ?(seed = 0) () = let s = seed in
  { s0 = if s = 0 then 521288629 else s; s1 = if s = 0 then 362436069 else s }

let int t = let s0 = t.s0 and s1 = t.s1 in
  t.s0 <- (18000 * (s0 land 0xFFFF) + (s0 lsr 16)) land 0xFFFFFFFF;
  t.s1 <- (30903 * (s1 land 0xFFFF) + (s1 lsr 16)) land 0xFFFFFFFF;
  ((t.s0 lsl 16) + (t.s1 land 0xFFFF)) land 0xFFFFFFFF

let float t = float (int t) /. 4294967296.0
