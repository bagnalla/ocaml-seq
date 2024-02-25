open PRNG

(** I'm encountering some strange and inconsistent behavior with lazy
    sequences of pseudo-random numbers. The PRNG being used is the
    implementation of LXM from the PRINGO library.

    The idea is simple: if we generate two lists of samples (say 10
    elements each) starting from the same initial PRNG seed for both,
    the lists should should contain the same elements. This is obvious
    because the PRNG produces a deterministic sequence of samples.

    We first check that this is true for the PRNG in isolation:
*)

let () =
  (* Seed LXM PRNG. *)
  let rng = LXM.Pure.seed "sufficiently long seed string" in

  (* Generate a list of [n] Boolean samples using [rng]. *)
  let gen (n : int) (rng : LXM.Pure.t) : bool list =
    let r = ref rng in
    let l = ref [] in
    for _ = n to n do
      let x, r' = LXM.Pure.bit !r in
      r := r';
      l := x :: !l
    done;
    !l
  in

  (* Generate 100000 lists of 10 samples, all with the same initial seed. *)
  let samples = List.init 100000 (fun _ -> gen 10 rng) in

  (* Check that all of the sampled lists are equal, as they should be
     since they were generated via a deterministic PRNG from the same
     initial seed. *)
  print_endline @@ string_of_bool @@
    List.fold_left (fun b l -> b && l = List.nth samples 0) true samples;
  print_endline ""


(** So far, so good. Now, instead of using the PRNG directly, we will
    build lazy sequences of random bits. *)

(** Build a lazy bit sequence from a PRNG. *)
let bits (rng : LXM.Pure.t) : bool Seq.t =
  Seq.unfold (fun r -> Some (LXM.Pure.bit r)) rng

(** Build a list from the first [n] elements of sequence [s]. *)
let rec prefix (n : int) (s : 'a Seq.t) : 'a list =
  if n <= 0 then [] else
    match Seq.uncons s with
    | None -> []
    | Some (y, s') -> y :: prefix (n-1) s'

let () =
  (* Seed LXM PRNG. *)
  let rng = LXM.Pure.seed "sufficiently long seed string" in

  (* Build lazy sequence of random bits (not evaluated yet). *)
  let bs = bits rng in

  (* Put the first 10 samples in a list and print it out. *)
  let samples1 = prefix 10 bs in
  print_string "bits: ";
  List.iter (fun b -> print_string @@ if b then "1" else "0") samples1;
  print_endline "\n";

  (* Get the first 10 samples again from the same stream. The result
     is the exact same list of samples, as expected: *)
  let samples2 = prefix 10 bs in
  print_endline @@ string_of_bool (samples1 = samples2);
  print_endline "";

  (* But something strange happens when we repeat this many
     times. Here we take the first 10 samples from the stream 10000
     times and check for when they are not equal to the original: *)
  for i = 1 to 10000000 do
    let samples2 = prefix 10 bs in
    if samples1 <> samples2 then begin

        (* This happens sometimes! *)
        print_endline @@ string_of_int i;
        List.iter (fun b -> print_string @@ if b then "1" else "0") samples2;
        print_endline "";

        (* But strangely, whenever it does, we can get the samples
           again and see the expected values... *)
        let samples3 = prefix 10 bs in
        List.iter (fun b -> print_string @@ if b then "1" else "0") samples3;
        print_endline "\n"
      end
  done

(** What is going on here? I've verified that it happens with OCaml
    versions 4.14.1 and 5.1.1. I've also tried defining my own type of
    lazy streams and it still happens, so I don't think the problem is
    with the Seq module.

    EDIT: it seems to happen only with the LXM PRNG, and not Splitmix
    or Chacha.
*)
