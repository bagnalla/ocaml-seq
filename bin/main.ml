open PRNG

(** The main idea being tested here is simple: if we generate two
    lists of samples (say 10 elements each) starting from the same
    initial PRNG seed for both, the lists should should contain the
    same elements. This is obvious because the PRNG is supposed to
    produce a deterministic sequence of samples. *)

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
  print_endline @@ string_of_bool (samples1 = samples2) ^ "\n";

  (* But something strange happens when we repeat this many
     times. Here we take the first 10 samples from the stream 10000
     times and check for when they are not equal to the original: *)
  for i = 1 to 1000000 do
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

(** This seems to happen only with the LXM PRNG, and not Splitmix or
    Chacha. It also appears to be happening every time the GC runs a
    minor collection! Run with [export OCAMLRUNPARAM='v=0x02']
    (although this doesn't seem to work on 5.1.1...). *)
