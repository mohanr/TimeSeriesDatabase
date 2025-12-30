open Types
open Bigarray

module BITREADER = struct

  let new_read_buffer buffer =
    {
            buffer =  buffer;

            byte_position = Array1.create int8_unsigned c_layout 1;

            bit_position =  Array1.create int8_unsigned c_layout 1;
    }
let int_size = Sys.word_size - 1
let int2bin =
  let buf = Bytes.create int_size in
  fun n ->
    for i = 0 to int_size - 1 do
      let pos = int_size - 1 - i in
      Bytes.set buf pos (if n land (1 lsl i) != 0 then '1' else '0')
    done;
    (* skip leading zeros *)
    match Bytes.index_opt buf '1' with
    | None -> "0b0"
    | Some i -> "0b" ^ Bytes.sub_string buf i (int_size - i)


let read_bit buf =

        let b = (Array1.get (CCVector.get buf.buffer 0)) 0 in
        Printf.printf "read_bit %s\n"  (int2bin b);

        let bp = Array1.get (CCVector.get buf.buffer 0) 0 in
        if bp >= CCVector.length buf.buffer then
            None
        else

        let bip = Array1.get buf.bit_position 0 in

        let bit = Int.equal (Int.logand (Int.shift_right bp (Int.sub 7  bip) )  1) 1 in

        Array1.set buf.bit_position 0 (Int.add bip  1);
        if Int.equal( Array1.get buf.bit_position 0)  8 then(
            Array1.set buf.byte_position 0 (bp+1);
            Array1.set buf.bit_position 0 0;
        );

        Some bit

let read_bits buf bits =
        let value = 0L in
        let rec loop_while idx acc =
        if idx <  (Array1.dim bits) - 1 then
            (match read_bit buf with
            |Some _ ->
            let value = Int64.logor (Int64.shift_left acc  1) 1L in
            loop_while (idx+1) value
            | None ->
            loop_while (idx+1) value
            )
        else acc
        in
        loop_while 0 0L
end
