open Types
open Bigarray

module BITREADER = struct

  let new_buffer() =
    {
            buffer =  Buffer_vector.create();

            current_byte = Array1.create int8_unsigned c_layout 1;

            bit_position =  Array1.create int8_unsigned c_layout 1;
    }

let read_bit buf =
        let bp = Array1.get buf.bit_position 0 in
        let ()= Printf.printf "bp %d len %d\n" bp (CCVector.length buf.buffer) in
        if bp >= CCVector.length buf.buffer then
            None
        else

        let bp = Array1.get buf.bit_position 0 in
        let cb = Array1.get buf.current_byte 0 in
        let byte = CCVector.get buf.buffer bp in

        let bit = Int.equal (Int.logand cb (Int.shift_right (Int.sub 7  bp) 1))  1 in

        Array1.set buf.bit_position 0 (Int.add bp  1);
        if Int.equal( Array1.get buf.bit_position 0)  8 then(
            Array1.set buf.current_byte 0 0;
            let cb = Array1.get buf.current_byte 0 in
            Array1.set buf.bit_position 0 (cb+1);
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
