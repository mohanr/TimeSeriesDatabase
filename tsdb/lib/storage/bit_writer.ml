open Types
open Bigarray

module BITWRITER = struct

  let new_write_buffer() =
    {
            buffer =  Buffer_vector.create();

            current_byte = Array1.create int8_unsigned c_layout 1;

            bit_position =  Array1.create int8_unsigned c_layout 1;
    }

  (* This is fro0 OCaml forum *)
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
    (* | Some i -> "0b" ^ Bytes.sub_string buf i (int_size - i) *)

    | Some _i -> "0b" ^ Bytes.to_string buf

let write_bit (buf : bit_writer) bit =
        let bp = Array1.get buf.bit_position 0 in
        if bit then(
            let cb = Array1.get buf.current_byte 0 in
            let () = Array1.set buf.current_byte 0 (Int.logor cb (Int.shift_left 1 (Int.sub 7  bp) )  ) in
            Printf.printf "write_bit %d %s\n"  (Array1.get buf.current_byte 0) (int2bin (Int.logor cb (Int.shift_left 1 (Int.sub 7  bp) )   ));
          );
         Array1.set buf.bit_position 0 (Int.add bp  1);

        if Int.equal( Array1.get buf.bit_position 0)  8 then(
            CCVector.push buf.buffer buf.current_byte;
            Array1.set buf.current_byte 0 0;
            Array1.set buf.bit_position 0 0;
        ) else ()

let  write_bits buf bitfield len =
        let bits = bitfield.{0} in
         (* Printf.printf " bit is  %s\n"  (int2bin (Int32.to_int bits)); *)

        for i = len - 1 downto 0 do
            let bit = (Int32.equal(Int32.logand (Int32.shift_right bits  i)  1l) 1l ) in
            write_bit buf bit
        done

let push_to_buffer (buf : bit_writer) =
        let bp = Array1.get buf.bit_position 0 in
        if bp > 0 then
            CCVector.push buf.buffer buf.current_byte;
        buf.buffer

let bit_count buf =
        let bp = Array1.get buf.bit_position 0 in
        CCVector.length buf.buffer * 8 + bp

end
