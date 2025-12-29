open Types
open Bigarray

module BITWRITER = struct

  let new_write_buffer() =
    {
            buffer =  Buffer_vector.create();

            current_byte = Array1.create int8_unsigned c_layout 1;

            bit_position =  Array1.create int8_unsigned c_layout 1;
    }

  (* This is from OCaml forum *)
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

let write_bit (buf : bit_writer) bit =
        if bit then(
            let bp = Array1.get buf.bit_position 0 in
            let cb = Array1.get buf.current_byte 0 in
            let () = Array1.set buf.current_byte 0 (Int.logor cb (Int.shift_left 1 (Int.sub 7  bp) )  ) in
            Printf.printf "write_bit %s\n"  (int2bin (Array1.get buf.current_byte 0));
          );
         let bp = Array1.get buf.bit_position 0 in
         Array1.set buf.bit_position 0 (Int.add bp  1);

        if Int.equal( Array1.get buf.bit_position 0)  8 then(
            CCVector.push buf.buffer buf.current_byte;
            Array1.set buf.current_byte 0 0;
            Array1.set buf.bit_position 0 0;
        ) else ()

let  write_bits buf bits =
        for i = (Array1.dim bits) - 1 downto 0 do
            let bit = (Int.equal(Int.logand (Int.shift_right (Array1.get bits i)  i)  1) 1 ) in
            (* let () = Printf.printf " i is %d bit is %b %d\n" i bit  (Array1.get bits i) in *)

            write_bit buf bit
        done

let push_to_buffer (buf : bit_writer) =
        let bp = Array1.get buf.bit_position 0 in
        if bp > 0 then
            CCVector.push buf.buffer buf.current_byte;
        buf

let bit_count buf =
        let bp = Array1.get buf.bit_position 0 in
        CCVector.length buf.buffer * 8 + bp

end
