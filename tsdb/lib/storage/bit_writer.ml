open Types
open Bigarray

module BITWRITER = struct

  let new_buffer() =
    {
            buffer =  Buffer_vector.create();

            current_byte = Array1.create int8_unsigned c_layout 1;

            bit_position =  Array1.create int8_unsigned c_layout 1;
    }



let write_bit buf bit =
        if bit then(
            let bp = Array1.get buf.bit_position 0 in
            let cb = Array1.get buf.current_byte 0 in
            Array1.set buf.current_byte 0 (Int.logor cb (Int.shift_left (Int.sub 7  bp) 1)  )
        );
         let bp = Array1.get buf.bit_position 0 in
         Array1.set buf.bit_position 0 (Int.add bp  1);

        if Int.equal( Array1.get buf.bit_position 0)  8 then(
            CCVector.push buf.buffer buf.current_byte;
            Array1.set buf.current_byte 0 0;
            Array1.set buf.bit_position 0 0;
        ) else ()

let  write_bits buf bits =
        Printf.printf " Array1.dim bits %d\n" ( Array1.dim bits );
        for i = (Array1.dim bits) - 1 downto 0 do
            let bit = (Int.equal(Int.logand (Int.shift_right_logical (Array1.get bits i)  1)  1) 1 ) in
            let () = Printf.printf " bit is %b\n" bit in
            write_bit buf bit
        done

let push_to_buffer buf =
        let bp = Array1.get buf.bit_position 0 in
        if bp > 0 then
            CCVector.push buf.buffer buf.current_byte;
        buf.buffer

let bit_count buf =
        let bp = Array1.get buf.bit_position 0 in
        CCVector.length buf.buffer * 8 + bp

end
