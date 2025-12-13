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
            Array1.set buf.current_byte 0 (Int.shift_left (Int.logor cb  1)
                                            (Int.sub 7  bp))
        );
         let bp = Array1.get buf.bit_position 0 in
         Array1.set buf.bit_position 0 (Int.add bp  1);

        if Array1.get buf.bit_position 0 == 8 then(
            CCVector.push buf.buffer buf.current_byte;
            Array1.set buf.current_byte 0 0;
            Array1.set buf.bit_position 0 0;
        );

end
