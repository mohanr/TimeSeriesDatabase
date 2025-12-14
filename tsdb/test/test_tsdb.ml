open Bigarray
open Tsdb__Bit_writer.BITWRITER

let%expect_test "Test Set and Get keys"=

        let buf = new_buffer() in
        let bit_array = Array1.create int8_unsigned c_layout 4 in
        Array1.set bit_array 0 1;
        Array1.set bit_array 1 0;
        Array1.set bit_array 2 1;
        Array1.set bit_array 3 0;
        write_bit buf true;
        write_bit buf false;
        write_bit buf true;
        write_bits buf bit_array;

        let buffer = push_to_buffer buf in
        Printf.printf "Length %d\n" (CCVector.length buffer);
        CCVector.iteri ( fun i b -> Printf.printf "[%d] %d" i (Array1.get b i) ) buffer;

        [%expect {|
           Array1.dim bits 4
           bit is false
           bit is false
           bit is false
           bit is false
          Length 1
          [0] 14
          |}]
