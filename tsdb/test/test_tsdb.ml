open Bigarray
open Tsdb__Bit_writer.BITWRITER
open Tsdb__Bit_reader.BITREADER

let%expect_test "Test Set and Get keys"=

        let buf = new_write_buffer() in
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
        let read_buffer = new_read_buffer buffer in
        let () =
        (match (read_bit read_buffer) with
        | Some b -> Printf.printf "%b " b
        | None -> Printf.printf "None " ;
        ) in
        let () =
        match (read_bit read_buffer) with
        | Some b -> Printf.printf "%b " b
        | None -> Printf.printf "None " ;
        in
        let () =
        match (read_bit read_buffer) with
        | Some b -> Printf.printf "%b " b
        | None -> Printf.printf "None " ;
        in
        Printf.printf "Bits %Ld\n" (read_bits read_buffer bit_array);
  [%expect {|
    write_bit 0b10000000
    write_bit 0b10100000
    write_bit 0b10100010
    read_bit 0b10100010
    None read_bit 0b10100010
    None read_bit 0b10100010
    None read_bit 0b10100010
    read_bit 0b10100010
    read_bit 0b10100010
    Bits 0
    |}];
  [%expect {| |}]
