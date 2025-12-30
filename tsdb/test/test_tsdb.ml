open Bigarray
open Tsdb__Bit_writer.BITWRITER
open Tsdb__Bit_reader.BITREADER

let%expect_test "Test Set and Get keys"=

        let buf = new_write_buffer() in
        let bitfield = Array1.create Int32 c_layout 1 in
        bitfield.{0} <- Int32.of_int 0b1010;
        write_bit buf true;
        write_bit buf false;
        write_bit buf true;
        write_bits buf bitfield 4;

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
        Printf.printf "Bits %s\n" (int2bin (Int64.to_int (read_bits read_buffer 6)));
  [%expect {|
    write_bit 128 0b000000000000000000000000000000000000000000000000000000010000000
    write_bit 160 0b000000000000000000000000000000000000000000000000000000010100000
    write_bit 176 0b000000000000000000000000000000000000000000000000000000010110000
    write_bit 180 0b000000000000000000000000000000000000000000000000000000010110100
    read_bit 0b10110100
    true read_bit 0b10110100
    false read_bit 0b10110100
    true read_bit 0b10110100
    read_bit 0b10110100
    read_bit 0b10110100
    read_bit 0b10110100
    read_bit 0b10110100
    Bits 0b10100
    |}];
