open Bigarray
open Tsdb__Bit_writer.BITWRITER
open Tsdb__Bit_reader.BITREADER

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

        (* CCVector.iteri ( fun i b -> Printf.printf "[%d] %d" i (Array1.get b i) ) buffer; *)
        match (read_bit buffer) with
        | Some b -> Printf.printf "%b" b
        | None -> Printf.printf "None " ;

        match (read_bit buffer) with
        | Some b -> Printf.printf "%b" b
        | None -> Printf.printf "None " ;

        match (read_bit buf) with
        | Some b -> Printf.printf "%b" b
        | None -> Printf.printf "None " ;

        Printf.printf "Bits %Ld\n" (read_bits buffer bit_array);
        [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Invalid_argument "index out of bounds")
  Raised by primitive operation at Test_tsdb.(fun) in file "tsdb/test/test_tsdb.ml", line 13, characters 8-32
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
