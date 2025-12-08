open Timedesc
open Types

module TimeSeries = struct

let time_since time () =

  let time_as_float =
      Timedesc.Span.sub (Timedesc.Timestamp.now()) ( Timedesc.to_timestamp_single time)
  in  Time.hour (match (Timedesc.Time.of_span time_as_float) with | Some t -> t | None -> failwith "Duration error" )

let time_series() =

        let  start = Int64.mul (Int64.of_int 1000)  (Int64.of_float (Unix.gettimeofday())) in

        {

            start_time = start;

            points = Datapoint_vector.create();

            compressed_data = Compressed_data_vector.create();

            compressed_size  = 0L;
          }

let new_time_series k =

        {
            key = k;
            active_block  = time_series();
            closed_blocks = Timeseries_block_vector.create();
         }

end

module TSDBOp = TimeSeries
