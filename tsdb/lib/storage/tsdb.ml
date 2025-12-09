open Timedesc
open Types

module TimeSeries = struct

let time_since (time : float) =

  let time_as_float =
  (match (Timedesc.of_timestamp_float_s time) with
    | Some t ->
      let time_as_float = Timedesc.Span.sub (Timedesc.Timestamp.now())  (Timedesc.to_timestamp_single t) in
      (match (Timedesc.Time.of_span time_as_float) with
      | Some t ->
         (Time.hour  t, Time.ns t)
      | None -> failwith "Duration error" )
    | None -> failwith "Duration error" )
  in time_as_float



let time_series() =

        let  start = Float.mul 1000.  (Unix.gettimeofday()) in

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
let  insert time_s timestamp value =
      let dp = { tstamp = timestamp; value = value } in
      match (time_since timestamp) with
      | (h, ns) -> if h > 2 || (h = 2 && ns > 0 ) then(
                     let () = CCVector.push time_s.closed_blocks time_s.active_block in
                     time_s.active_block <- time_series();
                     CCVector.push time_s.active_block.points dp
                     )
                     else(
                     CCVector.push time_s.active_block.points dp
                     )

end

module TSDBOp = TimeSeries
