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


let overlaps start (en_d:float)=
  (match (Timedesc.of_timestamp_float_s (Float.add start  7200.)),
         (Timedesc.of_timestamp_float_s start) ,
         (Timedesc.of_timestamp_float_s en_d) with
    | Some t, Some t1,Some  t2 ->
         (Timedesc.Timestamp.lt (Timedesc.to_timestamp_single t2) (Timedesc.to_timestamp_single t))
          || (Timedesc.Timestamp.gt (Timedesc.to_timestamp_single t)
                                    (Timedesc.to_timestamp_single t1))
    | _,_,  _ ->  failwith "Duration error" )

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

let get_points timeseries start e_nd =

   let overlaps tstamp  =
    (match (Timedesc.of_timestamp_float_s tstamp),
           (Timedesc.of_timestamp_float_s start),
           (Timedesc.of_timestamp_float_s e_nd) with
    | Some t, Some t1,Some  t2 ->
        (Timedesc.Timestamp.ge (Timedesc.to_timestamp_single t) (Timedesc.to_timestamp_single t1))
        &&
        (Timedesc.Timestamp.le (Timedesc.to_timestamp_single t) (Timedesc.to_timestamp_single t2))
    | _,_, _ ->  failwith "Duration error" )
    in
    CCVector.filter ( fun dp -> overlaps dp.tstamp ) timeseries

let  insert time_s timestamp value =
      let dp = { tstamp = timestamp; value = value } in
      match (time_since timestamp) with
      (* I should use the time API to check this. *)
      | (h, ns) -> if h > 2 || (h = 2 && ns > 0 ) then( (*TODO  I check nanoseconds only!*)
                     let () = CCVector.push time_s.closed_blocks time_s.active_block in time_s.active_block <- time_series();
                         CCVector.push time_s.active_block.points dp
                     )
                     else(
                         CCVector.push time_s.active_block.points dp
                     )


let query start en_d blocks =
        let  results = CCVector.create() in

        CCVector.iter(fun block ->
            if overlaps block.start_time en_d then
                CCVector.push results (get_points block.points start en_d)
            else ()
        )blocks.closed_blocks;
        if overlaps blocks.active_block.start_time  en_d then
            CCVector.push results (get_points blocks.active_block.points
                            start en_d);

        results

end

module TSDBOp = TimeSeries
