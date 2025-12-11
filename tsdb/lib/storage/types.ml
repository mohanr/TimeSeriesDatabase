open Bin_prot.Std
open Bigarray

module  Datapoint_vector = CCVector

type data_point= {
	tstamp  :    float;
	value :   int64;
}
[@@deriving bin_io]

module Compressed_data_vector =  CCVector

type timeseries_block = {

    start_time : float;

    points: data_point Datapoint_vector.vector;

    compressed_data: ((int,int8_unsigned_elt, c_layout) Array1.t) Compressed_data_vector.vector;

    compressed_size: int64;
}

module  Timeseries_block_vector = CCVector

type time_series = {

    key : string;

    mutable active_block: timeseries_block ;

    closed_blocks:timeseries_block Timeseries_block_vector.vector;

}

module  Timeseries_vector = CCVector

module  Timeseriesmap = struct
  type t = string
  let compare s s1 =
     String.compare s s1
end

module  Indice_map = CCMap.Make(Timeseriesmap)

module  Indice_vector = CCVector

type  time_series_map = {

    series_vector: time_series option Timeseries_vector.vector;

	mutable index_map : int64  Indice_map.t;

    free_indices: int64 Indice_vector.vector;
}

module type TSDBOperator = sig
  val time_series : unit -> timeseries_block
end
