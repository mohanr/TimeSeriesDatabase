module type Scan = sig

  type 'a state = Pending | Resolved of 'a | Rejected of exn
  type 'a handler = 'a state -> unit

  type 'a scanner= {
    mutable state : 'a state;
    mutable handlers : 'a handler list
  }
  type 'a resolver = 'a scanner
  val (>>=) : 'a scanner-> ('a -> 'b scanner) -> 'b scanner
  val make : unit -> 'a scanner* 'a resolver

  val return : 'a -> 'a scanner

  val state : 'a scanner-> 'a state

  val resolve : 'a resolver -> 'a -> unit

  val reject : 'a resolver -> exn -> unit
  val (>>=) : 'a scanner-> ('a -> 'b scanner) -> 'b scanner
end

module Scan: Scan= struct
  type 'a state = Pending | Resolved of 'a | Rejected of exn
  type 'a handler = 'a state -> unit
  type 'a scanner= {
    mutable state : 'a state;
    mutable handlers : 'a handler list
  }
type 'a resolver = 'a scanner
  let enqueue
      (handler : 'a state -> unit)
      (scanner: 'a scanner) : unit
    =
    scanner.handlers <- handler :: scanner.handlers

let write_once p s =
    if p.state = Pending
    then p.state <- s
    else invalid_arg "cannot write twice"

  let make () =
    let p = {state = Pending; handlers = []} in
    p, p

  let return x =
    {state = Resolved x; handlers = []}

  let state p = p.state

  let resolve_or_reject (r : 'a resolver) (st : 'a state) =
    assert (st <> Pending);
    let handlers = r.handlers in
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers

  let reject r x =
    resolve_or_reject r (Rejected x)

  let resolve r x =
    resolve_or_reject r (Resolved x)

let handler (resolver : 'a resolver) : 'a handler
    = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Resolved x -> resolve resolver x

  let handler_of_callback
      (callback : 'a -> 'b scanner)
      (resolver : 'b resolver) : 'a handler
    = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Resolved x ->
        let promise = callback x in
        match promise.state with
        | Resolved y -> resolve resolver y
        | Rejected exc -> reject resolver exc
        | Pending -> enqueue (handler resolver) promise

  let (>>=)
      (input_promise : 'a scanner)
      (callback : 'a -> 'b scanner) : 'b scanner
    =
    match input_promise.state with
    | Resolved x -> callback x
    | Rejected exc -> {state = Rejected exc; handlers = []}
    | Pending ->
      let output_promise, output_resolver = make () in
      enqueue (handler_of_callback callback output_resolver) input_promise;
      output_promise
end
