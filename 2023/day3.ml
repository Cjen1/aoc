open! Core

type element = Digit of int | Empty | Part of char

module IntPairs = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

module PairsMap = Map.Make (IntPairs)

module Ser = struct
  open! Angstrom

  let element =
    let+ c = any_char in
    match c with
    | '0' .. '9' as i ->
        Digit (int_of_char i - int_of_char '0')
    | '.' ->
        Empty
    | c ->
        Part c

  let many_till1 elt delim =
    let* data = many_till elt delim in
    if Int.(List.length data = 0) then fail "empty result" else return data

  let input =
    let line = many_till1 element end_of_line in
    many1 line
end

let parse c p s =
  let res =
    match c with
    | `Prefix ->
        Angstrom.parse_string ~consume:Angstrom.Consume.Prefix p s
    | `All ->
        Angstrom.parse_string ~consume:Angstrom.Consume.All p s
  in
  match res with
  | Error s ->
      Fmt.failwith "Failed to parse with: %s" s
  | Ok e ->
      e

type per_part_lookup = {part_number: int; mutable part: bool}
[@@deriving sexp, compare]

let run file =
  let raw_data =
    In_channel.create file |> In_channel.input_all |> parse `Prefix Ser.input
  in
  let data =
    List.foldi raw_data ~init:PairsMap.empty ~f:(fun y map line ->
        List.foldi line ~init:map ~f:(fun x map e ->
            Map.add_exn map ~key:(x, y) ~data:e ) )
  in
  let module Uuid = Unique_id.Int () in
  (* loc -> Uuid.t *)
  let number_lookup = Hashtbl.create (module IntPairs) in
  (* Uuid -> number_data -> *)
  let part_number_lookaside = Hashtbl.create (module Uuid) in
  List.iteri raw_data ~f:(fun y row ->
      List.iteri row ~f:(fun x datum ->
          match datum with
          | Empty | Part _ ->
              ()
          | Digit d ->
              let number_id =
                Hashtbl.find number_lookup (x - 1, y)
                |> Option.value ~default:(Uuid.create ())
              in
              Hashtbl.set number_lookup ~key:(x, y) ~data:number_id ;
              print_s
                ([%sexp_of: (int * int) * Uuid.t * int] ((x, y), number_id, d)) ;
              let prev =
                Hashtbl.find part_number_lookaside number_id
                |> Option.value ~default:{part_number= 0; part= false}
              in
              let prev = {prev with part_number= (prev.part_number * 10) + d} in
              Hashtbl.set part_number_lookaside ~key:number_id ~data:prev ) ) ;
  print_s
    ([%sexp_of: (Uuid.t, per_part_lookup) Hashtbl.t] part_number_lookaside) ;
  let directions =
    let del = [-1; 0; 1] in
    List.cartesian_product del del
  in
  Map.iteri data ~f:(fun ~key:(x, y) ~data:datum ->
      match datum with
      | Part _ ->
          List.iter directions ~f:(fun (dx, dy) ->
              Hashtbl.find number_lookup (x + dx, y + dy)
              |> Option.iter ~f:(fun uuid ->
                     Hashtbl.find part_number_lookaside uuid
                     |> Option.iter ~f:(fun p -> p.part <- true) ) )
      | _ ->
          () ) ;
  let used_numbers =
    Hashtbl.iter part_number_lookaside
    |> Iter.from_labelled_iter
    |> Iter.filter (fun p -> p.part)
    |> Iter.to_list
  in
  let res =
    List.sum (module Int) ~f:(fun {part_number; _} -> part_number) used_numbers
  in
  Fmt.pr "%d\n" res

let star file =
  let raw_data =
    In_channel.create file |> In_channel.input_all |> parse `Prefix Ser.input
  in
  let data =
    List.foldi raw_data ~init:PairsMap.empty ~f:(fun y map line ->
        List.foldi line ~init:map ~f:(fun x map e ->
            Map.add_exn map ~key:(x, y) ~data:e ) )
  in
  let module Uuid = Unique_id.Int () in
  (* loc -> Uuid.t *)
  let number_lookup = Hashtbl.create (module IntPairs) in
  (* Uuid -> number_data -> *)
  let part_number_lookaside = Hashtbl.create (module Uuid) in
  List.iteri raw_data ~f:(fun y row ->
      List.iteri row ~f:(fun x datum ->
          match datum with
          | Empty | Part _ ->
              ()
          | Digit d ->
              let number_id =
                Hashtbl.find number_lookup (x - 1, y)
                |> Option.value ~default:(Uuid.create ())
              in
              Hashtbl.set number_lookup ~key:(x, y) ~data:number_id ;
              print_s
                ([%sexp_of: (int * int) * Uuid.t * int] ((x, y), number_id, d)) ;
              let prev =
                Hashtbl.find part_number_lookaside number_id
                |> Option.value ~default:{part_number= 0; part= false}
              in
              let prev = {prev with part_number= (prev.part_number * 10) + d} in
              Hashtbl.set part_number_lookaside ~key:number_id ~data:prev ) ) ;
  print_s
    ([%sexp_of: (Uuid.t, per_part_lookup) Hashtbl.t] part_number_lookaside) ;
  let directions =
    let del = [-1; 0; 1] in
    List.cartesian_product del del
  in
  let res =
    Map.fold data ~init:0 ~f:(fun ~key:(x, y) ~data:datum sum ->
        match datum with
        | Part '*' ->
            let part_numbers = Hash_set.create (module Uuid) in
            List.iter directions ~f:(fun (dx, dy) ->
                Hashtbl.find number_lookup (x + dx, y + dy)
                |> Option.iter ~f:(fun uuid -> Hash_set.add part_numbers uuid) ) ;
            if Hash_set.length part_numbers = 2 then
              Hash_set.fold part_numbers ~init:1 ~f:(fun v acc ->
                  v * (Hashtbl.find_exn part_number_lookaside acc).part_number )
              + sum
            else sum
        | _ ->
            sum )
  in
  Fmt.pr "%d\n" res

let () =
  let open Cmdliner in
  let file_a =
    Arg.(required & pos 0 (some string) None & info ~docv:"FILE" [])
  in
  let star_a = Arg.(value & flag & info ~docv:"STAR" ["s"]) in
  let open Cmd in
  let run file use_star = if not use_star then run file else star file in
  exit (Cmd.eval @@ v (info "aoc") Term.(const run $ file_a $ star_a))
