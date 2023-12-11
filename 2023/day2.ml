open! Core

module Ser = struct
  open! Angstrom

  let digit = function '0' .. '9' -> true | _ -> false

  let integer = 
    take_while digit >>| int_of_string

  let color = string "red" <|> string "green" <|> string "blue"

  let draw =
    let color = both integer (char ' ' *> color) in
    sep_by1 (string ", ") color

  let game =
    let* id = string "Game " *> integer <* string ": " in
    let+ draws = sep_by1 (string "; ") draw in
    (id, draws)

  let input =
    sep_by1 end_of_line game
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

let run file =
  let data =
    In_channel.create file |> In_channel.input_all |> parse `Prefix Ser.input
  in
  let maximums = Hashtbl.of_alist_exn (module String) [("red", 12); ("green", 13); ("blue", 14)] in
  let res = List.filter_map data ~f:(fun (gid, draws) ->
    let valid_game =
    List.for_all draws ~f:(fun counts ->
      List.for_all counts ~f:(fun (count, color) ->
        let max = Hashtbl.find_exn maximums color in
        count <= max
      )
    ) in
    if valid_game then Some gid else None
  ) in
  Fmt.pr "%d\n" (List.sum (module Int) ~f:Fun.id res)

let star file =
  let data =
    In_channel.create file |> In_channel.input_all |> parse `Prefix Ser.input
  in
  let powers = List.map data ~f:(fun (_, draws) ->
    let maximums = Hashtbl.of_alist_exn (module String) [("red", 0); ("green", 0); ("blue", 0)] in
    List.iter draws ~f:(fun counts ->
      List.iter counts ~f:(fun (count, color) ->
        let maximum = Hashtbl.find_exn maximums color in
        Hashtbl.set maximums ~key:color ~data:(max maximum count)
      )
    );
    Hashtbl.fold maximums ~init:1 ~f:(fun ~key:_ ~data:count acc -> acc * count)
  ) in
  print_s @@ [%sexp_of: int ] (List.sum (module Int) ~f:Fun.id powers)


let () =
  let open Cmdliner in
  let file_a =
    Arg.(required & pos 0 (some string) None & info ~docv:"FILE" [])
  in
  let star_a = Arg.(value & flag & info ~docv:"STAR" ["s"]) in
  let open Cmd in
  let run file use_star = if not use_star then run file else star file in
  exit (Cmd.eval @@ v (info "aoc") Term.(const run $ file_a $ star_a))
