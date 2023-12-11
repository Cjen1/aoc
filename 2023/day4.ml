open! Core

type card = {winning: int list; numbers: int list; id: int} [@@deriving sexp]

module Ser = struct
  open! Angstrom

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let spaces = take_while1 (function ' ' -> true | _ -> false)

  let card =
    let* id = string "Card" *> spaces *> integer <* string ":" <* spaces in
    let* winning = sep_by1 spaces integer <* string " |" <* spaces in
    let* numbers = sep_by1 spaces integer in
    return {id; winning; numbers}

  let input = sep_by1 end_of_line card
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

let matches {winning; numbers; _} =
  let ws = Set.of_list (module Int) winning in
  let ns = Set.of_list (module Int) numbers in
  let inter = Set.inter ws ns in
  Set.length inter

let run file =
  let raw_data =
    In_channel.create file |> In_channel.input_all |> parse `Prefix Ser.input
  in
  print_s @@ [%sexp_of: card list] raw_data ;
  let res =
    List.map raw_data ~f:(fun card ->
        let matches = matches card in
        if matches > 0 then Int.pow 2 (matches - 1) else 0 )
  in
  printf "%d\n" (List.sum (module Int) ~f:Fun.id res)

let star file =
  let raw_data =
    In_channel.create file |> In_channel.input_all |> parse `Prefix Ser.input
  in
  let increase extras n inc =
    let rec merge a b =
      match a, b with
      | [], [] -> []
      | [], ls -> ls
      | ls, [] -> ls
      | x::xs, y::ys -> (x + y):: (merge xs ys)
    in
    merge extras (List.init n ~f:(fun _ -> inc))
  in
  let rec aux cards extras sum =
    match cards with
    | [] ->
        sum
    | card :: cs ->
        let multiplier = (List.hd extras |> Option.value ~default:0) + 1 in
        let extras = List.tl extras |> Option.value ~default:[] in
        let matches = matches card in
        let extras = increase extras matches multiplier in
        aux cs extras (sum + multiplier)
  in
  let res = aux raw_data [] 0 in
  printf "%d\n" res

let () =
  let open Cmdliner in
  let file_a =
    Arg.(required & pos 0 (some string) None & info ~docv:"FILE" [])
  in
  let star_a = Arg.(value & flag & info ~docv:"STAR" ["s"]) in
  let open Cmd in
  let run file use_star = if not use_star then run file else star file in
  exit (Cmd.eval @@ v (info "aoc") Term.(const run $ file_a $ star_a))
