open! Core

module Ser = struct
  open! Angstrom

  let some t = t >>| fun x -> Some x

  let none t = t >>| fun _ -> None

  let line =
    let rec aux acc =
      let* c = none end_of_line <|> some any_char in
      match c with None -> return (acc |> List.rev) | Some c -> aux (c :: acc)
    in
    aux []

  let input =
    let rec aux acc =
      let* w = some line <|> none end_of_input in
      match w with None -> return @@ List.rev acc | Some l -> aux (l :: acc)
    in
    aux []

  let digit =
    let digit =
      any_char
      >>= function
      | '0' .. '9' as i ->
          return (int_of_char i - int_of_char '0')
      | _ ->
          fail "not digit"
    in
    let others =
      (string "zero" >>| (fun _ -> 0))
      <|> (string "one" >>| (fun _ -> 1))
      <|> (string "two" >>| fun _ -> 2)
      <|> (string "three" >>| fun _ -> 3)
      <|> (string "four" >>| fun _ -> 4)
      <|> (string "five" >>| fun _ -> 5)
      <|> (string "six" >>| fun _ -> 6)
      <|> (string "seven" >>| fun _ -> 7)
      <|> (string "eight" >>| fun _ -> 8)
      <|> (string "nine" >>| fun _ -> 9)
    in
    some (digit <|> others) <|> none any_char

  let line_s =
    let rec aux acc =
      let* c = none end_of_line <|> some digit in
      match c with None -> return (acc |> List.rev) | Some c -> aux (c :: acc)
    in
    aux []

  let input_s =
    let rec aux acc =
      let* w = some line_s <|> none end_of_input in
      match w with None -> return @@ List.rev acc | Some l -> aux (l :: acc)
    in
    aux []
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
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let f chars =
    let digits = List.filter ~f:is_digit chars in
    let fst = List.hd_exn digits in
    let lst = List.last_exn digits in
    let conv c = int_of_char c - int_of_char '0' in
    (conv fst * 10) + conv lst
  in
  let r = data |> List.map ~f |> List.sum (module Int) ~f:Fun.id in
  Fmt.pr "%d\n" r

let star file =
  let data =
    In_channel.create file |> In_channel.input_all |> parse `All Ser.input_s
  in
  let f ints =
    let digits = List.filter_opt ints in
    let fst = List.hd_exn digits in
    let lst = List.last_exn digits in
    (fst * 10) + lst
  in
  let r = data |> List.map ~f in
  print_s ([%sexp_of: int list] (List.take r 100));
  let r = r |> List.sum (module Int) ~f:Fun.id in
  Fmt.pr "%d\n" r

let () =
  let open Cmdliner in
  let file_a =
    Arg.(required & pos 0 (some string) None & info ~docv:"FILE" [])
  in
  let star_a = Arg.(value & flag & info ~docv:"STAR" ["s"]) in
  let open Cmd in
  let run file use_star = if not use_star then run file else star file in
  exit (Cmd.eval @@ v (info "aoc") Term.(const run $ file_a $ star_a))
