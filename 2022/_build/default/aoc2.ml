module Ser = struct
  type rps = Rock | Paper | Scissor

  let rps_pp ppf v = match v with
  | Rock ->    Fmt.pf ppf "Rock"
  | Paper ->   Fmt.pf ppf "Paper"
  | Scissor -> Fmt.pf ppf "Scissor"

  type result = Win | Draw | Loss

  let res_pp ppf v = match v with
  | Win ->    Fmt.pf ppf "Win"
  | Draw ->   Fmt.pf ppf "Draw"
  | Loss -> Fmt.pf ppf "Loss"

  open Angstrom

  let some t = t >>| fun x -> Some x
  let none t = t >>| fun _ -> None

  let rps = any_char >>= function
    | 'A' | 'X' -> return Rock
    | 'B' | 'Y' -> return Paper
    | 'C' | 'Z' -> return Scissor
    | _ -> Angstrom.fail "not valid option"
    
  let result = any_char >>= function
    | 'X' -> return Loss
    | 'Y' -> return Draw
    | 'Z' -> return Win
    | _ -> Angstrom.fail "not valid option"

  let input =
    let open Angstrom.Let_syntax in
    let line =
      let* them = rps in
      let* _ = char ' ' in
      let* ours = rps in
      return (them, ours)
    in 
    let rec aux acc =
      let* w = (some (line <* end_of_line) <|> none end_of_input) in
      match w with
      | None -> return @@ List.rev acc
      | Some(l) -> aux (l :: acc)
    in aux []
    
  let input_star =
    let open Angstrom.Let_syntax in
    let line =
      let* them = rps in
      let* _ = char ' ' in
      let* ours = result in
      return (them, ours)
    in 
    let rec aux acc =
      let* w = (some (line <* end_of_line) <|> none end_of_input) in
      match w with
      | None -> return @@ List.rev acc
      | Some(l) -> aux (l :: acc)
    in aux []
    
end 

let parse c p s = 
  let res = match c with
| `Prefix -> Angstrom.parse_string ~consume:Angstrom.Consume.Prefix p s
| `All -> Angstrom.parse_string ~consume:Angstrom.Consume.All p s
  in
  match res with
  | Error s -> Fmt.failwith "Failed to parse with: %s" s
  | Ok e -> e

let shape_score = 
  let open Ser in
  function
  | Rock -> 1
  | Paper -> 2
  | Scissor -> 3

let round_score a b =
  let open Ser in
  match (a,b) with
  | Rock, Paper 
  | Paper, Scissor 
  | Scissor, Rock -> 6
  | Rock, Rock
  | Paper, Paper
  | Scissor, Scissor -> 3
  | Paper, Rock
  | Scissor, Paper
  | Rock, Scissor -> 0

let main src = 
  let all = open_in src |> In_channel.input_all in
  let data = parse `Prefix Ser.input all in
  (*Fmt.pr "input: %a\n" Fmt.(list ~sep:comma @@ braces @@ pair ~sep:comma Ser.rps_pp Ser.rps_pp) data*)
  let results = data |> List.map (fun (t,u) -> (shape_score u) + (round_score t u)) in
  let total = List.fold_left (+) 0 results in
  Fmt.pr "total: %d" total

let cmd =
  let open Cmdliner in
  let file_a = Arg.(required & pos 0 (some string) None & info ~docv:"FILE" []) in
  Term.(const main $ file_a)

let how_to target them =
  let open Ser in
  match target, them with
  | Win, Rock -> Paper
  | Win, Paper -> Scissor
  | Win, Scissor -> Rock
  | Draw, Rock -> Rock
  | Draw, Paper -> Paper
  | Draw, Scissor -> Scissor
  | Loss, Rock -> Scissor
  | Loss, Paper -> Rock
  | Loss, Scissor -> Paper

let star src =
  let all = open_in src |> In_channel.input_all in
  let data = parse `Prefix Ser.input_star all in
  let rounds = data |> List.map (fun (them, aim) -> them, how_to aim them) in
  let results = rounds |> List.map (fun (t,u) -> (shape_score u) + (round_score t u)) in
  let total = List.fold_left (+) 0 results in
  Fmt.pr "total: %d\n" total

let cmd_s =
  let open Cmdliner in
  let file_a = Arg.(required & pos 0 (some string) None & info ~docv:"FILE" []) in
  Term.(const star $ file_a)
