module Ser = struct
  open Angstrom

  let some t = t >>| fun x -> Some x
  let none t = t >>| fun _ -> None

  let line = 
    let rec aux acc = 
      let* c = (none end_of_line) <|> (some any_char) in
      match c with
      | None -> return (acc |> List.rev |> Array.of_list)
      | Some c -> aux (c :: acc)
    in aux []

  let input =
    let rec aux acc =
      let* w = (some line <|> none end_of_input) in
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

module CharSet = Set.Make(struct type t = char;; let compare = Char.compare end)

let priority = function
  | 'a' .. 'z' as c-> 
      int_of_char c - int_of_char 'a' + 1
  | 'A' .. 'Z' as c-> 
      int_of_char c - int_of_char 'A' + 27
  | _ -> assert false

let main src = 
  let data = open_in src |> In_channel.input_all |> parse `Prefix Ser.input in
  let halves = data |> List.map (fun arr -> Array.sub arr 0 (Array.length arr / 2), Array.sub arr (Array.length arr / 2) (Array.length arr / 2)) in
  let sets = halves |> List.map ( fun (l,r) -> l |> Array.to_list |> CharSet.of_list, r |> Array.to_list |> CharSet.of_list) in
  let sim = sets |> List.map (fun (l,r) -> CharSet.inter l r |> CharSet.elements) in
  let mistakes = List.concat sim in
  let priorities = mistakes |> List.map priority in
  let res = priorities |> List.fold_left (+) 0 in
  Fmt.pr "res: %d\n" res

let star src = 
  let data = open_in src |> In_channel.input_all |> parse `Prefix Ser.input in
  let contents = data |> List.map (fun arr -> arr |> Array.to_list |> CharSet.of_list) in
  let rec badges ls acc =
    match ls with
    | a::b::c::ls -> badges ls ((CharSet.inter a b |> CharSet.inter c) :: acc)
    | [] -> acc |> List.rev |> List.map CharSet.elements
    | _ -> assert false
  in
  let badges = badges contents [] in
  let res = badges |> List.concat |> List.map priority |> List.fold_left (+) 0 in
  Fmt.pr "res: %d\n" res

let cmd =
  let open Cmdliner in
  let file_a = Arg.(required & pos 0 (some string) None & info ~docv:"FILE" []) in
  Term.(const main $ file_a)

let cmds =
  let open Cmdliner in
  let file_a = Arg.(required & pos 0 (some string) None & info ~docv:"FILE" []) in
  Term.(const star $ file_a)

