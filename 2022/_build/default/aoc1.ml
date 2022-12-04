module Ser = struct
  open Angstrom
  let integer =
    take_while1 (function '0'..'9'->true | _ -> false) >>| int_of_string

  let line = (integer >>| (fun i -> Some i) <|> return None) <* end_of_line

  let series =
    let rec aux current acc =  
      let line_opt = 
    line >>= function 
      | Some i -> aux (i :: current) acc
      | None -> aux [] (List.rev current :: acc)
      in
      line_opt <|> (end_of_input >>| fun () -> List.rev (current :: acc))
    in aux [] []
end 

let main src =
  let all = open_in src |> In_channel.input_all in
  let data = Angstrom.parse_string ~consume:Angstrom.Consume.All Ser.series all |> Result.get_ok in
  let totals = List.map (List.fold_left (+) 0) data in
  let max = List.fold_left max (-Int.max_int) totals in
  Fmt.pr "max: %d\n" max

let star src =
  let all = open_in src |> In_channel.input_all in
  let data = Angstrom.parse_string ~consume:Angstrom.Consume.All Ser.series all |> Result.get_ok in
  let totals = List.map (List.fold_left (+) 0) data in
  let sorted = List.sort (fun a b -> Int.compare a b |> Int.neg) totals in
  let top_three = (sorted |> List.to_seq |> Seq.take 3 |> List.of_seq) in
  Fmt.pr "top three: %a\n" Fmt.(list ~sep:comma int) top_three;
  Fmt.pr "total: %d" (List.fold_left (+) 0 top_three)

let cmd =
  let open Cmdliner in
  let file_a = Arg.(required & pos 0 (some string) None & info ~docv:"FILE" []) in
  Term.(const main $ file_a)

let cmd_s =
  let open Cmdliner in
  let file_a = Arg.(required & pos 0 (some string) None & info ~docv:"FILE" []) in
  Term.(const star $ file_a)
