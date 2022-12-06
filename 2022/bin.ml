let () =
  let open Cmdliner in
  let base_info = Cmd.info "advent-of-code" ~doc:"2022 advent of code answers" in
  let open Cmd in
  exit (Cmd.eval @@ group base_info 
    [
      v (info "aoc1") Aoc1.cmd
    ; v (info "aoc1s") Aoc1.cmd_s
    ; v (info "aoc2") Aoc2.cmd
    ; v (info "aoc2s") Aoc2.cmd_s
    ; v (info "aoc3") Aoc3.cmd
    ; v (info "aoc3s") Aoc3.cmds
    ]
  )
