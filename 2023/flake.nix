{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, opam-repository}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        devPackagesQuery = {
          ocaml-lsp-server = "*";
          ocamlformat = "0.25.1";
          utop = "*";
        };
        repos = [
          opam-repository
        ];
        query = devPackagesQuery // {
          ocaml-base-compiler = "5.1.1";
        };
        scope = on.buildDuneProject { inherit repos; } "aoc" ./. query;
        devPackages = builtins.attrValues
          (pkgs.lib.getAttrs (builtins.attrNames query) scope);
      in
      {
        defaultPackage = scope.ocons;
        devShell = pkgs.mkShell {
          inputsFrom = [scope.aoc];
          buildInputs = devPackages ++ [
            pkgs.linuxPackages_latest.perf
          ];
        };
      });
}

