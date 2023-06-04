# default.nix
let
    ihp = ...;
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = (import ./devenv.nix { pkgs = {}; inputs = {}; config = {}; }).ihp.haskellPackages;
        otherDeps = pkgs: (import ./devenv.nix { inherit pkgs; inputs = {}; config = {}; }).packages;
        projectPath = ./.;
    };
in
    haskellEnv