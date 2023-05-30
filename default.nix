let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        # ref = "refs/tags/v1.0.1";
        # If changing to a specific `rev` Execute:
        # nix-shell -j auto --cores 0 --run 'rm -rf build/ihp-lib; make -B build/ihp-lib; rm .envrc; make -B .envrc; direnv allow; make -B build/Generated/Types.hs;'
        rev = "ab0611ee456fc1b9896d51881976663f0ae1292d";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
        ];
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
            imagemagick
            nodejs
        ];
        projectPath = ./.;
    };
in
    haskellEnv
