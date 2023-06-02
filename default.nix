let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        # ref = "refs/tags/v1.0.1";
        # If changing to a specific `rev` Execute:
        # nix-shell -j auto --cores 0 --run 'rm -rf build/ihp-lib; make -B build/ihp-lib; rm .envrc; make -B .envrc; direnv allow; make -B build/Generated/Types.hs;'
        rev = "99084417ac3a05d27bec0dcf2b911f6464e6571c";
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
            jwt
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
