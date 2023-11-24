{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ self, nixpkgs, ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                        imagemagick
                        nodejs
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint
                        jwt
                    ];
                };
            };

            # Adding the new NixOS configuration for "ihp-app"
            flake.nixosConfigurations."ihp-app" = nixpkgs.lib.nixosSystem {
                system = "x86_64-linux";
                specialArgs = inputs;
                modules = [
                    "${nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
                    ihp.nixosModules.appWithPostgres
                    ({ ... }: {
                        security.acme.defaults.email = "no-reply@tpp-qa.gizra.site";

                        services.ihp = {
                            domain = "tpp-qa.gizra.site";
                            migrations = ./Application/Migration;
                            schema = ./Application/Schema.sql;
                            fixtures = ./Application/Fixtures.sql;
                            sessionSecret = "M$cmzMCEx7xfL-5_q6%9cpve_0BAd5BbDaOtzhv7";
                        };

                        swapDevices = [ { device = "/swapfile"; size = 8192; } ];

                        system.stateVersion = "23.05";
                    })
                ];
            };


        };
}
