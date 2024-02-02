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
                    ({ pkgs, ... }: {

                        networking.firewall = {
                           enable = true;
                           allowedTCPPorts = [ 22 80 443 8000 ];
                        };

                        security.acme.defaults.email = "no-reply@tpp-qa.gizra.site";
                        security.acme.acceptTerms = true;
                        services.nginx.virtualHosts."tpp-qa.gizra.site".enableACME = false;
                        services.nginx.virtualHosts."tpp-qa.gizra.site".forceSSL = false;

                        services.ihp = {
                            domain = "tpp-qa.gizra.site";
                            migrations = ./Application/Migration;
                            schema = ./Application/Schema.sql;
                            fixtures = ./Application/Fixtures.sql;
                            sessionSecret = "M$cmzMCEx7xfL-5_q6%9cpve_0BAd5BbDaOtzhv7";
                            additionalEnvVars = {
                                JWT_PRIVATE_KEY_PATH = "/root/jwtRS256.key";
                                JWT_PUBLIC_KEY_PATH = "/root/jwtRS256.key.pub";
                            };
                        };

                        swapDevices = [ { device = "/swapfile"; size = 2000; } ];

                        system.stateVersion = "23.05";

                        systemd.services.app.preStart = ''
                            if [ ! -f /root/jwtRS256.key ]; then
                                ${pkgs.openssh}/bin/ssh-keygen -t rsa -b 4096 -m PEM -f /root/jwtRS256.key;
                            fi
                            if [ ! -f /root/jwtRS256.key.pub ]; then
                                ${pkgs.openssl}/bin/openssl rsa -in /root/jwtRS256.key -pubout -outform PEM -out /root/jwtRS256.key.pub;
                            fi
                        '';
                    })
                ];
            };


        };
}
