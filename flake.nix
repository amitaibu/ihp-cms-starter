{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, ... }:
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

                        (nodePackages.tailwindcss.overrideAttrs
                            (_: {
                                plugins = [
                                    nodePackages."@tailwindcss/aspect-ratio"
                                    nodePackages."@tailwindcss/forms"
                                    nodePackages."@tailwindcss/language-server"
                                    nodePackages."@tailwindcss/line-clamp"
                                    nodePackages."@tailwindcss/typography"
                                ];
                            })
                        )
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
                        mmark
                    ];
                };

                devenv.shells.default = {
                    # Custom processes that don't appear in https://devenv.sh/reference/options/
                    processes = {
                        tailwind.exec = "tailwindcss -c tailwind/tailwind.config.js -i ./tailwind/app.css -o static/app.css --watch=always";

                        # @todo: Move to a better place?
                        initDevEnv.exec = ''
                            UUID="84fa7d47-002d-4fe5-a86a-df51cc4ec10f"
                            SOURCE_FILE="/static/styleGuideImages/$UUID"
                            TARGET_DIR="/paragraph_quotes/imageUrl"
                            TARGET_FILE="$TARGET_DIR/$UUID"

                            # Create the target directory if it doesn't exist
                            mkdir -p "$TARGET_DIR"

                            # Check if the file exists, if not, copy it
                            if [ ! -f "$TARGET_FILE" ]; then
                                cp "$SOURCE_FILE" "$TARGET_FILE"
                                echo "File copied to $TARGET_FILE"
                            else
                                echo "File already exists at $TARGET_FILE"
                            fi
                        '';
                    };

                    # This is needed so when running tests in GitHub actions, we can execute `devenv up &` without an error.
                    process.implementation = "overmind";
                };
            };

        };
}