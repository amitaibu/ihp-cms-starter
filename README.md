# IHP Landing Page

## Install 

- Install [IHP](https://ihp.digitallyinduced.com/)
- Install `devenv` and enable `.envrc`

```
cachix use devenv
nix-env -if https://github.com/cachix/devenv/tarball/latest
direnv allow
```

## Execute

- On one terminal tab: `make tailwind-dev`
- On another terminal tab: `devenv up`
