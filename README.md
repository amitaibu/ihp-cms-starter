# IHP Landing Page

## Install 

- Install [IHP](https://ihp.digitallyinduced.com/)
- Install [devenv](https://devenv.sh/getting-started/)

```
cachix use devenv
nix-env -if https://github.com/cachix/devenv/tarball/latest
direnv allow
npm install
```

## Execute

- On one terminal tab: `make tailwind-dev`
- On another terminal tab: `devenv up`
