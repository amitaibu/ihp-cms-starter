# IHP Landing Page

## Install 

- Install [IHP](https://ihp.digitallyinduced.com/)
- Install devenv:
```
cachix use devenv
nix-env -if https://github.com/cachix/devenv/tarball/latest
```

Let the dependencies automatically build whenever you enter the project's directory
```
direnv allow
```

Install node packages

```
npm install
```

## Execute

- On one terminal tab: `make tailwind-dev`
- On another terminal tab: `devenv up`
