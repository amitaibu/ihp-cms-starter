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

Create RSA keys, used to create image styles

```
ssh-keygen -t rsa -b 4096 -m PEM -f ./Config/jwtRS256.key && openssl rsa -in ./Config/jwtRS256.key -pubout -outform PEM -out ./Config/jwtRS256.key.pub
```

## Execute

- On one terminal tab: `make tailwind-dev`
- On another terminal tab: `devenv up`
