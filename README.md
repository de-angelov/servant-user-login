# haskell example app


## about

the repo includes vscode dev environment setup for haskell

docker compose file used to run main app and postgres containers



## commands

to run build on every change

```bash
find . -name '*.hs'  | entr -r cabal build
```

to start main app and postgres containers

```bash
docker-compose up
```

