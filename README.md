# Reflex Playground

Playground to evaluate [reflex](https://github.com/reflex-frp/reflex) and [reflex-dom](https://github.com/reflex-frp/reflex-dom) and play with different examples.

## Install

```
$ stack setup
$ stack build
```

Get ready for a long compilation of GHCJS, you also might need additional libraries like `happy` and `alex`.

## Run

```
$ cp app.css .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/reflex-demo/reflex-demo.jsexe/
$ cp index.html .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/reflex-demo/reflex-demo.jsexe/
$ firefox .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/reflex-demo/reflex-demo.jsexe/index.html
```

## TODO

- Add Nix deps to remove tedious task of environment configuration

