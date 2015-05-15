# my-reflex-recipes
A collection of worked [reflex-dom](https://github.com/ryantrinkle/reflex-dom) exercises. See them running [here](http://web.mit.edu/greghale/Public/my-reflex-examples/index.html).

## Installation
Building the dependencies is still tricky.

Use [try-reflex](http://github.com/ryantrinkle/try-reflex) nix environment, or see buildUbuntu.md in this repository

For this package, build a javascript app:
```bash
cabal sandbox init
cabal install --ghcjs --only-dependencies
cabal configure --ghcjs
cabal build
cabal install```

Or build a Haskell app that will render the DOM in a GTK+ window:
```bash
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
cabal install
```

Switch between a ghc or ghcjs build with the `--ghcjs` flag when configuring. Building for gtk seems to work better with `cabal repl`, ghc-mod, haskell-mode

## Purpose
I'm having trouble wrapping my head around [reflex-dom](http://github.com/ryantrinkle/reflex-dom), but it looks awesome and I want to invest some energy in it. So I'll try to do some simple exercises here. Perhaps useful to others in the same boat. I'll hopefully be able to contribute documentation to them as I figure things out, and I hope you'll consider doing the same!

## Output
Let's render the example code and output to .js to see the nice results. TODO: add link when there's something to show.
