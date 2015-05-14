## Building on Ubuntu 14.04 with ghc 7.10.1

The recommended way to get reflex working is with the [try-reflex](http://github.com/ryantrinkle/try-reflex) nix environment.

A goal of this repository is to stay up to date with the most recent versions of the tools I'm using. Feel free to send a pull request if something needs to be different to support things besides emacs & haskell-mode.

Alternatively, things can be made to work with a more terrestrial setup. Please open an issue if something here gets out of date. *(This is written from memory - will check next time I'm starting up on a fresh machine)*

### Ubuntu 14.04, ghc 7.10.1

First pull in some non-Haskell development packages

```bash
sudo apt-get install libgtk-3-0 libgtk-3-dev
sudo apt-get install libwebkit-3.0-0 libwebkitgtk-3.0-dev
sudo apt-get install libcairo2-dev libpango1.0-dev
```
The last line is getting dependencies for gtk+. reflex-dom uses gtk and webkit only when you build your project with ghc (as opposed to ghcjs). Extremely useful, because when you're configured to build with ghcjs, some project tools don't seem to work (cabal repl, emacs haskell-mode).

We need patched version of cabal 1.22, cabal-install and ghcjs
```bash
mkdir reflex-tools && cd $_
cabal sandbox init
git clone -branch 1.22 git@github.com:haskell/cabal
cabal install cabal/Cabal cabal/cabal-install

We will need node.js soon too - a newer version than is packaged with Ubuntu 14.04

```bash
wget http://nodejs.org/dist/v0.12.2/node-v0.12.2-linux-x64.tar.gz
tar -xf node-v6.02.2-linux-x64.gz
sudo cp node-v0.12.0-linux-x64/bin/node /usr/local/bin
node --version  # Make sure it's 0.12 (there aren't other versions installed)`
```

Get a fresh start for cabal packages

```bash
rm ~/.cabal/bin/cabal
rm -r ~/.ghc
cabal init
cp ~/.cabal-sandbox/bin/cabal ~/.cabal/bin/
```

Get ghcjs set up system-wide

```bash
cabal install gtk2hs-buildtools
cp .cabal-sandbox/bin/gtk2hs* ~/.cabal/bin/
git clone git@github.com:ghcjs/ghcjs
git clone git@github.com:ghcjs/ghcjs-prim
cabal install ./ghcjs ./ghcjs-prim
cp .cabal-sandbox/bin/ghcjs* ~/.cabal/bin/

cd ~ # Don't be in a sandbox environment for ghcjs-boot
ghcjs-boot --dev --ghcjs-boot-dev-branch ghc-7.10
```

That's as much system-wide installation as we want. Other things (reflex, reflex-dom) we will install per-project in a sandbox.

```bash
cd path-to-project
cabal sandbox init

### Build for web
cabal install --only-dependencies --ghcjs
cabal configure --ghcjs
cabal build
cabal install

### Build for running locally (better ghc-mod, cabal repl, haskell-mode)
cabal configure
cabal build
cabal install
```
