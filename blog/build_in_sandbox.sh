#!/bin/bash

PS4='($LINENO)+ '
set -x
set -e

rm -fr .cabal-sandbox cabal.sandbox.config dist

cabal sandbox init

export PATH=`pwd`/.cabal-sandbox/bin:$PATH

cabal install happy alex --haddock-hyperlink-source --dependencies-only
cabal install happy alex --haddock-hyperlink-source

cabal install --haddock-hyperlink-source --dependencies-only # Is this necessary? Why not just cabal install?
cabal install

cabal install angel-0.4.4 --haddock-hyperlink-source --dependencies-only
cabal install angel-0.4.4 --haddock-hyperlink-source

cabal install yesod-test --haddock-hyperlink-source --dependencies-only
cabal install yesod-test --haddock-hyperlink-source

