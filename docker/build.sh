#!/bin/sh

set -e

rm -rf deps

mkdir deps

( cd ~/projects/lovett;
  python setup.py sdist;
)

LOVETT_VERSION=$( cd ~/projects/lovett;
  echo `python setup.py -V`
)

cp ~/projects/lovett/dist/lovett-${LOVETT_VERSION}.tar.gz deps

cp ~/projects/lovett/requirements.txt deps/lovett-requirements.txt
cp ../python/requirements.txt deps
cp ../python/main.py deps
cp -r ../python/static deps

rm deps/static/elm.js

(cd ..; elm-make src/Main.elm --yes --warn --output=docker/deps/static/elm.js)

sudo docker build -t aecay/annotald:testing .
