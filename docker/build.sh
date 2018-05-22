#!/bin/sh

# TODO: elm build

rsync --exclude .git ~/projects/lovett .
cp ../python/requirements.txt .
cp ../python/main.py .
cp -r ../python/static .

sudo docker build -t annotald .
