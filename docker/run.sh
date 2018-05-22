#!/bin/sh

HERE=`readlink -f .`

docker run \
       --mount "type=bind,src=${HERE},dst=/data" \
       -p 8000:8000 \
       annotald:latest python /annotald/main.py --docker $@
