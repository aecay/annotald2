#!/bin/sh

docker run \
       --mount "type=bind,src=.,dst=/data" \
       annotald $@
