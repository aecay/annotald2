#!/bin/sh

HERE=$(dirname `readlink -f $0`)

source $HERE/python/venv/bin/activate

$HERE/python/main.py $@
