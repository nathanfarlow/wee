#!/bin/bash

if ! [ -x "$(command -v docker)" ]; then
    echo "Error: docker is not installed. Install docker and try again." >&2
    exit 1
fi

if [ $# -ne 2 ]; then
    echo "Usage: $0 <file.c> <memory size>" >&2
    echo "Example: $0 test.c 100000" >&2
    exit 1
fi

if [ ! -f "$1" ]; then
    echo "Error: $1 is not a C file" >&2
    exit 1
fi

PARENT_DIR=$(realpath $(dirname $1))
FILE_NAME=$(basename $1)

if [ -z "$(docker images -q wee)" ]; then
    echo "Building docker image for the first time. This may take several minutes." >&2
fi

docker build -t wee .
docker run -it -v $PARENT_DIR:/target -w /target wee bash -c "/home/opam/elvm/out/8cc -S $FILE_NAME -I /home/opam/elvm/libc -o /tmp/out.elvm && wee /tmp/out.elvm $2"
