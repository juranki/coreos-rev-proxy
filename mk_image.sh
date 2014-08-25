#!/usr/bin/env bash

cabal build
cp dist/build/coreos-rev-proxy/coreos-rev-proxy docker
docker build -t coreos-rev-proxy docker
