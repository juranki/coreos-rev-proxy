#!/usr/bin/env bash

cabal sandbox init
cabal install --only-dependencies
wget https://github.com/coreos/etcd/releases/download/v0.4.6/etcd-v0.4.6-linux-amd64.tar.gz
tar xzf etcd-v0.4.6-linux-amd64.tar.gz
rm etcd-v0.4.6-linux-amd64.tar.gz
