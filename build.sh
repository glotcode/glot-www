#!/bin/bash
# This script is meant to be used in combination with build_docker.sh
set -e

stack_root="/build/.stack"

stack --stack-root $stack_root setup
stack --stack-root $stack_root clean
stack --stack-root $stack_root build

rm -rf /build/release || true
mkdir -p /build/release/glot-www

mv /build/.stack-work/dist/x86_64-*/Cabal-*/build/glot/glot /build/release/glot-www/
cp -rf /build/{config,static} /build/release/glot-www/

tar -C /build/release -czvf /build/release/glot-www.tar.gz glot-www
