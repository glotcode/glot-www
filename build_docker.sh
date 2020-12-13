#!/bin/bash

# This script requires the following docker image to exist
#
# FROM haskell:8
# MAINTAINER Petter Rasmussen "petter.rasmussen@gmail.com"
#
# RUN apt-get update \
#     && apt-get install --no-install-recommends -y postgresql-server-dev-all
#
# RUN mkdir /build
# VOLUME ["/build"]
# WORKDIR /build
# CMD "/build/build.sh"

docker run \
  --volume $(pwd):/build \
  --rm \
  prasmussen/glot-www-build:latest
