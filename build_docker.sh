#!/bin/bash

# This script requires the following docker image to exist
#
# FROM ubuntu:16.04
# MAINTAINER Petter Rasmussen "petter.rasmussen@gmail.com"
#
# RUN apt-get update \
#     && apt-get install --no-install-recommends -y postgresql-server-dev-all \
#     && apt-get install --no-install-recommends --no-install-suggests -y curl ca-certificates \
#     && curl -sSL https://get.haskellstack.org/ | sh

# # Set locale
# ENV LANG C.UTF-8
#
# RUN mkdir /build
# VOLUME ["/build"]
# WORKDIR /build
# CMD "/build/build.sh"


docker run \
  --volume $(pwd):/build \
  --rm \
  prasmussen/glot-www-build:latest
