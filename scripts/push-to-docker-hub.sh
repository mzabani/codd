#!/usr/bin/env bash

nix-build -o results/codd-docker -A dockerImage
docker load -i results/codd-docker

docker login
docker tag codd:latest mzabani/codd:latest
docker push mzabani/codd:latest
