#!/usr/bin/env bash

nix-build -o results/codd-docker nix/docker/codd-exe.nix
docker load -i results/codd-docker

docker login
docker tag codd:latest mzabani/codd:latest
docker push mzabani/codd:latest