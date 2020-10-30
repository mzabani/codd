#!/usr/bin/env bash

set -e
trap "rm -f result" EXIT INT

nix-build -A codd.components.library | cachix push mzabani