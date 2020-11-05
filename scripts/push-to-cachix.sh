#!/usr/bin/env bash

set -e
trap "rm -f result" EXIT INT

nix-build -A codd.components.library | cachix push mzabani

# For some reason, several dependencies aren't properly detected and pushed..