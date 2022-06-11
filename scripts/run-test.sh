#!/usr/bin/env bash
set -e

if [[ "$1" = "--with-nix" ]]; then
    ./local/codd-test/bin/codd-test "${@:2}"
elif [[ "$1" = "" ]]; then
    cabal run -O0 codd-test
else
    cabal run -O0 codd-test -- "$@"
fi