#!/usr/bin/env bash
set -e

if [[ "$1" = "--with-nix" ]]; then
    ./local/codd-tests/bin/codd-test "${@:2}"
else
    cabal run -O0 codd-test -- $@
fi