#!/usr/bin/env bash
set -e

[[ "$1" = "--with-nix" ]] && WITH_NIX="--with-nix"

if [[ $WITH_NIX ]]; then
    echo Building test component with Nix..
    nix build ".#codd:test:codd-test" -o local/codd-test
fi

./scripts/run-test.sh $WITH_NIX --skip "/DbDependentSpecs/"