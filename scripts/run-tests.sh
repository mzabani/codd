#!/usr/bin/env bash
set -e

[[ "$1" = "--with-nix" ]] && WITH_NIX="--with-nix"
[[ "$1" = "--with-nix" ]] && NIX_DEV_ARGS="-i"

if [[ $WITH_NIX ]]; then
    echo Building test component with Nix..
    nix build ".#codd:test:codd-test" -o local/codd-test
fi

# Tests which are not Postgres-version dependent first
./scripts/run-test.sh $WITH_NIX --skip "/DbDependentSpecs/"

# Postgres-version dependent tests for each possible version next
echo Running tests on Postgres 14
nix develop ".#testShells.x86_64-linux.pg14" $NIX_DEV_ARGS -c ./scripts/run-db-test.sh "$WITH_NIX"

echo Running tests on Postgres 13
nix develop ".#testShells.x86_64-linux.pg13" $NIX_DEV_ARGS -c ./scripts/run-db-test.sh "$WITH_NIX"

echo Running tests on Postgres 12
nix develop ".#testShells.x86_64-linux.pg12" $NIX_DEV_ARGS -c ./scripts/run-db-test.sh "$WITH_NIX"

echo Running tests on Postgres 11
nix develop ".#testShells.x86_64-linux.pg11" $NIX_DEV_ARGS -c ./scripts/run-db-test.sh "$WITH_NIX"

echo Running tests on Postgres 10
nix develop ".#testShells.x86_64-linux.pg10" $NIX_DEV_ARGS -c ./scripts/run-db-test.sh "$WITH_NIX"
