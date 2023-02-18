#!/usr/bin/env bash
set -e

[[ "$1" = "--with-nix" ]] && WITH_NIX="--with-nix"
[[ "$1" = "--with-nix" ]] && NIX_DEV_ARGS="-i"

if [[ $WITH_NIX ]]; then
    echo Building test component with Nix..
    nix build ".#x86_64-unknown-linux-musl:codd:test:codd-test" -o local/codd-test
fi

# Always build codd with aeson 1 with Nix to avoid ruining cached local build
# artifacts.
echo "Building codd with Aeson 1"
nix build --no-link ".#flakeAeson1.x86_64-linux.defaultPackage"

echo "Running tests with the Aeson 2 version of codd"
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
