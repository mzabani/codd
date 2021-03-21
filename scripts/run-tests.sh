#!/usr/bin/env bash
set -e

[[ "$1" = "--with-nix" ]] && WITH_NIX="--with-nix"

if [[ $WITH_NIX ]]; then
    echo Building test component with Nix..
    nix-build -A codd.components.tests.codd-test -o local/codd-tests
fi

# Tests which are not Postgres-version dependent first
./scripts/run-test.sh "$WITH_NIX" --skip "/DbDependentSpecs/"

# Postgres-version dependent tests for each possible version next
# Postgres 13
echo Running tests on Postgres 13
nix-shell nix/test-shell-pg13.nix --run "./scripts/run-test.sh $WITH_NIX --match \"/DbDependentSpecs/\""

# Postgres 12
echo Running tests on Postgres 12
nix-shell nix/test-shell-pg12.nix --run "./scripts/run-test.sh $WITH_NIX --match \"/DbDependentSpecs/\""

# Postgres 11
echo Running tests on Postgres 11
nix-shell nix/test-shell-pg11.nix --run "./scripts/run-test.sh $WITH_NIX --match \"/DbDependentSpecs/\""

# Postgres 10
echo Running tests on Postgres 10
nix-shell nix/test-shell-pg10.nix --run "./scripts/run-test.sh $WITH_NIX --match \"/DbDependentSpecs/\""