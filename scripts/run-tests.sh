#!/usr/bin/env bash
set -e

[[ "$1" = "--with-nix" ]] && WITH_NIX="--with-nix"
[[ "$1" = "--with-nix" ]] && NIX_SHELL_ARGS="--pure"

if [[ $WITH_NIX ]]; then
    echo Building test component with Nix..
    nix build .#codd:test:codd-test -o local/codd-test
fi

# Tests which are not Postgres-version dependent first
./scripts/run-test.sh $WITH_NIX --skip "/DbDependentSpecs/"

# Still don't know how to replace nix-shell with 'nix develop',
# particularly because test-shell-pgX.nix try to import nixpkgs.nix
# and thus are not pure in the flakes sense.
# Postgres-version dependent tests for each possible version next
# Postgres 14
echo Running tests on Postgres 14
nix-shell $NIX_SHELL_ARGS nix/test-shell-pg14.nix --run "./scripts/run-test.sh $WITH_NIX --match \"/DbDependentSpecs/\""

# Postgres 13
echo Running tests on Postgres 13
nix-shell $NIX_SHELL_ARGS nix/test-shell-pg13.nix --run "./scripts/run-test.sh $WITH_NIX --match \"/DbDependentSpecs/\""

# Postgres 12
echo Running tests on Postgres 12
nix-shell $NIX_SHELL_ARGS nix/test-shell-pg12.nix --run "./scripts/run-test.sh $WITH_NIX --match \"/DbDependentSpecs/\""

# Postgres 11
echo Running tests on Postgres 11
nix-shell $NIX_SHELL_ARGS nix/test-shell-pg11.nix --run "./scripts/run-test.sh $WITH_NIX --match \"/DbDependentSpecs/\""

# Postgres 10
echo Running tests on Postgres 10
nix-shell $NIX_SHELL_ARGS nix/test-shell-pg10.nix --run "./scripts/run-test.sh $WITH_NIX --match \"/DbDependentSpecs/\""
