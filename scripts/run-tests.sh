#!/usr/bin/env bash
set -e

# Tests which are not Postgres-version dependent first
# cabal run -O0 codd-test -- --skip "/DbDependentSpecs/"

# Postgres-version dependent tests for each possible version next
echo Running tests on Postgres 12
nix-shell nix/test-shell-pg12.nix --run 'cabal run -O0 codd-test -- --match "/DbDependentSpecs/"'

# Postgres 11 is not supported yet
echo Running tests on Postgres 11
nix-shell nix/test-shell-pg11.nix --run 'cabal run -O0 codd-test -- --match "/DbDependentSpecs/"'

# Postgres 10 is not supported yet.
echo Running tests on Postgres 10
nix-shell nix/test-shell-pg10.nix --run 'cabal run -O0 codd-test -- --match "/DbDependentSpecs/"'