#!/usr/bin/env bash
set -e

# Tests which are not Postgres-version dependent first
cabal run -O0 codd-test -- --match "/General tests/"

# Postgres-version dependent tests for each possible version next
nix-shell nix/test-shell-pg12.nix --run 'cabal run -O0 codd-test -- --skip "/General tests/"'
nix-shell nix/test-shell-pg11.nix --run 'cabal run -O0 codd-test -- --skip "/General tests/"'

# PG 10 still not working
# nix-shell nix/test-shell-pg10.nix --run 'cabal run -O0 codd-test -- --skip "/General tests/"'