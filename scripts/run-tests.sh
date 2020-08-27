#!/usr/bin/env bash

# Tests which are not Postgres-version dependent first
cabal run -O0 codd-test -- --match "/General tests/"

# Postgres-version dependent tests for each possible version next
