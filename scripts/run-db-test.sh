#!/usr/bin/env bash
set -e

trap "echo Stopping postgres && pg_ctl stop" EXIT

./scripts/run-test.sh "${@}" --match "/DbDependentSpecs/"