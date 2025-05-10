#!/usr/bin/env bash

strace -f -e openat,open,close -o /tmp/strace-codd-system-resources-test.log \
  `cabal list-bin -O0 codd-test` --match "/SystemResourcesSpecs/RUNNING"
`cabal list-bin -O0 codd-test` --match "/SystemResourcesSpecs/CHECKING"
