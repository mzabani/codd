#!/usr/bin/env bash

# This installs codd with nix-env and good caching set up
# Note that:
# 1. Github's auto generated master.tar.gz archive does not include submodules.
# 2. nix-env interprets the "-f" flag differently depending on whether it's a URL or
#    a local file path:
#    "If the argument starts with http:// or https://, it is interpreted as the URL of a tarball that will be downloaded and  unpacked
#     to a temporary location"

curl https://raw.githubusercontent.com/mzabani/codd/master/nix/codd-master-default.nix -o codd-master-default-TEMPORARY-FILE.nix
trap "rm codd-master-default-TEMPORARY-FILE.nix" EXIT
nix-env -f "./codd-master-default-TEMPORARY-FILE.nix" \
    --option trusted-substituters 'https://cache.nixos.org https://hydra.iohk.io https://mzabani.cachix.org' \
    --option trusted-public-keys  'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= mzabani.cachix.org-1:wnkKakfl+rbT7zTtV1P1tAtjBTuh6GQVX7LfSd9sMbA=' \
    -iA codd.components.exes.codd
