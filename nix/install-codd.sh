#!/usr/bin/env bash

# This installs codd with nix-env and good caching set up

nix-env -f "https://raw.githubusercontent.com/mzabani/codd/master/nix/codd-master-default.nix" \
    --option trusted-substituters 'https://cache.nixos.org https://hydra.iohk.io https://mzabani.cachix.org' \
    --option trusted-public-keys  'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= mzabani.cachix.org-1:wnkKakfl+rbT7zTtV1P1tAtjBTuh6GQVX7LfSd9sMbA=' \
    -iA codd.components.exes.codd