#!/usr/bin/env bash

# This installs codd with nix-env and good caching set up
# Note that:
# 1. Github's auto generated master.tar.gz archive does not include submodules.
# 2. nix-env and nix-shell both interpret the file path differently depending on whether it's a URL or
#    a local file path:
#    "If the argument starts with http:// or https://, it is interpreted as the URL of a tarball that will be downloaded and  unpacked
#     to a temporary location"
# 3. I could not find a way to use Nix's fetchgit* functions to fetch `master` impurely.
#    builtins.fetchGit does it, but doesn't allow submodules to be fetched for Nix <2.4.

trap "rm -f install-codd-nixpkgs-TEMPORARY-FILE.nix" EXIT
curl https://raw.githubusercontent.com/mzabani/codd/master/nix/install-codd-nixpkgs.nix -o install-codd-nixpkgs-TEMPORARY-FILE.nix

# To test this script, create a new directory and use the line below instead of "curl".
# cp ~/Projects/codd/nix/install-codd-nixpkgs.nix install-codd-nixpkgs-TEMPORARY-FILE.nix

CODDPATH=$(nix-shell -E "let n = import ./install-codd-nixpkgs-TEMPORARY-FILE.nix; in n.installShell" --run "nix-prefetch-git --quiet --fetch-submodules https://github.com/mzabani/codd.git refs/heads/master | jq -r .path")
nix-env -f "$CODDPATH" \
    --option trusted-substituters 'https://cache.nixos.org https://hydra.iohk.io https://mzabani.cachix.org' \
    --option trusted-public-keys  'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= mzabani.cachix.org-1:wnkKakfl+rbT7zTtV1P1tAtjBTuh6GQVX7LfSd9sMbA=' \
    -iA codd.components.exes.codd
