#!/usr/bin/env bash

set -eo pipefail

# This installs codd with nix-env and good caching set up
# Note that:
# - nix-env and nix-shell both interpret the file path differently depending on whether it's a URL or
#   a local file path:
#    "If the argument starts with http:// or https://, it is interpreted as the URL of a tarball that will be downloaded and unpacked
#     to a temporary location"

if ! command -v git &> /dev/null
then
    echo "git executable not found, but is necessary to install codd"
    exit 1
fi

if ! command -v nix-env &> /dev/null
then
    echo "nix-env executable not found, but is necessary to install codd. Please install Nix by visiting https://nixos.org/download.html"
    exit 1
fi

rm -rf /tmp/codd-checkout-Y6fRwa_23x
git clone --depth 1 https://github.com/mzabani/codd.git /tmp/codd-checkout-Y6fRwa_23x

nix-env -f "/tmp/codd-checkout-Y6fRwa_23x/nix/install-codd-nixpkgs.nix" \
    --option trusted-substituters 'https://cache.nixos.org https://hydra.iohk.io https://mzabani.cachix.org' \
    --option trusted-public-keys  'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= mzabani.cachix.org-1:wnkKakfl+rbT7zTtV1P1tAtjBTuh6GQVX7LfSd9sMbA=' \
    -iA codd

rm -rf /tmp/codd-checkout-Y6fRwa_23x

echo "---------------------------------------------------------------"
echo "Codd successfully installed. Run 'codd --help' to view options."
echo "Run 'nix-env --uninstall codd' to uninstall"