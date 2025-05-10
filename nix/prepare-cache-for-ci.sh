#!/usr/bin/env bash

set -eo pipefail

cachix watch-exec mzabani -- nix-build --no-out-link -A coddexe
cachix watch-exec mzabani -- nix-build --no-out-link -A coddtests
cachix watch-exec mzabani -- nix-build --no-out-link -A coddhaddocks
cachix watch-exec mzabani -- nix-build --no-out-link -A coddbenchmarks
cachix watch-exec mzabani -- nix-build --no-out-link ./nix/install-codd-nixpkgs.nix -A codd
cachix watch-exec mzabani -- nix-build --no-out-link ./nix/install-codd-nixpkgs.nix -A coddWithCheck
cachix watch-exec mzabani -- nix-build --no-out-link -A dockerImage
