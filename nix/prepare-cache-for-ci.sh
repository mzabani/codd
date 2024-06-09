#!/usr/bin/env bash

set -eo pipefail

cachix watch-exec mzabani -- nix build --no-link ".#x86_64-unknown-linux-musl:codd:exe:codd"
cachix watch-exec mzabani -- nix build --no-link ".#x86_64-unknown-linux-musl:codd:test:codd-test"
cachix watch-exec mzabani -- nix build --no-link ".#x86_64-unknown-linux-musl:codd:lib:codd.doc"
cachix watch-exec mzabani -- nix build --no-link ".#x86_64-unknown-linux-musl:codd:bench:codd-bench"
cachix watch-exec mzabani -- nix-build --no-out-link ./nix/install-codd-nixpkgs.nix -A codd
cachix watch-exec mzabani -- nix-build --no-out-link ./nix/install-codd-nixpkgs.nix -A coddWithCheck
cachix watch-exec mzabani -- nix build --no-link .#dockerImage.x86_64-linux
