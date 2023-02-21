#!/usr/bin/env bash

cachix watch-exec mzabani -- nix build --no-link ".#x86_64-unknown-linux-musl:codd:test:codd-test"
cachix watch-exec mzabani -- nix build --no-link ".#x86_64-unknown-linux-musl:codd:lib:codd.doc"
cachix watch-exec mzabani -- nix build --no-link ".#x86_64-unknown-linux-musl:codd:bench:codd-bench"
cachix watch-exec mzabani -- nix-build --no-output-link ./nix/install-codd-nixpkgs.nix -A codd
cachix watch-exec mzabani -- nix build --no-link .#flakeAeson1.x86_64-linux.codd-musl
cachix watch-exec mzabani -- nix build --no-link .#dockerImage.x86_64-linux
