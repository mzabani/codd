#!/usr/bin/env bash

cachix watch-exec mzabani -- nix build --no-link .#codd:test:codd-test
cachix watch-exec mzabani -- nix build --no-link .#packages.x86_64-linux.codd:lib:codd.doc
cachix watch-exec mzabani -- nix build --no-link .#dockerImage.x86_64-linux