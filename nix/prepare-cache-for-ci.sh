#!/usr/bin/env bash

set -eo pipefail

cachix watch-exec mzabani -- nix-build --no-out-link -A coddexe
cachix watch-exec mzabani -- nix-build --no-out-link -A coddtests
cachix watch-exec mzabani -- nix-build --no-out-link -A coddhaddocks
cachix watch-exec mzabani -- nix-build --no-out-link -A coddbenchmarks
cachix watch-exec mzabani -- nix-build --no-out-link -A dockerImage
