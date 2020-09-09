{ pkgs ? import ./nixpkgs.nix }:
    {
        writeShellScriptInBinFolder = name: text: pkgs.writeTextFile {
            inherit name;
            executable = true;
            destination = "/bin/${name}";
            text = ''
                #!${pkgs.runtimeShell}
                set -e
                trap "set +e" 0
                ${text}
                set +e
                '';
            checkPhase = ''
                ${pkgs.stdenv.shell} -n $out/bin/${name}
            '';
        };
    }