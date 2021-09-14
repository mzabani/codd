self: super: {
    # let postgresql = 1;
    pkgsCross = super.pkgsCross // {
        musl64 = super.pkgsCross.musl64 // {
            haskellPackages = super.pkgsCross.musl64.haskellPackages // {
                postgresql-libpq = self.haskell.lib.addBuildTool (self.haskell.lib.addSetupDepend (self.haskell.lib.addBuildDepend (super.pkgsCross.musl64.postgresql.override (_: {
                    enableSystemd = false;
                    systemd = null;
                })) self.pkgsCross.musl64.haskellPackages.postgresql) self.pkgsCross.musl64.haskellPackages.postgresql) self.pkgsCross.musl64.haskellPackages.postgresql;
                # postgresql-libpq = self.haskell.lib.addBuildDepend super.pkgsCross.musl64.haskellPackages.postgresql-libpq ;
            };
        };
    };
    # pkgs.pkgsCross.musl64.haskellPackages.postgresql-libpq = 1;
    # pkgs.pkgsCross.musl64.haskellPackages.
}

# This is what fails with musl:
# building '/nix/store/virm0fiayiv3v8pppfs1kganyi901w0x-postgresql-libpq-lib-postgresql-libpq-0.9.4.3-x86_64-unknown-linux-musl.drv'...
# unpacking sources
# unpacking source archive /nix/store/5k7nl6azr5nkqfpz0yvl0qa3dx3baia9-postgresql-libpq-0.9.4.3.tar.gz
# source root is postgresql-libpq-0.9.4.3
# setting SOURCE_DATE_EPOCH to timestamp 1000000000 of file postgresql-libpq-0.9.4.3/test/Smoke.hs
# patching sources
# updateAutotoolsGnuConfigScriptsPhase
# configuring
# Configure flags:
# --prefix=/nix/store/f9gfzaxnjzfc5dn6sfxc7imf6xydvfk2-postgresql-libpq-lib-postgresql-libpq-0.9.4.3-x86_64-unknown-linux-musl lib:postgresql-libpq --extra-include-dirs=/nix/store/5zqdnnnms1h570pxjpmpfrpi2gw9ryv0-postgresql-11.10-x86_64-unknown-linux-musl/include --extra-lib-dirs=/nix/store/plnw3hkfyfxrsmdm1x0v8x6x5hx17b6k-postgresql-11.10-x86_64-unknown-linux-musl-lib/lib --package-db=clear --package-db=/nix/store/h2il0xr8jsf22jmnlkv9rqlqa1y1hdhi-x86_64-unknown-linux-musl-postgresql-libpq-lib-postgresql-libpq-0.9.4.3-config/lib/x86_64-unknown-linux-musl-ghc-8.10.4/package.conf.d --flags=-use-pkg-config --exact-configuration --dependency=rts=rts --dependency=ghc-heap=ghc-heap-8.10.4 --dependency=ghc-prim=ghc-prim-0.6.1 --dependency=integer-gmp=integer-gmp-1.0.3.0 --dependency=base=base-4.14.1.0 --dependency=deepseq=deepseq-1.4.4.0 --dependency=array=array-0.5.4.0 --dependency=ghc-boot-th=ghc-boot-th-8.10.4 --dependency=pretty=pretty-1.1.3.6 --dependency=template-haskell=template-haskell-2.16.0.0 --dependency=ghc-boot=ghc-boot-8.10.4 --dependency=ghc=ghc-8.10.4 --dependency=Cabal=Cabal-3.2.1.0 --dependency=array=array-0.5.4.0 --dependency=binary=binary-0.8.8.0 --dependency=bytestring=bytestring-0.10.12.0 --dependency=containers=containers-0.6.2.1 --dependency=directory=directory-1.3.6.0 --dependency=filepath=filepath-1.4.2.1 --dependency=ghc-boot=ghc-boot-8.10.4 --dependency=ghc-compact=ghc-compact-0.1.0.0 --dependency=ghc-prim=ghc-prim-0.6.1 --dependency=hpc=hpc-0.6.1.0 --dependency=mtl=mtl-2.2.2 --dependency=parsec=parsec-3.1.14.0 --dependency=process=process-1.6.9.0 --dependency=text=text-1.2.4.1 --dependency=time=time-1.9.3 --dependency=transformers=transformers-0.5.6.2 --dependency=unix=unix-2.7.2.2 --dependency=xhtml=xhtml-3000.2.2.1 --with-ghc=x86_64-unknown-linux-musl-ghc --with-ghc-pkg=x86_64-unknown-linux-musl-ghc-pkg --with-hsc2hs=x86_64-unknown-linux-musl-hsc2hs --with-gcc=x86_64-unknown-linux-musl-cc --with-ld=x86_64-unknown-linux-musl-ld.gold --ghc-option=-optl-fuse-ld=gold --ld-option=-fuse-ld=gold --with-ar=x86_64-unknown-linux-musl-ar --with-strip=x86_64-unknown-linux-musl-strip --disable-executable-stripping --disable-library-stripping --disable-library-profiling --disable-executable-profiling --enable-static --enable-shared --disable-coverage --enable-library-for-ghci --enable-split-sections
# Configuring library for postgresql-libpq-0.9.4.3..
# Warning: The flag --disable-executable-profiling is deprecated. Please use
# --disable-profiling instead.
# Setup: The program 'pg_config' is required but it could not be found.

# builder for '/nix/store/virm0fiayiv3v8pppfs1kganyi901w0x-postgresql-libpq-lib-postgresql-libpq-0.9.4.3-x86_64-unknown-linux-musl.drv' failed with exit code 1
# cannot build derivation '/nix/store/idjhlmgbcklmp5n57q033b96205rx1w5-codd-exe-codd-0.1.0.0-x86_64-unknown-linux-musl.drv': 1 dependencies couldn't be built
# error: build of '/nix/store/idjhlmgbcklmp5n57q033b96205rx1w5-codd-exe-codd-0.1.0.0-x86_64-unknown-linux-musl.drv' failed
