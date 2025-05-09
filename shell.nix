{ }:
let
  pkgs = import ./nix/nixpkgs.nix {};
  project = (import ./default.nix { inherit pkgs; }).project;
in
project.shellFor {
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
    fourmolu = "latest";
  };
  exactDeps = true;
  buildInputs =
    with pkgs;
    [
      cacert
      ghcid
      glibcLocales
      hyperfine
      postgresql_16
      nix-prefetch-docker
      podman
      run
      shellcheck
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ strace ];

  shellHook = ''
    source scripts/source-env.sh .env

    # Postgres 15 insists in appearing in PATH before postgres 16.
    # So we add postgres 16 _again_ to the beginning of the PATH, and also some useful scripts
    export PATH="${pkgs.postgresql_16}/bin:$PATH:scripts/path"

    # Starting doesn't actually work with direnv. I tried to daemonize starting postgres but was not able
    # to make it work. See https://github.com/direnv/direnv/issues/755
    ./scripts/init-pg-cluster.sh ./conf

    echo You should be able to start postgres with 'pg_ctl start' and use 'psql' to connect to it, and it will be independent from any your own system might have provided.
    echo If 'psql' fails to connect, check logs at $PGDATA/log/
  '';
}
