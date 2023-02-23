final: prev:
prev // rec {
  # Taken from https://github.com/helsinki-systems/nixpkgs/blob/940b7d4ee1a1008881f044cdf2986b817c753c42/pkgs/servers/sql/postgresql/default.nix
  # , since while https://github.com/input-output-hk/haskell.nix/issues/1857 isn't resolved
  # it's not possible to bump haskell.nix to a nixpkgs with postgres 15.
  postgresql_15 = prev.postgresql_14.overrideAttrs (_: rec {
    psqlSchema = "15";
    version = "15.2";
    src = prev.fetchurl {
      url =
        "mirror://postgresql/source/v${version}/postgresql-${version}.tar.bz2";
      hash = "sha256-maIXH8PWtbX1a3V6ejy4XVCaOOQnOAXe8jlB7SuEaMc=";
    };
  });
  postgresql = postgresql_15;
}
