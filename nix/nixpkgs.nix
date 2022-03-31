let
  pkgsAttrs = import ./install-codd-nixpkgs.nix;
  nixpkgsSrc = pkgsAttrs.nixpkgsSrc;
  nixpkgsArgs = pkgsAttrs.nixpkgsArgs;

  noKvmOverlay = import ./docker/no-kvm-overlay.nix;
  customArgs = nixpkgsArgs // {
    overlays = nixpkgsArgs.overlays ++ [ noKvmOverlay ];
  };
in import nixpkgsSrc customArgs
