let
  nixpkgsRev = "008d33c";
  compilerVersion = "ghc947";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";

  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  pkgs = import (githubTarball "NixOS" "nixpkgs" nixpkgsRev) { inherit config; };
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;

  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          hs-redis = super.callCabal2nix "hs-redis" (gitIgnore [./.gitignore] ./.) {};
        };
      };
    };
  };


in {
  inherit pkgs;
  shell = compilerSet.shellFor {
    packages = p: [p.hs-redis];
    buildInputs = with pkgs; [
      compilerSet.cabal-install
    ];
  };
}
