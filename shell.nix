{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hpack, http-conduit, stdenv
      , utf8-string
      }:
      mkDerivation {
        pname = "icfpc-mmxx-starterkit-haskell";
        version = "0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base http-conduit utf8-string ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [ base http-conduit utf8-string ];
        doHaddock = false;
        prePatch = "hpack";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
