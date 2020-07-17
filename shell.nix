{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, hpack
      , http-conduit, mtl, stdenv, unordered-containers, utf8-string
      }:
      mkDerivation {
        pname = "icfpc2020";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          attoparsec base bytestring http-conduit mtl unordered-containers
          utf8-string
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          attoparsec base bytestring http-conduit mtl unordered-containers
          utf8-string
        ];
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
