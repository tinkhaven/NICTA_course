{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, containers, directory, doctest
      , filepath, QuickCheck, stdenv, template-haskell
      }:
      mkDerivation {
        pname = "course";
        version = "0.1.4";
        src = ./.;
        libraryHaskellDepends = [
          array base containers doctest QuickCheck
        ];
        testHaskellDepends = [
          base directory doctest filepath QuickCheck template-haskell
        ];
        homepage = "https://github.com/NICTA/course";
        description = "Source code for a functional programming course";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
