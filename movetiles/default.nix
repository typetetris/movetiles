{ nixpkgs ? import <nixos-unstable> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, linear
      , optparse-applicative, protolude, sdl2, sdl2-image, stdenv, vector, gi-gtk, haskell-gi-base, text, gtk3, wrapGAppsHook, gnome3
      }:
      mkDerivation {
        pname = "movetiles";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableSystemDepends = [ gtk3 wrapGAppsHook gnome3.defaultIconTheme gnome3.dconf ];
        executableHaskellDepends = [
          base containers linear optparse-applicative protolude sdl2
          sdl2-image vector gi-gtk haskell-gi-base text
        ];
        homepage = "http://github.com/typetetris/sdl-exploration";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
