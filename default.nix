{ nixpkgs ? import <nixos-unstable> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, protolude, sdl2, sdl2-image, stdenv, gi-gtk, haskell-gi-base, text, gtk3, gnome3, wrapGAppsHook, optparse-applicative, random }:
      mkDerivation {
        pname = "movetiles";
        version = "0.0.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableSystemDepends = [ gtk3 wrapGAppsHook gnome3.defaultIconTheme gnome3.dconf ];
        executableHaskellDepends = [ base protolude sdl2 sdl2-image gi-gtk haskell-gi-base text optparse-applicative random ];
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
