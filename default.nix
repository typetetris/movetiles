{ nixpkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/3878342ef02ded79a81d8d03092386804531992f.tar.gz) {}, compiler ? "default", doBenchmark ? false, sdl2check ? false }:

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

  haskellPackages1 = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};
  haskellPackages = if sdl2check
                       then haskellPackages1
                       else with pkgs.haskell.lib; haskellPackages1.override {
                         overrides = self: super: {
                           sdl2 = dontCheck super.sdl2;
                           sdl2-image = dontCheck super.sdl2-image;
                         };
                       };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
