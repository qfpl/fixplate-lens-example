{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  variant1-info = pkgs.lib.importJSON ./variant1.json;
in
  (import (pkgs.fetchFromGitHub {
     owner = "qfpl";
     repo = "variant1";
     inherit (variant1-info) rev sha256;
   }) { inherit nixpkgs compiler; }).variant1 