{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  fixplate-lens-info = pkgs.lib.importJSON ./fixplate-lens.json;
in
  (import (pkgs.fetchFromGitHub {
     owner = "qfpl";
     repo = "fixplate-lens";
     inherit (fixplate-lens-info) rev sha256;
   }) { inherit nixpkgs compiler; })
