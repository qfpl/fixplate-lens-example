{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  record1-info = pkgs.lib.importJSON ./record1.json;
in
  (import (pkgs.fetchFromGitHub {
     owner = "qfpl";
     repo = "record1";
     inherit (record1-info) rev sha256;
   }) { inherit nixpkgs compiler; }).record1 