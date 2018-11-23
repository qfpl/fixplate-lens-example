{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default", doBenchmark ? false
}:

let
  overlay =
    pkgsSelf: pkgsSuper: {
      haskellPackages =
        (if compiler == "default"
         then pkgsSuper.haskellPackages
         else pkgsSuper.haskell.packages.${compiler}).override {
           overrides = self: super: {
             fixplate-lens =
               import ./nix/fixplate-lens.nix {
                 nixpkgs = pkgsSelf;
                 inherit compiler;
               };
             variant1 =
               import ./nix/variant1.nix {
                 nixpkgs = pkgsSelf;
                 inherit compiler;
               };
             variant1-lens =
               import ./nix/variant1-lens.nix {
                 nixpkgs = pkgsSelf;
                 inherit compiler;
               };
           };
         };
    };

  f = import ./fixplate-lens-example.nix;

  pkgs = import nixpkgs.path { overlays = [ overlay ]; };

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
