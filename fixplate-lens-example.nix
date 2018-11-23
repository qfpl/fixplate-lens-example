{ mkDerivation, base, deriving-compat, fixplate, fixplate-lens
, lens, record1, record1-lens, stdenv, variant1, variant1-lens
}:
mkDerivation {
  pname = "fixplate-lens-example";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base deriving-compat fixplate fixplate-lens lens record1
    record1-lens variant1 variant1-lens
  ];
  description = "An example of using classy prisms & variants with @fixplate@";
  license = stdenv.lib.licenses.bsd3;
}
