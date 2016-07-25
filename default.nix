{ mkDerivation, array, base, containers, mtl, stdenv, transformers
, xhb, xhb-keysyms, xhb-mapping-state, xhb-monad
}:
mkDerivation {
  pname = "tinywm";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base containers mtl transformers xhb xhb-keysyms
    xhb-mapping-state xhb-monad
  ];
  license = stdenv.lib.licenses.mit;
}
