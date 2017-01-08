{ mkDerivation, base, mtl, stdenv, transformers, xhb
, xhb-event-queue, xhb-keysyms, xhb-mapping-state, xhb-monad
}:
mkDerivation {
  pname = "tinywm";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base mtl transformers xhb xhb-event-queue xhb-keysyms
    xhb-mapping-state xhb-monad
  ];
  license = stdenv.lib.licenses.mit;
}
