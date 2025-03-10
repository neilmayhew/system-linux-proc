{ mkDerivation, attoparsec, base, binary, bytestring, containers
, directory, errors, filepath, hedgehog, lib, optparse-applicative
, pretty-show, terminal-size, text
}:
mkDerivation {
  pname = "system-linux-proc";
  version = "0.1.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory errors text
  ];
  executableHaskellDepends = [
    attoparsec base binary bytestring directory errors filepath
    optparse-applicative terminal-size text
  ];
  testHaskellDepends = [ base directory hedgehog pretty-show ];
  homepage = "https://github.com/erikd/system-linux-proc";
  description = "A library for accessing the /proc filesystem in Linux";
  license = lib.licenses.bsd3;
}
