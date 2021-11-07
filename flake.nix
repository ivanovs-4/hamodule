{
  description = "hamodule";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    inputs.haskell-flake-utils.lib.simpleCabal2flake {
      inherit self nixpkgs;
      name = "hamodule";
    };
}
