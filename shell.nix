{ nixpkgs ? (import (fetchTarball "channel:nixpkgs-unstable") {}), compiler ? "ghc865" }:
#{ nixpkgs ? (import (fetchTarball "channel:nixos-19.03") {}), compiler ? "ghc864" }:
# Currently works with either nixos-19.03 or nixpkgs-unstable as of May 27 2019.
# if unstable stops working at some point fall back to 19.03 channel, which has an older ghc with more bugs ;)

let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    ###### Tools that help editing and development #####
    ghcid
    hindent
    hlint
    ###### Packages we want from the nixpkgs binary cache #####
    ##### Commented ones don't work and have to be compiled
    #argon2
    #clay
    #hasql-migration
    #hasql-pool
    DRBG
    aeson
    bytestring
    configurator
    crypto-api
    data-default
    entropy
    hasql
    hasql-transaction
    http-types
    jose-jwt
    lens
    mtl
    protolude
    random
    scotty
    stm
    string-qq
    text
    text-short
    time
    transformers
    transformers
    util
    uuid
    uuid-types
    wai
    wai-cors
    wai-extra
    warp
    ##### END Packages ######
    ##### From cabal file as of:
    # commit bf9ef4555e72419934d5e63287c3fea3d01979c8
    # Date:   Sun May 5 15:41:40 2019 +0200
    ##                     , argon2 >= 1.3.0.1 && < 1.4
    ##                     , clay
    ##                     , hasql-migration >= 0.2.0 && < 0.3.0
    ##                     , hasql-pool >= 0.5.0 && < 0.6
    #-                     , DRBG
    #-                     , aeson
    #-                     , bytestring
    #-                     , configurator
    #-                     , crypto-api
    #-                     , data-default >= 0.7.1.1 && < 1.0
    #-                     , entropy
    #-                     , hasql >= 1.3.0.3 && < 1.4
    #-                     , hasql-transaction
    #-                     , http-types
    #-                     , jose-jwt
    #-                     , lens
    #-                     , mtl
    #-                     , protolude
    #-                     , random
    #-                     , scotty
    #-                     , stm
    #-                     , string-qq
    #-                     , text
    #-                     , text-short
    #-                     , time
    #-                     , transformers
    #-                     , transformers
    #-                     , util
    #-                     , uuid
    #-                     , uuid-types
    #-                     , wai >= 3.0.0 && <3.3
    #-                     , wai-cors >= 0.2.6 && < 1.0
    #-                     , wai-extra >=3.0.0 && < 3.3
    #-                     , warp >= 3.2.25 && < 3.3

  ]);
in
pkgs.stdenv.mkDerivation {
  name = "dukkhalessserver";
  buildInputs = with pkgs; [ ghc zlib postgresql cabal-install ];
  propagatedBuildInputs = with pkgs; [ ghc zlib postgresql cabal-install ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
