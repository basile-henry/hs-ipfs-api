# IPFS API wrapper library in Haskell

A client library for the IPFS HTTP API, implemented in Haskell.

## Installing the library

```zsh
cabal configure
cabal install --only-dependencies
cabal install hprotoc
make
cabal build
```

## Usage

Checkout the [documentation](https://ipfs.io/ipfs/QmPtj3NjRSGob9xvSLVWXihxN721MTEyofrKTzhkQsyE8y/)! :)

## Tests

Note: Some of the tests are node dependent and might publish hashes (if that is a problem, disable the IPNS tests by modifying tests/Main.hs)

```zsh
cabal configure --enable-test
cabal install --only-dependencies --enable-test
cabal test
```