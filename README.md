# README
This is a ChatGPT API CLI(Command-Line Interface) write with Haskell.

ghc version 9.0.2

cabal version 3.6.2.0

## Usage
add a `.env` file in the project path and set:

```shell
api-key={your api key here}
proxy-host={your proxy server host name/ip}
proxy-port={your proxy server port}
```
and then you can build and run the project:

```shell
cabal build

cabal run
```

then there is a prompt wait for your input.
input `quit` will exit the repl.
have fun!
