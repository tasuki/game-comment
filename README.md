# Game Comment

A thing to analyze Go(Torus), Hex, TwixT games from LittleGolem.

## Setup

On Debian 12 Bookworm, frontend toolchain:

```
sudo apt install npm uglifyjs
sudo npm install -g elm@latest-0.19.1 elm-test elm-format serve
```

Backend toolchain:

```
sudo apt install haskell-stack libtinfo-dev zlib1g-dev
```

## Develop

[![CI](https://github.com/tasuki/game-comment/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/tasuki/game-comment/actions/workflows/ci.yml)

Run `make develop` in both `backend/` and `frontend/` directories. Also possible to run it just once in root, but noisy intermingled output is not fun.

LSP: install `mise`; run `mise up` and `mise install-haskell-tools`; then perhaps LSP in nvim will work.

## Deploy

Git push! The `make release` won't work unless a `game-comment` service is set up. The dependencies are circular unfortunately. Can't be bothered to fix that now...
