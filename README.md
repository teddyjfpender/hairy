# Haskell Monorepo

## Open in Dev Container
1. Open in VS Code / or Cursor
2. “Reopen in Container” when prompted.

## Building & Running

### Build everything
```bash
cabal build all
```

### Run a tutorial executable
```bash
cabal run tutorial-basic
cabal run tutorial-recursion
```

### In GHCi
```bash
# load a specific package
cabal repl tutorial-recursion
# then at the GHCi prompt:
Prelude> main
```
>Replace tutorial-… with the package you’re working on.

With this setup you can:

- **Isolate** each topic in its own package
- **Share** common code in `common-lib`
- **Scale** to dozens of tutorials without clutter
- **Navigate** in VS Code’s package explorer easily

Next up: pick one of your new packages (e.g. `tutorial-recursion`) and start adding QuickCheck properties or spinning off into list-processing in `tutorial-next`. Happy Haskelling!


### General idea
The monorepo should follow this kind of a structure
```
haskell-monorepo/
├── .devcontainer/
│   └── devcontainer.json
├── cabal.project
├── common-lib/                   # shared utilities & types
│   ├── common-lib.cabal
│   └── src/
│       └── Common/
│           └── PreludeExtras.hs
├── tutorial-basic/               # “Hello, Haskell!” tutorial
│   ├── tutorial-basic.cabal
│   └── src/
│       └── Main.hs
├── tutorial-recursion/           # e.g. your factorial & friends
│   ├── tutorial-recursion.cabal
│   └── src/
│       └── Main.hs
└── tutorial-next/                # next topic, e.g. lists or monads
    ├── tutorial-next.cabal
    └── src/
        └── Main.hs

```