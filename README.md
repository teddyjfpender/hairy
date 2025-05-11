\
[<img src="./hairy.png" alt="HAIRY Prover Logo" width="200"/>]()

# HAIRY - STARK Prover

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://img.shields.io/github/actions/workflow/status/<your-github-username>/<your-repo-name>/haskell-ci.yml?branch=main)](https://github.com/<your-github-username>/<your-repo-name>/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/<your-hackage-package-name>.svg)](https://hackage.haskell.org/package/<your-hackage-package-name>)
[![Stackage LTS](http://stackage.org/package/<your-hackage-package-name>/badge/lts)](http://stackage.org/lts/package/<your-hackage-package-name>)
[![Stackage Nightly](http://stackage.org/package/<your-hackage-package-name>/badge/nightly)](http://stackage.org/nightly/package/<your-hackage-package-name>)
[![GHC Version](https://img.shields.io/badge/GHC-Update%20Version-blueviolet.svg)](#prerequisites)

**H**askell **A**lgebraic **IR** (AIR) prover… **Y**? Because every project name needs a questionable backronym and we're not shaving it off. **Hairy** is a re-write of Stwo in Haskell.

This project is built & maintained by AI Agents (shocking!).

## Table of Contents

- [Features](#features)
- [Project Structure](#project-structure)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
  - [Using Cabal](#using-cabal)
  - [Using Stack](#using-stack)
- [Usage](#usage)
  - [Library](#library)
  - [Executables](#executables)
- [Running Tests](#running-tests)
- [Development](#development)
- [Contributing](#contributing)
- [License](#license)

## Features

Please update this section with the key features of the HAIRY prover. Examples:
- Efficient STARK proof generation for specific AIRs.
- Modular architecture for easy extension.
- ...

## Project Structure

This project uses Cabal and is structured as a multi-package monorepo. The main packages are defined in the root `cabal.project` file:

- `core`: The primary library containing the core STARK proving logic, located in `core/core.cabal`.
- `directory`: A supporting package, potentially for utilities or specific components, located in `core/directory/directory.cabal`.
- *(Add more packages here as your project evolves)*

The `cabal.project` file orchestrates the build for the entire workspace.

## Prerequisites

Before you begin, ensure you have the following installed:

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) (e.g., version 9.x.x or as specified in your `.cabal` files)
- [Cabal (Haskell build tool)](https://www.haskell.org/cabal/) (e.g., version 3.x)
- OR [Stack (Haskell build tool)](https://docs.haskellstack.org/en/stable/install_and_upgrade/) (optional, if you add a `stack.yaml`)

## Installation

Clone the repository:
```bash
git clone https://github.com/<your-github-username>/<your-repo-name>.git
cd <your-repo-name>
```

### Using Cabal

To build the project using Cabal:
```bash
cabal update  # Fetches the latest package list from Hackage
cabal build all # Builds all packages in the project
cabal test all # Tests all packages in the project
```
This will compile all local packages defined in your `cabal.project` file.

To install executables (if any) from the project:
```bash
# Replace <executable-name> with the actual executable name from your .cabal file
cabal install exe:<executable-name> --install-method=copy --overwrite-policy=always
```
Or, to run an executable directly:
```bash
# Replace <executable-name> with the actual executable name
cabal run exe:<executable-name> -- arguments
```

### Using Stack

(Optional: If you set up a `stack.yaml` for the project)
```bash
stack setup   # Downloads the correct GHC version if needed
stack build   # Builds all packages
stack test    # Tests all packages
```
To run an executable:
```bash
# Replace <executable-name> with the actual executable name
stack exec <executable-name> -- arguments
```

## Usage

### Library

The primary library provided by this project is `core`. To use it in another Haskell project, add `core` (or the specific package name you intend to use if it differs) to the `build-depends` section of your `.cabal` file.

```haskell
-- Example of using a function from the 'core' library
import Core.SomeModule (someFunction) -- TODO: Update with actual module and function

main :: IO ()
main = do
  putStrLn "Using the HAIRY library:"
  print (someFunction 42) -- TODO: Update with actual function call
```

### Executables

If the HAIRY project provides any command-line executables, describe how to run them here.
```bash
# For Cabal (replace <executable-name> with actual name)
cabal run exe:<executable-name> -- --option1 value1 --option2

# For Stack (replace <executable-name> with actual name, if using Stack)
stack exec <executable-name> -- --option1 value1 --option2
```

## Running Tests

To run all test suites in the project:

### Using Cabal
```bash
cabal test all
```
This command will execute all test suites defined in the `.cabal` files of your local packages (e.g., `m31-tests` from the `core` package).

## Development

Information for developers:
- **Setting up a development environment**: Standard `cabal build all` or `stack build` should suffice.
- **Interactive Development (REPL)**:
  ```bash
  # For a specific component (e.g., the 'core' library)
  cabal repl lib:core
  # Or for a test suite
  cabal repl test:m31-tests
  ```
- **Live Reloading (GHCID)**: Install `ghcid` (`cabal install ghcid` or `stack install ghcid`) then run:
  ```bash
  ghcid --command="cabal repl lib:core" # Or your target component
  ```
- **Code Formatting**: Consider using [Ormolu](https://github.com/tweag/ormolu) or [Fourmolu](https://github.com/fourmolu/fourmolu).
  ```bash
  # Example with Ormolu
  ormolu -i . # Formats files in place
  ```
- **Linting**: [HLint](https://github.com/ndmitchell/hlint) is a popular choice.
  ```bash
  hlint .
  ```

## Contributing

Contributions are welcome! Please follow these general steps:
1. Fork the repository.
2. Create a new branch (`git checkout -b feature/your-feature-name`).
3. Make your changes.
4. Ensure tests pass (`cabal test all` or `stack test`).
5. Add tests for new features or bug fixes.
6. Format and lint your code.
7. Commit your changes (`git commit -am 'Add some feature'`).
8. Push to the branch (`git push origin feature/your-feature-name`).
9. Create a new Pull Request.

Please make sure to update tests and documentation as appropriate.

## License

This project is licensed under the MIT License. You should create a `LICENSE` file in the root of your project with the full text of the MIT license, or your chosen license.