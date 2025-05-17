#!/bin/bash
# Comprehensive setup script for Haskell development environment
# This script installs all necessary dependencies for development
# Run this during container initialization to ensure offline development capabilities

set -e  # Exit immediately if a command exits with a non-zero status

echo "Starting comprehensive environment setup..."

# System dependencies
echo "Installing system dependencies..."
apt-get update
apt-get install -y \
    grc \
    python3 \
    python3-pip \
    git \
    curl \
    wget \
    unzip \
    zip \
    make \
    build-essential \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libtinfo-dev \
    zlib1g-dev \
    libnuma-dev

# Install Python dependencies
echo "Installing Python dependencies..."
pip3 install -r ./requirements.txt

# Install additional Python development tools
pip3 install \
    pytest \
    pytest-cov \
    black \
    flake8 \
    isort \
    mypy

# Install GHCup and Haskell tools with specific versions
echo "Installing Haskell tools with GHCup..."
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.6.3 \
    BOOTSTRAP_HASKELL_CABAL_VERSION=latest \
    BOOTSTRAP_HASKELL_HLS_VERSION=latest \
    sh

# Source GHCup environment
source /root/.ghcup/env

# Add GHCup to shell configuration
grep -qxF 'source /root/.ghcup/env' /root/.bashrc || echo 'source /root/.ghcup/env' >> /root/.bashrc
grep -qxF 'source /workspace/.devcontainer/.bashrc' /root/.bashrc || echo 'source /workspace/.devcontainer/.bashrc' >> /root/.bashrc

# Set up grc configuration for Cabal
mkdir -p /root/.grc
cp /workspace/.devcontainer/cabal /root/.grc/cabal

# Update Cabal and install Haskell development tools
echo "Updating Cabal and installing Haskell development tools..."
cabal update
cabal install \
    hlint \
    ormolu \
    ghcid \
    cabal-fmt \
    hoogle \
    implicit-hie \
    stan

# Build documentation
echo "Building Hoogle database..."
hoogle generate

# Build all project packages
echo "Building project packages..."
cabal build all

# Run tests to verify setup
echo "Running tests to verify setup..."
cabal test all || echo "Tests failed but continuing setup"

# Pre-download common Hackage dependencies to ensure offline availability
echo "Pre-downloading common dependencies..."
cabal install --lib \
    aeson \
    text \
    bytestring \
    containers \
    mtl \
    transformers \
    vector \
    async \
    stm \
    safe \
    random \
    time \
    network \
    http-client \
    http-client-tls \
    http-types \
    QuickCheck \
    quickcheck-instances \
    hspec \
    tasty \
    tasty-hunit \
    tasty-quickcheck \
    criterion

echo "Environment setup complete!" 