#!/bin/bash
# Script to set up and build the devcontainer

set -e  # Exit immediately if a command exits with a non-zero status

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Setting up devcontainer for the Haskell project...${NC}"

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo -e "${RED}Docker is not installed. Please install Docker first.${NC}"
    echo -e "Visit https://docs.docker.com/get-docker/ for installation instructions."
    exit 1
fi

# Check if VS Code is installed (optional)
if ! command -v code &> /dev/null; then
    echo -e "${YELLOW}VS Code not found. While not required, VS Code is recommended for devcontainer usage.${NC}"
    echo -e "You can download it from https://code.visualstudio.com/"
fi

# Check if the VS Code Remote - Containers extension is installed (optional)
if command -v code &> /dev/null; then
    if ! code --list-extensions | grep -q "ms-vscode-remote.remote-containers"; then
        echo -e "${YELLOW}VS Code Remote - Containers extension not found.${NC}"
        echo -e "Installing VS Code Remote - Containers extension..."
        code --install-extension ms-vscode-remote.remote-containers
    fi
fi

# Make sure we're in the repository root
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || echo ".")
cd "$REPO_ROOT"

# Check if .devcontainer directory exists
if [ ! -d ".devcontainer" ]; then
    echo -e "${RED}The .devcontainer directory doesn't exist in this repository.${NC}"
    exit 1
fi

# Check if required files exist in .devcontainer
if [ ! -f ".devcontainer/devcontainer.json" ]; then
    echo -e "${RED}The .devcontainer/devcontainer.json file doesn't exist.${NC}"
    exit 1
fi

echo -e "${GREEN}All prerequisites are satisfied!${NC}"

echo -e "${YELLOW}Note: This project uses a comprehensive container-setup.sh script which will automatically run when the devcontainer is created.${NC}"
echo -e "${YELLOW}This script installs all necessary dependencies for offline development.${NC}"

# Pull the base image from the devcontainer.json
BASE_IMAGE=$(grep -o '"image": "[^"]*' .devcontainer/devcontainer.json | cut -d'"' -f4)
if [ -n "$BASE_IMAGE" ]; then
    echo -e "${YELLOW}Pulling base image: $BASE_IMAGE${NC}"
    docker pull $BASE_IMAGE
else
    echo -e "${YELLOW}No base image found in devcontainer.json${NC}"
fi

# Provide instructions for different ways to open the devcontainer
echo -e "\n${YELLOW}To open the devcontainer, you can:${NC}"
echo -e "1. In VS Code: Press F1, then select 'Remote-Containers: Open Folder in Container...'"
echo -e "2. Run: code --remote-containers ."

# For direct Docker usage
echo -e "\n${GREEN}To start the container directly with Docker:${NC}"
if [ -n "$BASE_IMAGE" ]; then
    echo -e "docker run -it --rm -v $(pwd):/workspace $BASE_IMAGE bash"
    echo -e "Note: This won't run the devcontainer's post-create commands automatically."
fi

# Offer to open in VS Code if available
if command -v code &> /dev/null; then
    echo -e "\n${YELLOW}Would you like to open the project in VS Code with devcontainer now? (y/n)${NC}"
    read -r response
    if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
        code --remote-containers .
    fi
fi

echo -e "\n${GREEN}Setup complete!${NC}" 