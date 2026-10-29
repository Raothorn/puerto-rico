#!/usr/bin/env bash
set -euo pipefail

# Find staged Haskell files (.hs)
mapfile -d '' files < <(
    git diff --cached --name-only --diff-filter=ACMR -z -- '*.hs'
)

# Exit early if no Haskell files are staged
if [ "${#files[@]}" -eq 0 ]; then
    exit 0
fi

echo "Formatting Haskell files with Fourmolu..."

# Run fourmolu in-place on the staged files
~/.local/share/nvim/mason/bin/fourmolu -i "${files[@]}"

# Re-stage the files so the formatting changes are committed
git add "${files[@]}"
