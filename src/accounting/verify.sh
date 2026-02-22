#!/bin/bash

# Verification Script for Node.js Application
# This script installs dependencies, runs tests, and verifies the application

cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting

echo "=== Installing Dependencies ==="
npm install

echo ""
echo "=== Running Test Suite ==="
npm test

echo ""
echo "=== Verification Complete ==="
