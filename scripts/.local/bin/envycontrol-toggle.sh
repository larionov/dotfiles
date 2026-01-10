#!/bin/bash
#
# EnvyControl GPU Mode Toggle Script
# Cycles through GPU modes: integrated -> hybrid -> nvidia
# Requires reboot to apply changes
#

set -euo pipefail

# Check if envycontrol is installed
if ! command -v envycontrol &> /dev/null; then
    notify-send "EnvyControl Error" "envycontrol is not installed" -i error -u critical
    exit 1
fi

# Get current GPU mode
current=$(envycontrol --query 2>/dev/null || echo "unknown")

case "$current" in
    "integrated")
        sudo envycontrol -s hybrid --force
        notify-send "GPU Mode" "Switching to Hybrid mode\nReboot required" -i video-display -u normal
        ;;
    "hybrid")
        sudo envycontrol -s nvidia --force
        notify-send "GPU Mode" "Switching to NVIDIA mode\nReboot required" -i video-display -u normal
        ;;
    "nvidia")
        sudo envycontrol -s integrated --force
        notify-send "GPU Mode" "Switching to Integrated mode\nReboot required" -i video-display -u normal
        ;;
    *)
        # Default to hybrid if unknown
        sudo envycontrol -s hybrid --force
        notify-send "GPU Mode" "Setting to Hybrid mode\nReboot required" -i video-display -u normal
        ;;
esac
