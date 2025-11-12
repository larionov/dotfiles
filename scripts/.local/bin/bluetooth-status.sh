#!/bin/bash
#
# Bluetooth Status Script for Waybar
# Shows bluetooth status and connected devices
#

# Check if bluetooth is powered on
if ! bluetoothctl show | grep -q "Powered: yes"; then
    echo '{"text": "", "tooltip": "Bluetooth Off", "class": "off"}'
    exit 0
fi

# Get connected devices
connected_devices=$(bluetoothctl devices Connected | cut -d' ' -f3-)
device_count=$(echo "$connected_devices" | grep -c .)

if [ -z "$connected_devices" ] || [ "$device_count" -eq 0 ]; then
    echo '{"text": "", "tooltip": "Bluetooth On\nNo devices connected", "class": "on"}'
else
    # Build tooltip with all connected devices
    tooltip="Bluetooth On\nConnected devices:\n"
    while IFS= read -r device; do
        tooltip="${tooltip}• ${device}\n"
    done <<< "$connected_devices"

    # Show count and icon
    if [ "$device_count" -eq 1 ]; then
        text=" $connected_devices"
    else
        text=" ($device_count)"
    fi

    echo "{\"text\": \"$text\", \"tooltip\": \"$tooltip\", \"class\": \"connected\"}"
fi
