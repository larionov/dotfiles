#!/bin/bash
#
# Bluetooth Cleanup Script
# Removes all unpaired and untrusted Bluetooth devices
#

echo "Cleaning up Bluetooth devices..."
echo "Keeping only paired or trusted devices..."
echo ""

bluetoothctl devices | grep "Device" | awk '{print $2}' | while read mac; do
    paired=$(bluetoothctl info "$mac" 2>/dev/null | grep "Paired: yes")
    trusted=$(bluetoothctl info "$mac" 2>/dev/null | grep "Trusted: yes")
    name=$(bluetoothctl info "$mac" 2>/dev/null | grep "Name:" | cut -d: -f2 | xargs)

    if [ -z "$paired" ] && [ -z "$trusted" ]; then
        echo "Removing: $mac ($name)"
        bluetoothctl remove "$mac" 2>/dev/null
    else
        echo "Keeping:  $mac ($name)"
    fi
done

echo ""
echo "Cleanup complete!"
echo "Remaining devices:"
bluetoothctl devices
