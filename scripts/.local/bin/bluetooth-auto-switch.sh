#!/bin/bash
#
# Bluetooth Auto Audio Profile Switch
# Automatically switches to A2DP audio profile when Bluetooth audio devices connect
#

DEVICE_MAC="2C:BE:EB:D7:06:8A"  # Nothing Ear (a)
CARD_NAME="bluez_card.2C_BE_EB_D7_06_8A"

# Wait for device to be fully connected
sleep 2

# Check if device is connected
if bluetoothctl info "$DEVICE_MAC" | grep -q "Connected: yes"; then
    echo "Device connected, switching to A2DP profile..."

    # Set card profile to A2DP (high quality audio)
    pactl set-card-profile "$CARD_NAME" a2dp-sink

    # Set as default sink
    pactl set-default-sink "$CARD_NAME"

    echo "Audio profile switched successfully"
    notify-send "Bluetooth Audio" "Nothing Ear (a) connected with A2DP profile" -i audio-headphones
fi
