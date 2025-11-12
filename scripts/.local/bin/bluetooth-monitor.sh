#!/bin/bash
#
# Bluetooth Connection Monitor
# Watches for Bluetooth connections and auto-switches to A2DP profile
#

set -euo pipefail

LOCK_FILE="/tmp/.bluetooth-monitor.lock"
RETRY_DELAY=5  # Increased from 3 to 5 seconds (more reliable)

# Cleanup on exit
trap 'rm -f "$LOCK_FILE"' EXIT

# Check for lock file to prevent multiple instances
if [ -e "$LOCK_FILE" ]; then
    echo "Another instance is already running (PID: $(cat "$LOCK_FILE" 2>/dev/null || echo 'unknown'))"
    exit 0
fi

# Create lock file
echo $$ > "$LOCK_FILE"

echo "Monitoring Bluetooth audio device connections..."

# Function to check if device should be switched to A2DP
should_switch_to_a2dp() {
    local mac="$1"
    local card_name="$2"

    # Check if device is paired and trusted
    local info=$(bluetoothctl info "$mac" 2>/dev/null)
    if ! echo "$info" | grep -q "Paired: yes"; then
        echo "Device not paired, skipping"
        return 1
    fi

    if ! echo "$info" | grep -q "Trusted: yes"; then
        echo "Device not trusted, skipping"
        return 1
    fi

    if echo "$info" | grep -q "Blocked: yes"; then
        echo "Device is blocked, skipping"
        return 1
    fi

    # Check if device has audio sink UUID
    if ! echo "$info" | grep -qi "Audio Sink"; then
        echo "Device is not an audio sink, skipping"
        return 1
    fi

    # Check if A2DP is already active
    local card_info=$(pactl list cards 2>/dev/null | grep -A 50 "$card_name" || echo "")
    if echo "$card_info" | grep -q "Active Profile:.*a2dp"; then
        echo "A2DP already active, no need to switch"
        return 1
    fi

    return 0
}

# Function to switch device to A2DP
switch_to_a2dp() {
    local mac="$1"
    local card_name="$2"
    local device_name=$(bluetoothctl info "$mac" 2>/dev/null | grep "Name:" | cut -d: -f2 | xargs)

    echo "$(date '+%Y-%m-%d %H:%M:%S'): $device_name connected, switching to A2DP..."

    # Wait for PipeWire to fully detect the device
    sleep "$RETRY_DELAY"

    # Verify card exists in PipeWire
    if ! pactl list cards short | grep -q "$card_name"; then
        echo "$(date '+%Y-%m-%d %H:%M:%S'): Card $card_name not found in PipeWire yet"
        return 1
    fi

    # Try to switch to A2DP profile
    if pactl set-card-profile "$card_name" a2dp-sink 2>/dev/null; then
        # Set as default sink
        pactl set-default-sink "$card_name" 2>/dev/null || true

        echo "$(date '+%Y-%m-%d %H:%M:%S'): Successfully switched $device_name to A2DP"
        notify-send "Bluetooth Audio" "$device_name connected with A2DP profile" -i audio-headphones 2>/dev/null || true
        return 0
    else
        echo "$(date '+%Y-%m-%d %H:%M:%S'): Failed to switch $device_name to A2DP"
        return 1
    fi
}

# Monitor ALL Bluetooth devices using dbus
dbus-monitor --system "type='signal',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged',path_namespace='/org/bluez/hci0'" 2>/dev/null | \
while read -r line; do
    # Extract device path from signals
    if echo "$line" | grep -q "path=/org/bluez/hci0/dev_"; then
        device_path=$(echo "$line" | grep -o '/org/bluez/hci0/dev_[A-F0-9_]*')
        current_device="$device_path"
        continue
    fi

    # Check for Connected property change to true
    if echo "$line" | grep -q "Connected.*true" && [ -n "${current_device:-}" ]; then
        # Extract MAC address from device path
        mac=$(echo "$current_device" | sed 's|/org/bluez/hci0/dev_||' | tr '_' ':')
        card_name="bluez_card.$(echo "$current_device" | sed 's|/org/bluez/hci0/dev_||')"

        # Check if we should switch this device
        if should_switch_to_a2dp "$mac" "$card_name"; then
            switch_to_a2dp "$mac" "$card_name" || true
        fi

        current_device=""
    fi
done
