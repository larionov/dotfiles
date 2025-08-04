# Clean Power Management for Lenovo P14s Gen3 Intel

Simple power management setup using power-profiles-daemon defaults with Intel graphics optimization.

## What This Setup Does

- **Uses power-profiles-daemon defaults**: No custom power tweaks, relies on system defaults
- **Intel graphics optimization**: Enables power saving features (RC6, FBC, GuC, PSR)
- **Kernel thermal management**: Uses native step_wise thermal policy
- **No thermald conflicts**: Prevents conflicts with kernel thermal management

## Installation

```bash
# Run the clean power setup script
~/Sync/dotfiles/bin/power-setup-clean.sh
```

## Power Profiles

### 🔋 Power Saver (Default)
- System chooses conservative settings
- Prioritizes battery life and low temperatures

### ⚖️ Balanced  
- System default balanced profile
- Good mix of performance and efficiency

### 🚀 Performance
- System chooses aggressive performance settings
- Maximum performance when needed

## Usage

### Command Line
```bash
# Check current profile
powerprofilesctl get

# Switch profiles
powerprofilesctl set power-saver
powerprofilesctl set balanced  
powerprofilesctl set performance

# Toggle through profiles
~/Sync/dotfiles/bin/power-toggle.sh
```

### Waybar Integration
- **Visual indicator**: 🔋 ⚖️ 🚀 icons show current profile
- **Click to toggle**: Cycles through profiles

## Files

- **`bin/power-setup-clean.sh`**: Installation script
- **`bin/power-toggle.sh`**: Profile cycling script  
- **Waybar config**: Power profile indicator
- **Intel graphics config**: `/etc/modprobe.d/i915.conf`

## Monitoring

```bash
# Check temperatures
sensors

# Check power profile
powerprofilesctl get

# List available profiles
powerprofilesctl list
```

## Why This Approach

- **Simpler**: Uses system defaults instead of custom tweaks
- **Less maintenance**: No custom TLP configurations to maintain
- **Standard behavior**: Works like other Linux distributions
- **Flexible**: Easy to switch performance levels as needed

---
**Generated**: August 4, 2025  
**System**: Arch Linux on Lenovo ThinkPad P14s Gen3 Intel