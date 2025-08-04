# Advanced Power Management for Lenovo P14s Gen3 Intel

This setup combines TLP with power-profiles-daemon to provide flexible power management while maintaining excellent thermal performance.

## Features

- **Flexible Power Profiles**: Switch between power-saver, balanced, and performance modes
- **Thermal Optimization**: Conservative TLP settings prevent overheating
- **Waybar Integration**: Visual power profile indicator with click-to-toggle
- **Automatic Setup**: Single script installation

## Installation

```bash
# Run the power setup script
~/Sync/dotfiles/bin/power-setup.sh
```

## Power Profiles

### 🔋 Power Saver (Default)
- **Purpose**: Maximum cooling, longest battery life  
- **CPU**: Limited performance, no boost
- **Temperature**: ~60-65°C at idle
- **Use case**: General computing, long battery sessions

### ⚖️ Balanced  
- **Purpose**: Good performance with thermal management
- **CPU**: Moderate performance limits
- **Temperature**: ~65-70°C at idle  
- **Use case**: Most daily tasks

### 🚀 Performance
- **Purpose**: Maximum performance when needed
- **CPU**: Full performance, boost enabled
- **Temperature**: ~70-80°C under load
- **Use case**: Demanding tasks, plugged in

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
- **Tooltip**: Shows current profile name

### Keybind (add to Hyprland config)
```bash
bind = SUPER, F1, exec, ~/.local/bin/power-toggle.sh
```

## Architecture

### TLP Configuration
- **Conservative thermal settings**: Prevents overheating baseline
- **Moderate CPU limits**: Allows power-profiles-daemon to work  
- **Platform profile management**: Deferred to power-profiles-daemon

### Power Profiles Daemon
- **Dynamic profile switching**: Changes CPU governor, frequencies, boost
- **Platform profile integration**: Uses ACPI platform profiles
- **User-friendly**: Simple three-tier system

### Thermal Management
- **Kernel thermal zones**: Uses native step_wise policy
- **No thermald conflicts**: Avoided known thermal management conflicts
- **Intel graphics optimization**: RC6, FBC, GuC, PSR enabled

## Files Created

- **`bin/power-setup.sh`**: Complete installation script
- **`bin/power-toggle.sh`**: Profile cycling script  
- **Waybar config**: Power profile indicator and toggle
- **TLP config**: `/etc/tlp.d/01-thinkpad-p14s.conf`
- **Intel graphics**: `/etc/modprobe.d/i915.conf`

## Monitoring

```bash
# Check temperatures
sensors

# Monitor continuously
watch sensors

# Check power profile
powerprofilesctl get

# TLP status
sudo tlp-stat -s

# Verify thermal zones
cat /sys/class/thermal/thermal_zone*/policy
```

## Troubleshooting

### High Temperatures
1. Verify power profile: `powerprofilesctl get`
2. Switch to power-saver: `powerprofilesctl set power-saver`
3. Check processes: `top`
4. Verify thermal zones: All should show "step_wise"

### Power Profile Not Switching
1. Check service: `systemctl status power-profiles-daemon`
2. Restart service: `sudo systemctl restart power-profiles-daemon`
3. Check TLP conflicts: `sudo tlp-stat -p`

### Waybar Not Showing Profile
1. Check script path: `ls ~/.local/bin/power-toggle.sh`
2. Make executable: `chmod +x ~/.local/bin/power-toggle.sh`
3. Test manually: `~/.local/bin/power-toggle.sh`

## Comparison to Previous Setup

### Before (TLP only)
- Fixed thermal settings
- No performance flexibility  
- Manual configuration changes needed

### After (TLP + power-profiles-daemon)
- **Same thermal protection** in power-saver mode
- **Performance on demand** when needed
- **Easy switching** via GUI or commands
- **Better user experience** with visual feedback

---
**Generated**: August 4, 2025  
**System**: Arch Linux on Lenovo ThinkPad P14s Gen3 Intel