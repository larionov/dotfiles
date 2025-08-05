# Lenovo P14s Gen3 Intel Thermal Fix

This directory contains a thermal management fix for Lenovo ThinkPad P14s Gen3 with Intel i7-1260P processor running Arch Linux.

## Problem Solved

Fixes severe overheating issues where the laptop runs at 83°C+ at idle with fans constantly running at maximum speed.

## Solution Overview

The fix involves:
1. **TLP power management** - Caps CPU performance and enables aggressive power saving
2. **Kernel thermal management** - Uses native "step_wise" thermal policy  
3. **Intel graphics power saving** - Enables RC6, FBC, GuC, and PSR
4. **Avoiding thermald** - Prevents conflicts with kernel thermal management

## Quick Installation

```bash
# Run the automated setup script
~/Sync/dotfiles/bin/thermal-fix-p14s.sh
```

## Results

- **Temperature reduction**: 83°C → 61°C at idle (22°C improvement)
- **Fan noise**: Significantly reduced
- **Power consumption**: Lower with maintained performance
- **Battery life**: Improved due to better power management

## Files Created

- `/etc/tlp.d/01-thinkpad-p14s.conf` - TLP power management configuration
- `/etc/modprobe.d/i915.conf` - Intel graphics power saving options
- `/home/larionov/Sync/dotfiles/bin/thermal-fix-p14s.sh` - Installation script

## Manual Steps (if script fails)

### 1. Install Packages
```bash
sudo pacman -S tlp powertop --needed
```

### 2. Configure TLP
Create `/etc/tlp.d/01-thinkpad-p14s.conf` with optimized settings for thermal management.

### 3. Intel Graphics Power Saving
```bash
echo 'options i915 enable_rc6=1 enable_fbc=1 enable_guc=3 enable_psr=1' | sudo tee /etc/modprobe.d/i915.conf
```

### 4. Disable Conflicting Services
```bash
# IMPORTANT: Do not use thermald - it conflicts with kernel thermal management
sudo systemctl disable --now thermald  # if installed
sudo systemctl mask systemd-rfkill.service systemd-rfkill.socket
```

### 5. Enable TLP
```bash
sudo systemctl enable --now tlp
```

### 6. Set Thermal Zone Policies
```bash
# Set all thermal zones to use kernel's step_wise policy
for zone in /sys/class/thermal/thermal_zone*/policy; do 
    echo "step_wise" | sudo tee "$zone"
done
```

### 7. Apply Immediate Optimizations
```bash
sudo powertop --auto-tune
sudo tlp start
```

## Monitoring

```bash
# Check temperatures
sensors

# Monitor continuously  
watch sensors

# Check TLP status
sudo tlp-stat -s

# Verify thermal zone policies (should all show "step_wise")
cat /sys/class/thermal/thermal_zone*/policy
```

## Troubleshooting

### High Temperatures After Setup
1. Check thermal zone policies:
   ```bash
   cat /sys/class/thermal/thermal_zone*/policy
   ```
   All should show "step_wise"

2. Ensure thermald is not running:
   ```bash
   systemctl status thermald
   ```
   Should show "inactive (dead)"

3. Check for high CPU processes:
   ```bash
   top -b -n 1 | head -15
   ```

### Reset Thermal Policies
If thermal zones get reset to "user_space":
```bash
for zone in /sys/class/thermal/thermal_zone*/policy; do 
    echo "step_wise" | sudo tee "$zone"
done
```

## Important Notes

- **Reboot required** after initial setup for Intel graphics changes
- **Do NOT install thermald** - it conflicts with kernel thermal management
- **Firefox/Chrome tabs** can cause temporary temperature spikes
- **Solution is permanent** - settings persist across reboots

## Hardware Compatibility

This fix is specifically designed for:
- **Laptop**: Lenovo ThinkPad P14s Gen3 Intel
- **CPU**: Intel i7-1260P (12th Gen)
- **Graphics**: Intel Alder Lake-P GT2 [Iris Xe Graphics]
- **OS**: Arch Linux

May work on similar ThinkPad models with Intel 12th gen processors.

---
Generated: August 4, 2025  
Tested on: Lenovo ThinkPad P14s Gen3 Intel with Arch Linux