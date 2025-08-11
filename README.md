# Dotfiles - GNU Stow Edition

A clean, modern dotfiles management system using GNU Stow for cross-platform configuration management.

## Structure

Each configuration is organized as a "Stow package" - a directory containing files in the same structure as they would appear in your home directory:

```
dotfiles-stow/
├── bash/                    # Shell configuration
│   └── .bashrc
├── kitty/                   # Terminal emulator
│   └── .config/kitty/kitty.conf
├── emacs/                   # Emacs editor
│   └── .emacs.d/
│       ├── *.el             # Configuration files
│       └── packages/        # Custom packages
├── hyprland/               # Wayland compositor (Linux)
│   └── .config/hypr/hyprland.conf
├── waybar/                 # Status bar (Linux)
│   └── .config/waybar/
├── dunst/                  # Notifications (Linux)
│   └── .config/dunst/dunstrc
├── scripts/                # User scripts
│   └── .local/bin/
└── install.sh              # Installation script
```

## Installation

### Prerequisites

GNU Stow is required and will be automatically installed if missing.

### Quick Start

```bash
cd ~/Sync/dotfiles-stow
./install.sh
```

### Commands

```bash
./install.sh                # Install all packages
./install.sh uninstall      # Remove all symlinks
./install.sh restow         # Remove and reinstall (useful after updates)
./install.sh update         # Update git-based packages
./install.sh help           # Show usage information
```

### Environment Variables

```bash
INSTALL_IOSEVKA=true ./install.sh    # Install optional Iosevka font
```

## How It Works

1. **GNU Stow** creates symbolic links from each package to your home directory
2. **Dependencies** are automatically installed (stow, fonts, system packages)
3. **Backups** are created for existing configurations before linking
4. **Platform detection** installs appropriate packages (Linux/macOS)

## Package Management

### Installing Individual Packages

```bash
cd ~/Sync/dotfiles-stow
stow bash        # Install just bash configuration
stow kitty       # Install just kitty configuration
stow emacs       # Install just Emacs configuration
```

### Removing Individual Packages

```bash
stow -D bash     # Remove bash configuration links
stow -D emacs    # Remove Emacs configuration links
```

### Platform-Specific Packages

- **Universal**: bash, kitty, emacs, scripts
- **Linux Only**: hyprland, waybar, dunst  
- **macOS Only**: (none currently)

## Features

### Automatic Package Installation

The installer automatically detects your platform and installs required packages:

- **Arch Linux**: `stow`, `dunst`, `libnotify`, `wofi`, `ripgrep`
- **macOS**: `stow` (via Homebrew), `ripgrep`
- **Other Linux**: `stow`, `ripgrep`

### Font Management

Automatically downloads and installs:

- **FiraMono Nerd Font** (default terminal font)
- **JetBrains Mono Nerd Font** (Linux only, for Waybar icons)
- **Iosevka** (optional, set `INSTALL_IOSEVKA=true`)

### Emacs Integration

- Uses [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) as base
- Git packages (macrursors) are automatically cloned and updated
- Custom configuration files are stowed on top

### Backup System

Before installing, existing configurations are backed up to timestamped directories like `~/.dotfiles_backup_20250808_143022/`.

## Supported Platforms

- **Arch Linux** (full support: Hyprland, Waybar, Dunst)
- **macOS** (basic support: terminal, shell, Emacs)
- **Other Linux** (basic support: terminal, shell, Emacs)

## Migration from Old System

If you're migrating from a previous dotfiles system:

1. **Backup** your current configs: `cp -r ~/.config ~/.config.backup`
2. **Run installer**: `./install.sh`
3. **Test configurations** and verify everything works
4. **Remove backups** when satisfied

## Customization

### Adding New Packages

1. Create a new directory: `mkdir mypackage`
2. Add files with proper structure: `mypackage/.config/myapp/config`
3. Install: `stow mypackage`

### Modifying Existing Packages

1. Edit files in the package directory
2. Restow: `stow -R packagename` or `./install.sh restow`

### Ignoring Files

Create `.stow-local-ignore` in any package to exclude files:

```
\.git
README.md
*.tmp
```

## Troubleshooting

### Conflicts

If Stow reports conflicts (existing files/directories):

1. **Check what exists**: `ls -la ~/.config/conflicting-app`
2. **Backup if needed**: `mv ~/.config/app ~/.config/app.backup`
3. **Restow**: `stow -R packagename`

### Missing Dependencies

If packages fail to install:

1. **Check your package manager** is working
2. **Install manually**: `sudo pacman -S stow` (Arch) or `brew install stow` (macOS)
3. **Re-run installer**: `./install.sh`

### Permission Issues

Scripts need execute permissions:

```bash
chmod +x ~/.local/bin/*
```

## Philosophy

This system follows the Unix philosophy of "do one thing well":

- **GNU Stow** handles symlink management
- **Install script** handles dependencies and setup
- **Package structure** mirrors home directory layout
- **Version control** tracks all configurations

Clean, simple, and maintainable.