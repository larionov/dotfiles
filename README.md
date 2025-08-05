# Cross-Platform Dotfiles

Modern cross-platform dotfiles configuration supporting macOS and Linux with automatic OS detection and platform-specific installations.

## Supported Platforms

### ✅ Full Support
- **macOS**: Emacs, Kitty, Shell configuration
- **Arch Linux**: Emacs, Kitty, Shell, Hyprland, Waybar, thermal management

### ⚡ Basic Support  
- **Other Linux distros**: Emacs, Kitty, Shell configuration

## Directory Structure

```
dotfiles/
├── common/                 # Cross-platform configurations
│   ├── config/
│   │   ├── emacs/         # Emacs configuration (all platforms)
│   │   └── kitty/         # Kitty terminal (all platforms)
│   ├── shell/
│   │   └── bashrc         # Bash configuration
│   └── scripts/           # Common scripts
├── linux/
│   └── arch/              # Arch Linux specific
│       ├── config/
│       │   ├── hypr/      # Hyprland window manager
│       │   └── waybar/    # Status bar
│       └── scripts/       # Arch-specific scripts
├── macos/                 # macOS specific configurations
│   ├── config/            # macOS app configs
│   └── scripts/           # macOS-specific scripts
└── bin/                   # Universal binaries/scripts
```

## Quick Start

```bash
# Clone the repository
git clone https://github.com/larionov/dotfiles.git
cd dotfiles

# Install for your platform (auto-detected)
./install.sh

# Update git-based packages
./install.sh update

# Get help
./install.sh help
```

## Features

### 🎯 Smart Platform Detection
- Automatically detects macOS, Arch Linux, Debian/Ubuntu, Fedora
- Only installs relevant configurations for your platform
- Platform-specific package manager integration

### 📦 Flexible Package Management
- **Git packages**: Fresh from upstream (e.g., macrursors)
- **Static packages**: Versioned in dotfiles (e.g., dired+)
- Easy to add new packages via configuration array

### 🔄 Cross-Platform Font Management
- **macOS**: `~/Library/Fonts`
- **Linux**: `~/.local/share/fonts` with font cache refresh
- Automatic Iosevka font installation
- Linux-only JetBrains Mono Nerd Font for Waybar

### ⚙️ Intelligent Configuration Linking
- Backs up existing configurations
- Creates symlinks for easy maintenance
- Platform-specific exclusions (no Hyprland on macOS)

## Configuration

### Adding Git Packages
Edit the `GIT_PACKAGES` array in `install.sh`:

```bash
declare -A GIT_PACKAGES=(
    ["macrursors"]="https://github.com/corytertel/macrursors"
    ["your-package"]="https://github.com/user/repo"
)
```

### Platform-Specific Configs
- Add macOS configs to `macos/config/`
- Add Linux configs to `linux/arch/config/` 
- Common configs go in `common/config/`

## Emacs Configuration

Uses [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) as base with:
- Modern completion (Vertico, Consult, Orderless)
- Built-in `helm-do-grep-ag` with ripgrep backend
- In-place search result editing with wgrep
- Multi-cursor support via macrursors
- Cross-platform compatibility

### Search & Edit Workflow
1. `C-c f` - Search with helm-do-grep-ag
2. `C-x C-s` - Save results to buffer
3. `C-c C-p` - Make buffer editable (wgrep)
4. Make changes directly in results
5. `C-c C-e` - Apply changes to files

## Dependencies

### Automatically Installed
- `unzip`, `wget`/`curl`, `git`
- `ripgrep` (for fast searching)
- Platform-specific packages via package managers

### Required
- **macOS**: Homebrew
- **Arch Linux**: pacman (standard)
- **Other Linux**: apt/dnf support

## License

MIT License - See individual package licenses for third-party components.