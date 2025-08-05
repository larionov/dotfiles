#!/bin/bash

# Cross-platform dotfiles installation script
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKUP_DIR="$HOME/.dotfiles_backup_$(date +%Y%m%d_%H%M%S)"
CONFIG_DIR="$HOME/.config"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
print_warning() { echo -e "${YELLOW}[WARN]${NC} $1"; }
print_debug() { echo -e "${BLUE}[DEBUG]${NC} $1"; }

ask_confirmation() {
    read -p "$(echo -e "${YELLOW}$1${NC} (y/N): ")" -n 1 -r
    echo
    [[ $REPLY =~ ^[Yy]$ ]]
}

# OS Detection
detect_os() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        if command -v pacman >/dev/null 2>&1; then
            echo "linux/arch"
        elif command -v apt >/dev/null 2>&1; then
            echo "linux/debian"
        elif command -v dnf >/dev/null 2>&1; then
            echo "linux/fedora"
        else
            echo "linux/generic"
        fi
    else
        echo "unknown"
    fi
}

# Platform-specific package managers
install_packages() {
    local packages=("$@")
    local os=$(detect_os)
    
    if [[ ${#packages[@]} -eq 0 ]]; then
        return 0
    fi
    
    print_info "Installing packages: ${packages[*]}"
    
    case "$os" in
        "macos")
            if command -v brew >/dev/null 2>&1; then
                brew install "${packages[@]}"
            else
                print_warning "Homebrew not found. Please install: ${packages[*]}"
                return 1
            fi
            ;;
        "linux/arch")
            sudo pacman -S --noconfirm "${packages[@]}"
            ;;
        "linux/debian")
            sudo apt update && sudo apt install -y "${packages[@]}"
            ;;
        "linux/fedora")
            sudo dnf install -y "${packages[@]}"
            ;;
        *)
            print_warning "Unknown OS. Please install manually: ${packages[*]}"
            return 1
            ;;
    esac
}

# Define git-based packages globally (name:repo_url)
declare -A GIT_PACKAGES=(
    ["macrursors"]="https://github.com/corytertel/macrursors"
    # Add more git packages here as needed:
    # ["some-package"]="https://github.com/user/repo"
)

backup_if_exists() {
    local target="$1"
    if [[ -e "$target" && ! -L "$target" ]]; then
        print_info "Backing up existing $target"
        mkdir -p "$BACKUP_DIR"
        cp -r "$target" "$BACKUP_DIR/"
    fi
}

create_symlink() {
    local source="$1"
    local target="$2"
    
    if [[ -L "$target" && "$(readlink "$target")" == "$source" ]]; then
        print_debug "✓ $target already linked correctly"
        return 0
    fi
    
    if [[ -e "$target" ]]; then
        if ask_confirmation "Replace existing $target?"; then
            backup_if_exists "$target"
            rm -rf "$target"
        else
            print_warning "Skipping $target"
            return 0
        fi
    fi
    
    mkdir -p "$(dirname "$target")"
    ln -sf "$source" "$target"
    print_info "✓ Linked $target → $source"
}

check_dependencies() {
    local os=$(detect_os)
    local missing_packages=()
    
    # Common dependencies
    if ! command -v unzip >/dev/null 2>&1; then
        missing_packages+=("unzip")
    fi
    
    if ! command -v wget >/dev/null 2>&1 && ! command -v curl >/dev/null 2>&1; then
        if [[ "$os" == "macos" ]]; then
            missing_packages+=("wget")
        else
            missing_packages+=("wget")
        fi
    fi
    
    # Ripgrep for helm search
    if ! command -v rg >/dev/null 2>&1; then
        missing_packages+=("ripgrep")
    fi
    
    # Git (should be available but check anyway)
    if ! command -v git >/dev/null 2>&1; then
        missing_packages+=("git")
    fi
    
    # Platform-specific dependencies
    case "$os" in
        "linux/arch")
            # SSH askpass for Wayland (skip on X11)
            if [[ -n "${WAYLAND_DISPLAY:-}" ]] && ! command -v ssh-askpass >/dev/null 2>&1; then
                # No need for ssh-askpass on Wayland, skip
                :
            fi
            ;;
        "macos")
            # macOS-specific checks could go here
            ;;
    esac
    
    if [[ ${#missing_packages[@]} -gt 0 ]]; then
        install_packages "${missing_packages[@]}"
        print_info "✓ Dependencies installed"
    fi
    
    return 0
}

install_fonts() {
    local os=$(detect_os)
    local fonts_dir
    
    case "$os" in
        "macos")
            fonts_dir="$HOME/Library/Fonts"
            ;;
        *)
            fonts_dir="$HOME/.local/share/fonts"
            ;;
    esac
    
    mkdir -p "$fonts_dir"
    
    # Install Iosevka font
    if ! fc-list | grep -i "iosevka" > /dev/null 2>&1; then
        print_info "Installing Iosevka font..."
        
        local temp_dir=$(mktemp -d)
        local iosevka_url="https://github.com/be5invis/Iosevka/releases/download/v31.8.0/PkgTTC-Iosevka-31.8.0.zip"
        
        if command -v wget >/dev/null 2>&1; then
            wget -O "$temp_dir/iosevka.zip" "$iosevka_url"
        else
            curl -L -o "$temp_dir/iosevka.zip" "$iosevka_url"
        fi
        
        unzip -q "$temp_dir/iosevka.zip" -d "$temp_dir"
        cp "$temp_dir"/*.ttc "$fonts_dir/" 2>/dev/null || true
        
        if [[ "$os" != "macos" ]]; then
            fc-cache -fv
        fi
        print_info "✓ Iosevka font installed"
        
        rm -rf "$temp_dir"
    else
        print_debug "✓ Iosevka font already installed"
    fi
    
    # Install JetBrains Mono Nerd Font for waybar icons (Linux only)
    if [[ "$os" != "macos" ]] && ! fc-list | grep -i "jetbrainsmono nerd font" > /dev/null 2>&1; then
        print_info "Installing JetBrains Mono Nerd Font..."
        
        local temp_dir=$(mktemp -d)
        local nerd_font_url="https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/JetBrainsMono.zip"
        
        if command -v wget >/dev/null 2>&1; then
            wget -O "$temp_dir/jetbrains-nerd.zip" "$nerd_font_url"
        else
            curl -L -o "$temp_dir/jetbrains-nerd.zip" "$nerd_font_url"
        fi
        
        unzip -q "$temp_dir/jetbrains-nerd.zip" -d "$temp_dir"
        cp "$temp_dir"/*.ttf "$fonts_dir/" 2>/dev/null || true
        fc-cache -fv
        print_info "✓ JetBrains Mono Nerd Font installed"
        
        rm -rf "$temp_dir"
    elif [[ "$os" == "macos" ]]; then
        print_debug "✓ Skipping JetBrains Mono Nerd Font on macOS (not needed)"
    else
        print_debug "✓ JetBrains Mono Nerd Font already installed"
    fi
}

install_emacs() {
    local emacs_dir="$HOME/.emacs.d"
    
    # Install minimal-emacs.d if it doesn't exist
    if [[ ! -d "$emacs_dir" ]]; then
        print_info "Installing minimal-emacs.d base configuration..."
        git clone https://github.com/jamescherti/minimal-emacs.d "$emacs_dir"
        print_info "✓ minimal-emacs.d installed"
    elif [[ ! -f "$emacs_dir/init.el" ]]; then
        print_warning "Emacs directory exists but missing init.el. Backing up and reinstalling..."
        local backup_dir="$HOME/.emacs.d.backup_$(date +%Y%m%d_%H%M%S)"
        mv "$emacs_dir" "$backup_dir"
        print_info "Backup saved to: $backup_dir"
        git clone https://github.com/jamescherti/minimal-emacs.d "$emacs_dir"
        print_info "✓ minimal-emacs.d installed"
    fi
    
    # Emacs config files to link
    declare -a EMACS_FILES=(
        "pre-early-init.el"
        "post-early-init.el" 
        "pre-init.el"
        "post-init.el"
        "local.el"
    )
    
    # Link individual emacs config files
    for file in "${EMACS_FILES[@]}"; do
        if [[ -f "$DOTFILES_DIR/common/config/emacs/$file" ]]; then
            create_symlink "$DOTFILES_DIR/common/config/emacs/$file" "$emacs_dir/$file"
        fi
    done
    
    # Create packages directory
    mkdir -p "$emacs_dir/packages"
    
    # Install git-based packages
    for package_name in "${!GIT_PACKAGES[@]}"; do
        local package_dir="$emacs_dir/packages/$package_name"
        local repo_url="${GIT_PACKAGES[$package_name]}"
        
        if [[ ! -d "$package_dir/.git" ]]; then
            print_info "Installing $package_name package from git..."
            rm -rf "$package_dir"  # Remove any existing copy
            git clone "$repo_url" "$package_dir"
            print_info "✓ $package_name package installed from git"
        else
            print_debug "✓ $package_name package already installed from git"
        fi
    done
    
    # Install static packages from dotfiles (skip git-managed ones)
    if [[ -d "$DOTFILES_DIR/common/config/emacs/packages" ]]; then
        for package in "$DOTFILES_DIR/common/config/emacs/packages"/*; do
            if [[ -d "$package" ]]; then
                local package_name=$(basename "$package")
                # Skip packages that are managed via git
                if [[ ! -v "GIT_PACKAGES[$package_name]" ]]; then
                    create_symlink "$package" "$emacs_dir/packages/$package_name"
                fi
            fi
        done
    fi
}

install_platform_configs() {
    local os=$(detect_os)
    local platform_dir="$DOTFILES_DIR/$os"
    
    print_info "Installing configurations for: $os"
    
    # Install platform-specific configs
    if [[ -d "$platform_dir/config" ]]; then
        for config in "$platform_dir/config"/*; do
            if [[ -d "$config" ]]; then
                local config_name=$(basename "$config")
                create_symlink "$config" "$CONFIG_DIR/$config_name"
            fi
        done
    fi
    
    # Install platform-specific scripts
    if [[ -d "$platform_dir/scripts" ]]; then
        mkdir -p "$HOME/.local/bin"
        for script in "$platform_dir/scripts"/*; do
            if [[ -f "$script" ]]; then
                create_symlink "$script" "$HOME/.local/bin/$(basename "$script")"
            fi
        done
    fi
}

update_git_packages() {
    local emacs_dir="$HOME/.emacs.d"
    
    print_info "Updating git-based Emacs packages..."
    
    for package_name in "${!GIT_PACKAGES[@]}"; do
        local package_dir="$emacs_dir/packages/$package_name"
        
        if [[ -d "$package_dir/.git" ]]; then
            print_info "Updating $package_name..."
            (cd "$package_dir" && git pull origin main 2>/dev/null || git pull origin master 2>/dev/null)
            print_info "✓ $package_name updated"
        else
            print_warning "$package_name not found or not a git repository"
        fi
    done
}

main() {
    local os=$(detect_os)
    print_info "Installing dotfiles on: $os"
    
    # Handle update command
    if [[ "${1:-}" == "update" ]]; then
        update_git_packages
        exit 0
    fi
    
    # Check dependencies first
    if ! check_dependencies; then
        exit 1
    fi
    
    mkdir -p "$CONFIG_DIR"
    
    # Install common shell configs
    if [[ -f "$DOTFILES_DIR/common/shell/bashrc" ]]; then
        create_symlink "$DOTFILES_DIR/common/shell/bashrc" "$HOME/.bashrc"
    fi
    
    # Install common config directories (emacs, kitty)
    if [[ -d "$DOTFILES_DIR/common/config" ]]; then
        for config in "$DOTFILES_DIR/common/config"/*; do
            if [[ -d "$config" ]]; then
                local config_name=$(basename "$config")
                case "$config_name" in
                    "emacs")
                        # Handle emacs separately
                        ;;
                    "kitty")
                        create_symlink "$config" "$CONFIG_DIR/$config_name"
                        ;;
                    *)
                        create_symlink "$config" "$CONFIG_DIR/$config_name"
                        ;;
                esac
            fi
        done
    fi
    
    # Install bin files
    if [[ -d "$DOTFILES_DIR/bin" ]]; then
        mkdir -p "$HOME/.local/bin"
        for bin_file in "$DOTFILES_DIR/bin"/*; do
            if [[ -f "$bin_file" ]]; then
                create_symlink "$bin_file" "$HOME/.local/bin/$(basename "$bin_file")"
            fi
        done
    fi
    
    # Install fonts
    install_fonts
    
    # Install Emacs configuration
    if [[ -d "$DOTFILES_DIR/common/config/emacs" ]]; then
        install_emacs
    fi
    
    # Install platform-specific configurations
    install_platform_configs
    
    print_info "Installation complete!"
    if [[ -d "$BACKUP_DIR" ]]; then
        print_info "Backups saved to: $BACKUP_DIR"
    fi
    
    # Platform-specific completion messages
    case "$os" in
        "linux/arch")
            print_info "Reload Hyprland with: hyprctl reload"
            ;;
        "macos")
            print_info "Restart Terminal or run: source ~/.bashrc"
            ;;
    esac
    
    print_info "Update git packages with: $0 update"
}

show_usage() {
    echo "Cross-platform dotfiles installer"
    echo "Usage: $0 [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  install     Install dotfiles (default)"
    echo "  update      Update git-based packages"
    echo ""
    echo "Supported platforms:"
    echo "  - macOS (emacs, kitty, shell)"
    echo "  - Arch Linux (emacs, kitty, shell, hyprland, waybar)"
    echo "  - Other Linux distros (basic support)"
    echo ""
    echo "Examples:"
    echo "  $0          # Install dotfiles for current platform"
    echo "  $0 update   # Update git packages"
}

# Handle help and unknown commands
case "${1:-install}" in
    "install"|"")
        main "$@"
        ;;
    "update")
        main "$@"
        ;;
    "help"|"-h"|"--help")
        show_usage
        ;;
    *)
        print_warning "Unknown command: $1"
        show_usage
        exit 1
        ;;
esac