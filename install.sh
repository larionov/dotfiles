#!/bin/bash

# Minimal dotfiles installation script
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKUP_DIR="$HOME/.dotfiles_backup_$(date +%Y%m%d_%H%M%S)"
CONFIG_DIR="$HOME/.config"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

print_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
print_warning() { echo -e "${YELLOW}[WARN]${NC} $1"; }

ask_confirmation() {
    read -p "$(echo -e "${YELLOW}$1${NC} (y/N): ")" -n 1 -r
    echo
    [[ $REPLY =~ ^[Yy]$ ]]
}

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
        print_info "✓ $target already linked correctly"
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
    local missing_packages=()
    
    # Check for required packages
    if ! command -v unzip >/dev/null 2>&1; then
        missing_packages+=("unzip")
    fi
    
    if ! command -v wget >/dev/null 2>&1 && ! command -v curl >/dev/null 2>&1; then
        missing_packages+=("wget")
    fi
    
    # Check for ripgrep for helm search
    if ! command -v rg >/dev/null 2>&1; then
        missing_packages+=("ripgrep")
    fi
    
    # Check for ssh-askpass for git operations
    if ! command -v ssh-askpass >/dev/null 2>&1; then
        missing_packages+=("openssh-askpass")
    fi
    
    if [[ ${#missing_packages[@]} -gt 0 ]]; then
        print_info "Installing missing packages: ${missing_packages[*]}"
        
        # Detect package manager and install
        if command -v pacman >/dev/null 2>&1; then
            sudo pacman -S --noconfirm "${missing_packages[@]}"
        elif command -v apt >/dev/null 2>&1; then
            sudo apt update && sudo apt install -y "${missing_packages[@]}"
        elif command -v dnf >/dev/null 2>&1; then
            sudo dnf install -y "${missing_packages[@]}"
        else
            print_warning "Unknown package manager. Please install manually: ${missing_packages[*]}"
            return 1
        fi
        
        print_info "✓ Dependencies installed"
    fi
    
    return 0
}

install_fonts() {
    local fonts_dir="$HOME/.local/share/fonts"
    mkdir -p "$fonts_dir"
    
    # Install Iosevka font
    if ! fc-list | grep -i "iosevka" > /dev/null; then
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
        fc-cache -fv
        print_info "✓ Iosevka font installed"
        
        rm -rf "$temp_dir"
    else
        print_info "✓ Iosevka font already installed"
    fi
    
    # Install JetBrains Mono Nerd Font for waybar icons
    if ! fc-list | grep -i "jetbrainsmono nerd font" > /dev/null; then
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
    else
        print_info "✓ JetBrains Mono Nerd Font already installed"
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
        if [[ -f "$DOTFILES_DIR/emacs/$file" ]]; then
            create_symlink "$DOTFILES_DIR/emacs/$file" "$emacs_dir/$file"
        fi
    done
    
    # Create packages directory and handle packages individually
    mkdir -p "$emacs_dir/packages"
    
    # Install static packages from dotfiles (like dired+)
    if [[ -d "$DOTFILES_DIR/emacs/packages" ]]; then
        for package in "$DOTFILES_DIR/emacs/packages"/*; do
            if [[ -d "$package" ]]; then
                local package_name=$(basename "$package")
                # Skip packages that should be cloned from git
                if [[ "$package_name" != "macrursors" ]]; then
                    create_symlink "$package" "$emacs_dir/packages/$package_name"
                fi
            fi
        done
    fi
    
    # Clone macrursors package from git (for updates)
    local macrursors_dir="$emacs_dir/packages/macrursors"
    if [[ ! -d "$macrursors_dir/.git" ]]; then
        print_info "Installing macrursors package from git..."
        rm -rf "$macrursors_dir"  # Remove any existing copy
        git clone https://github.com/corytertel/macrursors "$macrursors_dir"
        print_info "✓ macrursors package installed from git"
    else
        print_info "✓ macrursors package already installed from git"
    fi
}

main() {
    print_info "Installing dotfiles..."
    
    # Check dependencies first
    if ! check_dependencies; then
        exit 1
    fi
    
    mkdir -p "$CONFIG_DIR"
    
    # Link config directories
    declare -a CONFIG_DIRS=(
        "hypr:$CONFIG_DIR/hypr"
        "waybar:$CONFIG_DIR/waybar"
        "kitty:$CONFIG_DIR/kitty"
    )
    
    # Link home files
    declare -a HOME_FILES=(
        "bashrc:$HOME/.bashrc"
    )
    
    for item in "${CONFIG_DIRS[@]}"; do
        IFS=':' read -r source target <<< "$item"
        if [[ -d "$DOTFILES_DIR/$source" ]]; then
            create_symlink "$DOTFILES_DIR/$source" "$target"
        fi
    done
    
    # Link home files
    for item in "${HOME_FILES[@]}"; do
        IFS=':' read -r source target <<< "$item"
        if [[ -f "$DOTFILES_DIR/$source" ]]; then
            create_symlink "$DOTFILES_DIR/$source" "$target"
        fi
    done
    
    # Link bin files
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
    if [[ -d "$DOTFILES_DIR/emacs" ]]; then
        install_emacs
    fi
    
    print_info "Installation complete!"
    if [[ -d "$BACKUP_DIR" ]]; then
        print_info "Backups saved to: $BACKUP_DIR"
    fi
    
    print_info "Reload Hyprland with: hyprctl reload"
}

main "$@"