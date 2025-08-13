#!/bin/bash

# GNU Stow-based dotfiles installation script
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKUP_DIR="$HOME/.dotfiles_backup_$(date +%Y%m%d_%H%M%S)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

print_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
print_warning() { echo -e "${YELLOW}[WARN]${NC} $1"; }
print_debug() { echo -e "${BLUE}[DEBUG]${NC} $1"; }
print_error() { echo -e "${RED}[ERROR]${NC} $1"; }

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

# Package installation
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

# Check and install dependencies
check_dependencies() {
    local os=$(detect_os)
    local missing_packages=()
    
    # Check for GNU Stow
    if ! command -v stow >/dev/null 2>&1; then
        case "$os" in
            "macos") missing_packages+=("stow") ;;
            "linux/arch") missing_packages+=("stow") ;;
            "linux/debian") missing_packages+=("stow") ;;
            "linux/fedora") missing_packages+=("stow") ;;
            *) print_error "Please install GNU Stow manually"; exit 1 ;;
        esac
    fi
    
    # Common dependencies
    if ! command -v unzip >/dev/null 2>&1; then
        missing_packages+=("unzip")
    fi
    
    if ! command -v wget >/dev/null 2>&1 && ! command -v curl >/dev/null 2>&1; then
        missing_packages+=("wget")
    fi
    
    if ! command -v rg >/dev/null 2>&1; then
        missing_packages+=("ripgrep")
    fi
    
    if ! command -v git >/dev/null 2>&1; then
        missing_packages+=("git")
    fi
    
    # Platform-specific dependencies
    case "$os" in
        "linux/arch")
            if ! command -v dunst >/dev/null 2>&1; then
                missing_packages+=("dunst")
            fi
            if ! command -v notify-send >/dev/null 2>&1; then
                missing_packages+=("libnotify")
            fi
            if ! command -v wofi >/dev/null 2>&1; then
                missing_packages+=("wofi")
            fi
            if ! command -v hyprpolkitagent >/dev/null 2>&1; then
                missing_packages+=("hyprpolkitagent")
            fi
            if ! command -v zenity >/dev/null 2>&1; then
                missing_packages+=("zenity")
            fi
            ;;
    esac
    
    if [[ ${#missing_packages[@]} -gt 0 ]]; then
        install_packages "${missing_packages[@]}"
        print_info "✓ Dependencies installed"
    fi
}

# Font installation (unchanged from original)
install_fonts() {
    local os=$(detect_os)
    local fonts_dir
    local install_iosevka="${INSTALL_IOSEVKA:-false}"
    
    case "$os" in
        "macos")
            fonts_dir="$HOME/Library/Fonts"
            ;;
        *)
            fonts_dir="$HOME/.local/share/fonts"
            ;;
    esac
    
    mkdir -p "$fonts_dir"
    
    # Install FiraMono Nerd Font (check for FiraMono or FiraCode Nerd Font variants)
    
    if ! fc-list | grep -i firacode | grep -iq nerd && ! fc-list | grep -i firamono | grep -iq nerd; then
        print_info "Installing FiraMono Nerd Font (no Fira Nerd Font detected)..."
        
        local temp_dir=$(mktemp -d)
        local fira_url="https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraMono.zip"
        
        if command -v wget >/dev/null 2>&1; then
            wget -O "$temp_dir/fira-nerd.zip" "$fira_url"
        else
            curl -L -o "$temp_dir/fira-nerd.zip" "$fira_url"
        fi
        
        unzip -q "$temp_dir/fira-nerd.zip" -d "$temp_dir"
        cp "$temp_dir"/*.ttf "$fonts_dir/" 2>/dev/null || true
        
        if [[ "$os" != "macos" ]]; then
            fc-cache -fv
        fi
        print_info "✓ FiraMono Nerd Font installed"
        
        rm -rf "$temp_dir"
    else
        print_debug "✓ Fira Nerd Font already installed (FiraCode or FiraMono variant found)"
    fi
    
    # Install Iosevka font (optional)
    if [[ "$install_iosevka" == "true" ]] && ! fc-list | grep -i "iosevka" > /dev/null 2>&1; then
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
    fi
    
    # Install JetBrains Mono Nerd Font for Linux
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
    fi
}

# Install Emacs base configuration
install_emacs_base() {
    local emacs_dir="$HOME/.emacs.d"
    
    # Only install minimal-emacs.d if ~/.emacs.d doesn't exist
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
    else
        print_debug "✓ Emacs base configuration already exists"
    fi
}

# Install git packages for Emacs
install_git_packages() {
    # Install git packages into the dotfiles source directory, not the stowed location
    local source_packages_dir="$DOTFILES_DIR/emacs/.emacs.d/packages"
    
    # Install macrursors package from git
    local package_dir="$source_packages_dir/macrursors"
    if [[ ! -d "$package_dir/.git" ]]; then
        print_info "Installing macrursors package from git..."
        rm -rf "$package_dir"
        mkdir -p "$source_packages_dir"
        git clone "https://github.com/corytertel/macrursors" "$package_dir"
        print_info "✓ macrursors package installed from git"
    else
        print_debug "✓ macrursors package already installed from git"
    fi
}

# Backup existing configurations
create_backup() {
    local files_to_backup=()
    
    # Check what would be overwritten by stow
    if [[ -f "$HOME/.bashrc" && ! -L "$HOME/.bashrc" ]]; then
        files_to_backup+=("$HOME/.bashrc")
    fi
    
    if [[ -d "$HOME/.config/kitty" && ! -L "$HOME/.config/kitty" ]]; then
        files_to_backup+=("$HOME/.config/kitty")
    fi
    
    if [[ -d "$HOME/.emacs.d" && ! -L "$HOME/.emacs.d" ]]; then
        # Don't backup if it's minimal-emacs.d (we want to stow over it)
        if [[ ! -f "$HOME/.emacs.d/init.el" ]] || ! grep -q "minimal-emacs.d" "$HOME/.emacs.d/init.el" 2>/dev/null; then
            files_to_backup+=("$HOME/.emacs.d")
        fi
    fi
    
    if [[ ${#files_to_backup[@]} -gt 0 ]]; then
        print_info "Creating backup of existing files..."
        mkdir -p "$BACKUP_DIR"
        for file in "${files_to_backup[@]}"; do
            cp -r "$file" "$BACKUP_DIR/"
            print_info "Backed up: $(basename "$file")"
        done
        print_info "Backup saved to: $BACKUP_DIR"
    fi
}

# Stow packages
stow_packages() {
    local os=$(detect_os)
    local packages=()
    
    cd "$DOTFILES_DIR"
    
    # Universal packages
    packages+=("bash" "kitty" "scripts")
    
    # Platform-specific packages
    case "$os" in
        "linux/arch")
            packages+=("hyprland" "waybar" "dunst")
            # Machine-specific packages
            local machine_id=""
            if [[ -f /etc/machine-id ]]; then
                machine_id=$(cat /etc/machine-id 2>/dev/null || echo "")
            fi
            # Only install udev rules on the specific ThinkPad (machine ID: 2fe803d1fda247ce9c349f4c72fb2e4f)
            if [[ "$machine_id" == "2fe803d1fda247ce9c349f4c72fb2e4f" ]]; then
                packages+=("udev")
            fi
            ;;
        "macos")
            # macOS-specific packages would go here
            ;;
    esac
    
    # Always install emacs last (after base is set up)
    packages+=("emacs")
    
    print_info "Installing packages with Stow: ${packages[*]}"
    
    for package in "${packages[@]}"; do
        if [[ -d "$package" ]]; then
            if [[ "$package" == "udev" ]]; then
                print_info "Installing $package with sudo (requires root access for /etc)..."
                sudo stow -v -t / "$package"
                print_info "✓ $package stowed to root filesystem"
                # Reload udev rules
                print_info "Reloading udev rules..."
                sudo udevadm control --reload-rules
                sudo udevadm trigger --subsystem-match=input
                print_info "✓ udev rules reloaded"
            else
                print_info "Stowing $package..."
                stow -v -t ~ "$package"
                print_info "✓ $package stowed"
            fi
        else
            print_warning "Package directory $package not found, skipping"
        fi
    done
}

# Unstow packages (for uninstall)
unstow_packages() {
    local os=$(detect_os)
    local packages=()
    
    cd "$DOTFILES_DIR"
    
    # Universal packages
    packages+=("bash" "kitty" "scripts" "emacs")
    
    # Platform-specific packages
    case "$os" in
        "linux/arch")
            packages+=("hyprland" "waybar" "dunst")
            # Machine-specific packages
            local machine_id=""
            if [[ -f /etc/machine-id ]]; then
                machine_id=$(cat /etc/machine-id 2>/dev/null || echo "")
            fi
            # Only include udev rules on the specific ThinkPad (machine ID: 2fe803d1fda247ce9c349f4c72fb2e4f)
            if [[ "$machine_id" == "2fe803d1fda247ce9c349f4c72fb2e4f" ]]; then
                packages+=("udev")
            fi
            ;;
    esac
    
    print_info "Removing packages with Stow: ${packages[*]}"
    
    for package in "${packages[@]}"; do
        if [[ -d "$package" ]]; then
            if [[ "$package" == "udev" ]]; then
                print_info "Removing $package with sudo..."
                sudo stow -D -v -t / "$package"
                print_info "✓ $package unstowed from root filesystem"
            else
                print_info "Unstowing $package..."
                stow -D -v -t ~ "$package"
                print_info "✓ $package unstowed"
            fi
        fi
    done
}

# Update git packages
update_git_packages() {
    local source_packages_dir="$DOTFILES_DIR/emacs/.emacs.d/packages"
    
    print_info "Updating git-based Emacs packages..."
    
    # Update macrursors
    local package_dir="$source_packages_dir/macrursors"
    if [[ -d "$package_dir/.git" ]]; then
        print_info "Updating macrursors..."
        (cd "$package_dir" && git pull origin main 2>/dev/null || git pull origin master 2>/dev/null)
        print_info "✓ macrursors updated"
    else
        print_warning "macrursors not found or not a git repository"
    fi
}

# Main installation function
main() {
    local os=$(detect_os)
    print_info "Installing dotfiles with GNU Stow on: $os"
    
    # Handle commands
    case "${1:-install}" in
        "install"|"")
            ;;
        "uninstall")
            unstow_packages
            print_info "Dotfiles uninstalled"
            exit 0
            ;;
        "update")
            update_git_packages
            exit 0
            ;;
        "restow")
            print_info "Restowing packages..."
            unstow_packages
            stow_packages
            print_info "Packages restowed"
            exit 0
            ;;
        *)
            show_usage
            exit 1
            ;;
    esac
    
    # Check dependencies first
    if ! check_dependencies; then
        exit 1
    fi
    
    # Install fonts
    install_fonts
    
    # Install Emacs base configuration
    install_emacs_base
    
    # Install git packages
    install_git_packages
    
    # Create backup of existing files
    create_backup
    
    # Stow all packages
    stow_packages
    
    print_info "Installation complete!"
    if [[ -d "$BACKUP_DIR" ]]; then
        print_info "Backups saved to: $BACKUP_DIR"
    fi
    
    # Platform-specific setup
    case "$os" in
        "linux/arch")
            # Enable hyprpolkitagent systemd service
            if command -v hyprpolkitagent >/dev/null 2>&1; then
                systemctl --user enable hyprpolkitagent.service
                systemctl --user start hyprpolkitagent.service
                print_info "✓ hyprpolkitagent service enabled"
            fi
            print_info "Reload Hyprland with: hyprctl reload"
            ;;
        "macos")
            print_info "Restart Terminal or run: source ~/.bashrc"
            ;;
    esac
    
    print_info "Update git packages with: $0 update"
    print_info "Uninstall with: $0 uninstall"
    print_info "Restow packages with: $0 restow"
}

show_usage() {
    echo "GNU Stow-based dotfiles installer"
    echo "Usage: $0 [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  install     Install dotfiles (default)"
    echo "  uninstall   Remove all symlinks"
    echo "  restow      Remove and reinstall all symlinks"
    echo "  update      Update git-based packages"
    echo ""
    echo "Environment variables:"
    echo "  INSTALL_IOSEVKA=true    Install Iosevka font (optional)"
    echo ""
    echo "Examples:"
    echo "  $0                      # Install dotfiles"
    echo "  $0 uninstall            # Remove all symlinks"
    echo "  $0 update               # Update git packages"
    echo "  INSTALL_IOSEVKA=true $0 # Install with Iosevka font"
}

# Handle help and run main
case "${1:-install}" in
    "help"|"-h"|"--help")
        show_usage
        ;;
    *)
        main "$@"
        ;;
esac
