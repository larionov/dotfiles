#!/usr/bin/env bash

set -euo pipefail

TARGET_DIR="${HOME}/.config/sketchybar"
REPO_URL="https://github.com/binbinsh/sketchybar-config.git"
SBARLUA_DIR="${HOME}/.local/share/sketchybar_lua"
FONT_CATALOG=""

info() { printf "[i] %s\n" "$*"; }
warn() { printf "[!] %s\n" "$*"; }
done_msg() { printf "[+] %s\n" "$*"; }
die() { printf "[x] %s\n" "$*" >&2; exit 1; }

require_command() {
  local command_name="$1"
  command -v "$command_name" >/dev/null 2>&1 || die "Missing required command: ${command_name}. Install it with Homebrew or add it to PATH, then rerun this script."
}

load_font_catalog() {
  [ -n "$FONT_CATALOG" ] && return
  FONT_CATALOG="$(system_profiler SPFontsDataType 2>/dev/null || true)"
}

require_font_family() {
  local family="$1"
  load_font_catalog
  printf '%s' "$FONT_CATALOG" | grep -Fq "Family: ${family}" || die "Missing font family: ${family}. Install it via Homebrew cask or Font Book, then rerun this script."
}

ensure_runtime() {
  local command_name

  for command_name in git make clang lua sketchybar; do
    require_command "$command_name"
  done

  [ -x "${SBARLUA_DIR}/lua5.5" ] || die "Missing SbarLua interpreter: ${SBARLUA_DIR}/lua5.5"
  [ -f "${SBARLUA_DIR}/sketchybar.so" ] || die "Missing SbarLua module: ${SBARLUA_DIR}/sketchybar.so"

  require_font_family "JetBrainsMono Nerd Font"
  require_font_family "JetBrainsMono Nerd Font Mono"
  require_font_family "Symbols Nerd Font"
  require_font_family "sketchybar-app-font"
}

backup_target() {
  local backup_dir="${TARGET_DIR}_backup_$(date +%Y%m%d_%H%M%S)"
  warn "Backing up existing target to $backup_dir"
  mv "$TARGET_DIR" "$backup_dir"
}

clone_repo() {
  info "Cloning SketchyBar config into $TARGET_DIR"
  mkdir -p "$(dirname "$TARGET_DIR")"
  git clone --depth=1 "$REPO_URL" "$TARGET_DIR"
}

default_branch() {
  git -C "$1" symbolic-ref --quiet --short refs/remotes/origin/HEAD 2>/dev/null | sed 's|^origin/||'
}

sync_repo() {
  local branch
  local remote_url

  if [ ! -e "$TARGET_DIR" ]; then
    clone_repo
    return
  fi

  if [ ! -d "${TARGET_DIR}/.git" ]; then
    backup_target
    clone_repo
    return
  fi

  remote_url="$(git -C "$TARGET_DIR" remote get-url origin 2>/dev/null || true)"
  if [ "$remote_url" != "$REPO_URL" ] || [ -n "$(git -C "$TARGET_DIR" status --porcelain --untracked-files=all)" ]; then
    backup_target
    clone_repo
    return
  fi

  info "Updating existing repo in $TARGET_DIR"
  git -C "$TARGET_DIR" fetch --prune origin
  branch="$(default_branch "$TARGET_DIR")"
  [ -n "$branch" ] || branch="main"
  git -C "$TARGET_DIR" switch "$branch" >/dev/null 2>&1 || git -C "$TARGET_DIR" checkout "$branch" >/dev/null 2>&1 || true

  if git -C "$TARGET_DIR" merge --ff-only "origin/$branch" >/dev/null 2>&1; then
    done_msg "Updated $TARGET_DIR to origin/$branch"
  else
    warn "Fast-forward update failed for $TARGET_DIR"
    backup_target
    clone_repo
  fi
}

build_helpers() {
  info "Building native helpers"
  make -C "${TARGET_DIR}/helpers"
}

reload_sketchybar() {
  if ! pgrep -x sketchybar >/dev/null 2>&1; then
    warn "SketchyBar is not running; start it via Homebrew services or your launcher, then reload if needed."
    return
  fi

  info "Reloading SketchyBar"
  sketchybar --reload
}

print_runtime_notice() {
  info "Dependencies are expected to be managed by Homebrew"
  info "Detailed runtime notes: ${TARGET_DIR}/docs/runtime.md"
}

main() {
  ensure_runtime
  sync_repo
  build_helpers
  reload_sketchybar
  print_runtime_notice
  done_msg "SketchyBar bootstrap completed"
}

main "$@"
