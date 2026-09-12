# Runtime Notes

This repo expects a Homebrew-managed runtime plus a user-local SbarLua install.

## Required commands

- `sketchybar`
- `lua`
- `git`
- `make`
- `clang`

## Required font families

The active configuration expects these font families to be visible to macOS:

- `JetBrainsMono Nerd Font`
- `JetBrainsMono Nerd Font Mono`
- `Symbols Nerd Font`
- `sketchybar-app-font`

## SbarLua

`sketchybarrc` and `init.lua` expect the user-local SbarLua install layout:

- Interpreter: `~/.local/share/sketchybar_lua/lua5.5`
- Module: `~/.local/share/sketchybar_lua/sketchybar.so`

## Bootstrap behavior

`bootstrap.sh` does not install packages or fonts. It only:

1. Verifies the Homebrew-managed runtime and visible fonts.
2. Clones or fast-forwards `~/.config/sketchybar`.
3. Rebuilds the native helpers in `helpers/`.
4. Reloads SketchyBar if it is already running.
