{
  description = "larionov/dotfiles — cross-platform dev environment (minimal flake)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      # All systems this dotfiles repo targets (macOS + Linux).
      systems = [ "aarch64-darwin" "x86_64-darwin" "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    {
      # `nix develop` — a shell with the common CLI toolchain on every system.
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          lib = pkgs.lib;

          # Cross-platform CLI tools.
          common = with pkgs; [
            git
            stow
            fish
            ripgrep
            fd
            fzf
            jq
            bat
            eza
            tree
          ];

          # macOS-only extras (guarded by isDarwin).
          # NOTE: yabai / skhd / sketchybar are intentionally NOT managed here —
          # they live in the brew + GNU Stow setup (see install.sh `macos)` case).
          darwinOnly = lib.optionals pkgs.stdenv.isDarwin (with pkgs; [
            # e.g. pngpaste, terminal-notifier
          ]);

          # Linux-only extras (guarded by isLinux).
          linuxOnly = lib.optionals pkgs.stdenv.isLinux (with pkgs; [
            # e.g. wl-clipboard, waybar (WM tooling managed outside nix)
          ]);
        in
        {
          default = pkgs.mkShell {
            packages = common ++ darwinOnly ++ linuxOnly;
          };
        });

      # `nix build .#<name>` — add reproducible packages here per system.
      packages = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          # example: hello = pkgs.hello;
        });

      # `nix fmt`
      formatter = forAllSystems (system:
        nixpkgs.legacyPackages.${system}.nixpkgs-fmt);
    };
}
