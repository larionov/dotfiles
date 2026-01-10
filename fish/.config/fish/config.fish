if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Fix OpenSSL 3.6.0 compatibility with AUR
set -x OPENSSL_CONF /dev/null

# Add ~/.local/bin to PATH
fish_add_path ~/.local/bin
