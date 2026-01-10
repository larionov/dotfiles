#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Platform-specific configuration
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    export NODE_OPTIONS="--dns-result-order=ipv4first"
    eval "$(/opt/homebrew/bin/brew shellenv)"
    alias ls='ls -G'
else
    # Linux
    alias ls='ls --color=auto'
    # Configure sudo askpass
    export SUDO_ASKPASS="$HOME/.local/bin/sudo-askpass"
    # Fix OpenSSL 3.6.0 compatibility with AUR
    export OPENSSL_CONF=/dev/null
fi

# Common configuration
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/.claude/local:$PATH"
if [[ -f "$HOME/.local/bin/env" ]]; then
    . "$HOME/.local/bin/env"
fi

alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

# Bash completion settings
bind 'set show-all-if-ambiguous on' 2>/dev/null
bind 'TAB:menu-complete' 2>/dev/null

