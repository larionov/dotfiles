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
if [[ -f "$HOME/.local/bin/env" ]]; then
    . "$HOME/.local/bin/env"
fi

alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

# Bash completion settings
bind 'set show-all-if-ambiguous on' 2>/dev/null
bind 'TAB:menu-complete' 2>/dev/null

export PATH="$HOME/.npm-global/bin:$PATH"

# opencode
export PATH="$HOME/.opencode/bin:$PATH"

#export NVM_DIR="$HOME/.nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# direnv: load ~/.envrc and per-project env
eval "$(direnv hook bash)"


# Added by Antigravity CLI installer
export PATH="/Users/upwork/.local/bin:$PATH"

# Upwork corporate CA — NODE_EXTRA_CA_CERTS supplements Node's CA store (no replacement).
# NOTE: intentionally NOT SSL_CERT_FILE — that replaces the OpenSSL store and would break public CAs.
export NODE_EXTRA_CA_CERTS="$HOME/.config/upwork/certs/upwork-ca-bundle.pem"
