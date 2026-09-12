set fish_greeting

# Fix OpenSSL 3.6.0 compatibility with AUR
set -x OPENSSL_CONF /dev/null

# Add ~/.local/bin to PATH
fish_add_path ~/.local/bin

if test -x /opt/homebrew/bin/brew
    eval "$(/opt/homebrew/bin/brew shellenv)"
end

#set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --follow --no-ignore-vcs --glob "!node_modules"'
set -x FZF_ALT_C_OPTS "--walker-skip node_modules,.git,target,Library"

set -x FZF_CTRL_T_OPTS "\
  --walker-skip .git,node_modules,target,Library\
  --preview 'bat -n --color=always {}'\
  --bind 'ctrl-/:change-preview-window(down|hidden|)'"

set -x FZF_CTRL_R_OPTS "\
  --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort'\
  --color header:italic\
  --header 'Press CTRL-Y to copy command into clipboard'"

# Emacs daemon helpers
alias emacs-restart="emacsclient -e '(kill-emacs)' 2>/dev/null; echo 'Emacs restarting...'"
alias e="emacsclient -c -a=emacs"


# Added by Antigravity CLI installer
set -gx PATH "/Users/upwork/.local/bin" $PATH
