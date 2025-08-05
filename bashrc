#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

# Configure sudo askpass
export SUDO_ASKPASS="$HOME/.local/bin/wofi-askpass"
alias claude="/home/larionov/.claude/local/claude"
