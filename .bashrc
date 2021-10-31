#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

EDITOR=vim

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias egrep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias diff='diff --color=auto'

[ ! -e $HOME/.dircolors ] && eval $(dircolors -p > $HOME/.dircolors)
[ -e $HOME/.dircolors ] && eval $(dircolors -b $HOME/.dircolors)

PS1='[\u@\h \W]\$ '

alias config='git --git-dir=$HOME/dotfiles --work-tree=$HOME'

[[ -e $HOME/.local/bin ]] && eval $(PATH=$HOME/.local/bin:$PATH)

export PATH EDITOR
