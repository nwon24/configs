#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

EDITOR=vim

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias diff='diff --color=auto'

alias ..='cd ..'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

PS1='[\u@\h \W]\$ '

# Set PATH
if [ -d $HOME/.local/bin ]; then
    PATH=$HOME/.local/bin:$PATH
fi

if [ -d $HOME/.bin ]; then
    PATH=$HOME/.bin:$PATH
fi

if [ -d $HOME/opt/bin ]; then
    PATH=$HOME/opt/bin:$PATH
fi

# Extended color support for ls
[ ! -e $HOME/.dircolors ] && eval $(dircolors -p > $HOME/.dircolors)
[ -e $HOME/.dircolors ] && eval $(dircolors -b $HOME/.dircolors)

export EDITOR PATH
