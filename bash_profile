export CLICOLOR=1
export PS1="\u@\h:\W\$ "
export HISTCONTROL=ignoreboth
export EDITOR=vim
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8

# Autocompletion
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

JPATH=$GOPATH/src/github.com/jroimartin
JPATH=$JPATH:$GOPATH/src/bitbucket.org/jroimartin

j() {
   CDPATH=$JPATH cd $@
}

_j() {
   CDPATH=$JPATH _cd
}

complete -o nospace -F _j j
