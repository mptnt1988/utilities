# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

###------------###
### Tuan added ###
###------------###

# FOR MACOS
source $(brew --prefix)/etc/bash_completion

# TMUX
[ -z "$TMUX" ] && [ -z "${MP_MOBAXTERM}" ] && tmux

# LOAD FUNCTIONS
source ~/programs/utilities/LinuxScripts/bashrc/bash_funcs.sh

# BIND KEYS
mp_bind_keys

# SOFTWARE DIR
mp_add_PATH ~/programs/_softs_/

# PHP LARAVEL
mp_add_PATH ~/.config/composer/vendor/bin/

# CHANGE BASH PROMPT
mp_change_PS1

# PYTHON
# Python dir
export PYTHONSTARTUP=~/.pythonrc
# Add Python venv custom prompt
mp_venv_prompt
# Jupyter notebook
alias mp_jnb='jupyter notebook --ip=0.0.0.0 --no-browser'
alias mp_jlab='jupyter lab --ip=0.0.0.0 --no-browser'
alias mp_jlabs='~/programs/utilities/LinuxScripts/jupyter.sh'
alias mp_jnbl='jupyter notebook list'
alias mp_jnbs='jupyter notebook stop'
alias mp_jksl='jupyter kernelspec list'
alias mp_jksr='jupyter kernelspec remove'
# Setting for Django manage.py auto-complete
source ~/programs/utilities/LinuxScripts/bashrc/django_bash_completion.sh
# Other alias by me
alias mp_pms='python manage.py shell'
# Added by Miniconda2 installer
# export PATH="/home/tuantran/miniconda2/bin:$PATH"
# . /home/tuantran/miniconda2/etc/profile.d/conda.sh

# GO
export GOROOT=/usr/local/go
export GOPATH=/home/tuantran/programs/_softs_/go
mp_add_PATH $GOROOT/bin:$GOPATH/bin

# NODEJS - NPM
# global installation - change dir
mkdir ~/.npm-global 2>/dev/null
npm config set prefix '~/.npm-global'
mp_add_PATH ~/.npm-global/bin/
export NODE_PATH=~/.npm-global/lib/node_modules/

# DIRENV
eval "$(direnv hook bash)"

# SSH
alias sshtunnel='ssh -N -o GatewayPorts=yes'
alias sshtunnell='ssh -N -o GatewayPorts=yes -L'
alias sshtunnelr='ssh -N -o GatewayPorts=yes -R'
# for these auto-completions to work, make "ssh" + TAB work first
complete -F _ssh sshtn
complete -F _ssh sshtl
complete -F _ssh sshtr

# ALIAS / CONSTANT
alias cd_problems='cd ~/workspace/_works_/_side_pjs_/problems/'
alias cd_utilities='cd ~/programs/utilities/'
alias condaa='conda activate'
alias condad='conda deactivate'
alias emacsnw='emacs -nw'

###------###
### Tran ###
###------###
