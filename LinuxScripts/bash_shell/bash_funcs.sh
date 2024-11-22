#!/bin/bash

###=============================================================================
### Bind keys
### Usage:
###   mp_bind_keys

mp_bind_keys () {
    bind '"\e[A": history-search-backward'
    bind '"\e[B": history-search-forward'
}

###=============================================================================
### Join string with specified delimiter
### Usage:
###   mp_string_join <delimiter> <arg1> <arg2> ...
### Ex:
###   $ mp_string_join : a b c
###       a:b:c

mp_string_join () {
    local delim=$1  # delimiter for joining
    shift
    local backupIFS=$IFS  # save IFS, the field separator
    IFS=$delim
    local result="$*"
    IFS=$backupIFS  # restore IFS
    echo $result
}

###=============================================================================
### Add multiple dirs to PATH, respecting the specified order
### Usage:
###   mp_add_PATH <arg1> <arg2> ...
### Ex:
###   $ mp_add_PATH /a /b /c
###       /a:/b:/c:...

mp_add_PATH () {
    [[ -z "$@" ]] && return 1
    local uniqPaths=$(echo $@ | xargs -n1 | uniq | xargs)
    local paths
    for path in $uniqPaths
    do
        [[ ":$PATH:" != *":$path:"* ]] && paths+=($path)
    done
    [[ -z "${paths[@]}" ]] && return 0
    local result=$(mp_string_join : "${paths[@]}")
    PATH=$result:$PATH
}

###=============================================================================
### Change command prompt
### Usage:
###   mp_change_PS1

# foreground color
_mp_color () {
    echo "\[$(tput setaf $1)\]"
}

# reset all attributes (both fg & bg)
_mp_reset_color () {
    echo "\[$(tput sgr0)\]"
}

# background color
_mp_bg_color () {
    echo "\[$(tput setab $1)\]"
}

_mp_last_exit_status () {
    local last_status=$?
    local reset=$(_mp_reset_color)

    local failure="✘"
    local success="✔"

    if [[ "$last_status" != "0" ]]; then
        last_status="$(_mp_color 5)$failure$reset"
    else
        last_status="$(_mp_color 2)$success$reset"
    fi

    echo "$last_status"
}

# NOTE: git required
# TODO: change color variable names
# mp_change_PS1 () {
#     TERM="xterm-256color"
#     PROMPT_COMMAND='exit_status=$(_mp_last_exit_status);\
# pyvenv=$(if [[ -n "$MP_PYVENV_ALLOWED" ]]; then mp_venv_info; fi);\
# condaenv=$(if [[ -n "$MP_PYVENV_ALLOWED" ]]; then mp_condaenv_info; fi);\
# pyenv=$(if [[ -n "$MP_PYVENV_ALLOWED" ]]; then mp_pyenv_info; fi);\
# background=$(_mp_bg_color 18);\
# istmux=$(mp_tmux_info);\
# user=$(_mp_color 3);\
# at=$(_mp_color 255);\
# host=$(_mp_color 3);\
# time=$(_mp_color 255);\
# dir=$(_mp_color 208);\
# c_green=$(_mp_color 40);\
# gitbr=$(__git_ps1 2>/dev/null);\
# bold=$(tput bold);\
# reset=$(_mp_reset_color);\
# PS1="\n${condaenv}${pyvenv}${pyenv}$background$user\u$at@$host\h $c_green$istmux$time\A($exit_status$background)  $dir${PWD} $c_green$bold$gitbr$reset\n"'
# }
mp_change_PS1 () {
    TERM="xterm-256color"
    PROMPT_COMMAND='exit_status=$(_mp_last_exit_status);\
pyvenv=$(if [[ -n "$MP_PYVENV_ALLOWED" ]]; then mp_venv_info; fi);\
pyenv=$(if [[ -n "$MP_PYVENV_ALLOWED" ]]; then mp_pyenv_info; fi);\
nix=$(if [[ -n "$IN_NIX_SHELL" ]]; then echo "[nix]  "; fi);\
background=$(_mp_bg_color 18);\
istmux=$(mp_tmux_info);\
user=$(_mp_color 3);\
at=$(_mp_color 255);\
host=$(_mp_color 3);\
time=$(_mp_color 255);\
dir=$(_mp_color 208);\
c_green=$(_mp_color 40);\
gitbr=$(__git_ps1 2>/dev/null);\
bold=$(tput bold);\
reset=$(_mp_reset_color);\
PS1="\n${pyvenv}${pyenv}$background${nix}$user\u$at@$host\h $c_green$istmux$time\A($exit_status$background)  $dir${PWD} $c_green$bold$gitbr$reset\n"'
}

###=============================================================================
### Show 256 xterm colors
### Usage:
###   mp_show_colors

_mp_print_color () {
    for c; do
        printf '\e[48;5;%dm%03d' $c $c
    done
    printf '\e[0m \n'
}

mp_show_colors () {
    IFS=$' \t\n'
    _mp_print_color {0..15}
    for ((i=0;i<6;i++)); do
        _mp_print_color $(seq $((i*36+16)) $((i*36+51)))
    done
    _mp_print_color {232..255}
}

###=============================================================================
### GNOME logout
### Usage:
###   mp_logout

mp_logout () {
    gnome-session-quit --no-prompt
}

###=============================================================================
### Python conda env prompt adding
### Usage:
###   - To enable custom prompt
###       mp_venv_prompt
###   - To show the prompt
###       mp_condaenv_info

mp_condaenv_info () {
    [[ -n "$CONDA_DEFAULT_ENV" ]] && echo "<conda: $CONDA_DEFAULT_ENV>\n"
}

###=============================================================================
### Python virtualenv prompt adding
### Usage:
###   - To enable custom prompt
###       mp_venv_prompt
###   - To show the prompt
###       mp_venv_info

mp_venv_info () {
    [[ -n "$VIRTUAL_ENV" ]] && echo "<venv: $VIRTUAL_ENV>\n"
}

###=============================================================================
### Python pyenv prompt adding
### Usage:
###   - To enable custom prompt
###       mp_venv_prompt
###   - To show the prompt
###       mp_venv_info

mp_pyenv_info () {
    [[ -n "$PYENV_VIRTUALENV_INIT" ]] && echo "<pyenv: $(pyenv version)>\n"
}


#--------
mp_venv_prompt () {
    # Disable default virtualenv prompt change
    export VIRTUAL_ENV_DISABLE_PROMPT=1
    export MP_PYVENV_ALLOWED=1
}
#--------

###=============================================================================
### Python venv listing, creating and activating
### Usage:
###   Get help by:
###     mp_pyvenv [-h]

_mp_pyvenv_complete () {
    # fill local variable with a list of completions
    local COMPLETES=$(ls $MP_PYVENV_DIR)
    # we put the completions into $COMPREPLY using compgen
    COMPREPLY=( $(compgen -W "$COMPLETES" -- ${COMP_WORDS[COMP_CWORD]}) )
    return 0
}

mp_pyvenv () {
    case $1 in
        "" | "-h")
            echo -e "*** Use option:" \
            "\n\t\"-d <dir>\"\t\tsetting directory of venvs" \
            "\n\t\"-l\"\t\t\tlisting venvs" \
            "\n\t\"-c <venv name>\"\tcreating a venv" \
            "\n\t\"<venv name>\"\t\tactivating a venv"
            return 1;;
        "-l" )
            if [ -z $MP_PYVENV_DIR ]
            then
                echo "*** MP_PYVENV_DIR unset to any directory"
                mp_pyvenv -h
            else
                echo -e "- VENV DIR:\n$MP_PYVENV_DIR\n"
                echo -e "- VENVs:\n$(ls $MP_PYVENV_DIR)"
            fi;;
        "-d" )
            export MP_PYVENV_DIR=$2
            complete -F _mp_pyvenv_complete mp_pyvenv;;
        "-c" )
            [[ -z $MP_PYVENV_DIR ]] && echo "*** MP_PYVENV_DIR unset to any directory" && return 1
            python3 -m venv "$MP_PYVENV_DIR/$2";;
        * )
            [[ -z $MP_PYVENV_DIR ]] && echo "*** MP_PYVENV_DIR unset to any directory" && return 1
            local activateScript="$MP_PYVENV_DIR/$1/bin/activate"
            if [ -f $activateScript ]
            then source $activateScript
            else
                echo "*** NOT a venv: $MP_PYVENV_DIR/$1"
                return 1
            fi
    esac
}

###=============================================================================
### TMUX use prompt adding
### Usage:
###   mp_tmux_info

mp_tmux_info () {
    [[ -z ${TMUX+x} ]] || echo "(tmux)"
}

###=============================================================================
### Create conda env & corresponding .envrc
### Usage:
###   mp_conda_envrc <name>

mp_conda_envrc () {
    [ -z "$1" ] && { echo "Error: Please input conda env name"; return; }
    local pyver=$2
    [ -z "$pyver" ] && pyver="3.6"
    conda create -y -n $1 python=$pyver
    cat <<EOF > .envrc
CONDA=\$(which conda)
. \${CONDA%/*/*}/etc/profile.d/conda.sh
conda activate $1
EOF
    direnv allow
}
