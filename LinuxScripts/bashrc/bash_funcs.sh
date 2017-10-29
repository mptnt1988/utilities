#!/bin/bash

###=============================================================================
### Join string with specified delimiter
### Usage:
###   string_join <delimiter> <arg1> <arg2> ...
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
### Add multiple dirs to PATH
### Usage:
###   mp_add_PATH <arg1> <arg2> ...
### Ex:
###   $ mp_add_PATH /a /b /c
###       ...:/a:/b:/c

mp_add_PATH () {
    local result=$(mp_string_join : "$@")
    PATH=$PATH:$result
}

###=============================================================================
### Change command prompt
### Usage:
###   mp_change_PS1
# helper functions for Bash - easier coloring than using escape sequences

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
mp_change_PS1 () {
    PROMPT_COMMAND='exit_status=$(_mp_last_exit_status);\
background=$(_mp_bg_color 18);\
historyId=$(_mp_color 255);\
user=$(_mp_color 3);\
at=$(_mp_color 255);\
host=$(_mp_color 3);\
time=$(_mp_color 255);\
dir=$(_mp_color 208);\
branch=$(_mp_color 40);\
gitbr=$(__git_ps1)
bold=$(tput bold);\
reset=$(_mp_reset_color);\
PS1="\n$background$historyId{\!} $user\u$at@$host\h $time\A $dir${PWD} $branch$bold$gitbr$reset\n${exit_status} "'
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
