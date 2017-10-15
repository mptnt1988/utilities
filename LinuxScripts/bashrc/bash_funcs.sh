#!/bin/bash

### Usage:
###   string_join <delimiter> <arg1> <arg2> ...
### Ex:
###   $ string_join : a b c
###       a:b:c
string_join () {
    local delim=$1      # delimiter for joining
    shift
    local backupIFS=$IFS   # save IFS, the field separator
    IFS=$delim
    local result="$*"
    IFS=$oldIFS   # restore IFS
    echo $result
}

### Usage:
###   add_PATH <arg1> <arg2> ...
### Ex:
###   $ add_PATH /a /b /c
###       ...:/a:/b:/c
add_PATH () {
    local result=$(string_join : "$@")
    PATH=$PATH:$result
}

### Usage:
###   change_PS1
# helper functions for Bash - easier coloring than using escape sequences
function Color() {
  echo "\[$(tput setaf $1)\]"
}
function ResetColor() {
  echo "\[$(tput sgr0)\]"
}

function LastExitStatus() {
    local last_status=$?
    local reset=$(ResetColor)

    local failure="✘"
    local success="✔"

    if [[ "$last_status" != "0" ]]; then
        last_status="$(Color 5)$failure$reset"
    else
        last_status="$(Color 2)$success$reset"
    fi

    echo "$last_status"
}

change_PS1 () {
    PROMPT_COMMAND='PS1="\n\[\e[37;44m\]{\!} \[\e[m\]\[\e[33;44m\]\u\[\e[m\]\[\e[37;44m\]@\[\e[m\]\[\e[33;44m\]\h\[\e[m\]\[\e[33;44m\] \[\e[m\]\[\e[37;44m\]\A\[\e[m\]\[\e[33;44m\] \[\e[m\]\[\e[30;44m\]\w\[\e[m\]\n$(LastExitStatus) "'
}
