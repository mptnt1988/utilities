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
change_PS1 () {
    export PS1="\n\[\e[37;44m\]{\!} \[\e[m\]\[\e[33;44m\]\u\[\e[m\]\[\e[37;44m\]@\[\e[m\]\[\e[33;44m\]\h\[\e[m\]\[\e[33;44m\] \[\e[m\]\[\e[37;44m\]\A\[\e[m\]\[\e[33;44m\] \[\e[m\]\[\e[30;44m\]\w\[\e[m\]\n\[\e[33;44m\]\\$\[\e[m\] "    
}
