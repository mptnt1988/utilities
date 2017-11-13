#!/usr/bin/env tcsh

##------------------------------------------------------------------------------
## First load functions in this module:
##     setenv mp_scriptPath <dir_path>
##     source $mp_scriptPath/c_funcs.sh
## The call the functions which are aliases
##------------------------------------------------------------------------------

## Key bindings
bindkey ^r i-search-back

## Set color prompt
## Note: Changing alias precmd and var mp_scriptPath will affect this function
alias mp_set_prompt "source $mp_scriptPath/mp_set_prompt"
