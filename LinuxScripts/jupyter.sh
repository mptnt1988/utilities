#!/usr/bin/env bash

cd ~/workspace/_test_
. ~/miniconda2/bin/activate py36
jupyter lab --ip=0.0.0.0 --no-browser >/tmp/cron_jupyter.log 2>&1 &
