#!/usr/bin/env bash

cd ~
jupyter lab --ip=0.0.0.0 --no-browser >/tmp/cron_jupyter.log 2>&1 &
