#!/bin/bash

xwininfo -id $(wmctrl -lx | grep '\-.-' | cut -d" " -f1) | grep 'Absolute upper-left' | cut -d" " -f7
