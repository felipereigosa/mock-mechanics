#!/bin/bash

xwininfo -id $(wmctrl -lx | grep 'HP-laptop -' | cut -d" " -f1) | grep 'Absolute upper-left' | cut -d" " -f7
