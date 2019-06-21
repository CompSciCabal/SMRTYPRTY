#!/bin/bash

 tr -d " "  < $1 |  tr -d "\t" |  tr -d "\n" | sed 's/\\b/ /g' | ./makeasm.py | xclip -selection clipboard
