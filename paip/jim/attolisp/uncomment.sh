#!/bin/bash

sed s/\;.*// $1 | xclip -selection clipboard
