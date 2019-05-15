#!/bin/bash

sed 's/ |\t|\n//' $1 | ./makeasm.py > $2
