#!/usr/bin/sh -l
cd $1
latexmk -pdflua $2
