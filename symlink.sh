#!/bin/bash

for f in $(ls -d ~/Dotfiles/.*); 
do ln -s $f ~/; 
done && ls -al ~/
