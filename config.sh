#!/bin/csh -f
 
cd ~/
cp -i ~MS205274/.emacs .
cp -r ~MS205274/bin/ .
cp -r ~MS205274/.cshrc.perso .
echo "source ~/.cshrc.perso" >> ~/.cshrc

