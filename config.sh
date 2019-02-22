#!/bin/csh -f
 
cd ~/
cp -i ~MS013565/.emacs .
cp -r ~MS013565/bin/ .
cp -r ~MS013565/.cshrc.perso .
echo "source ~/.cshrc.perso" >> ~/.cshrc

