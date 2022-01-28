# IRIS-Software-UIO

If you find any bugs, please report them to the developers at the Institute of Theoretical Astrophysics.
Preferably by raising a new issue here: https://github.com/ITA-Solar/IRIS-Software-UIO/issues

or else by mail to:  
v.h.hansteen@astro.uio.no  
martin.wiesmann@astro.uio.no  


## For Developers

This repository includes a pre-commit git hook, that updates a specific line of each modified file with the current date and time. The line with this format will be edited:
```
; $Id: 2021-08-12 21:10 CEST $
```
If the file you modified, does not contain this line yet, please add it, preferably append it to the procedure description at the beginning of the file. 

To make git aware of this hook, run this command after cloning the repository:
```
cd path_of_repository
git config --local core.hooksPath .githooks/
chmod +x .githooks/*
```
Git will then run the script _./githooks/pre-commit_ every time you commit something. This script will check each modified and staged file whether there is a line with the above format, and if yes, updates date and time and adds these changes to the commit.
