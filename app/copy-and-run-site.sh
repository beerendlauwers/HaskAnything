#!/bin/bash
cd ~/HaskAnything/app
stack build
cd ~/HaskAnything/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/bin/
cp ./hask-anything-exe ~/HaskAnything/app/hask-anything-exe
cd ~/HaskAnything/app
./hask-anything-exe rebuild
./hask-anything-exe watch
