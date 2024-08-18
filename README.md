# Introduction
My personal init configurations for Emacs. Feel free to use and tweak it however you like.
### How is my stuff configured?
Basically, my Emacs is configured for the following uses :
* A text editor
* An IDE (through LSP)
* Managing files (through Dired)
* A password manager (not exactly yet)
### Why Doom and Vanilla configs and not just one or the other?
When I started with configuring Vanilla Emacs, I found that it took a lot of time to remotely make it more performant and organised.\
I found that Doom Emacs was pretty comfortable for me to work on, but it wont mean that I would completely shy away from working on my vanilla configs.\
I'll try to update the vanilla configurations occassionally to see what I can apply to make it feel better to use. 
# Installation 
## Prerequisites
* GNU or Doom Emacs Installed
## Downloading both configurations
```git clone --depth 1 https://github.com/kosuzuuu/emacs-figs```
## Downloading only Vanilla or Doom configurations
```git clone --depth 1 --no-checkout https://github.com/kosuzuuu/emacs-figs```\
```cd emacs-figs```\
```git sparse-checkout set Vanilla``` for Vanilla\
or\
```git sparse-checkout set Doom``` for Doom\
```git checkout```
