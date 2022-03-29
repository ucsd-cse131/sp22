---
title: Troubleshooting FAQ
headerImg: sea.jpg
---


# Troubleshooting FAQ (by Aidan Denlinger)


Here's a collection of troubleshooting instructions and threads. Please try the instructions in here before making a new piazza 
post. We'll try to keep this post updated, feel free to add any solutions/threads missing from this post in the followups.

## Turning in Homework:

- *Is it turned in?*: your latest commit on the github classroom repo is what we'll see.
- *Teammates*: edit `COLLABORATORS.md` with a comma separated list of your teammates. 
   Use the `Raw` button to check this file on github.

## ieng6 procedure

- **Please ssh into `ieng6-640.ucsd.edu`, @235, it seems to work while the other servers don't.**

- your ieng6 login should be your active directory credentials (your email username and password). (ex `ssh student@ieng6-640.ucsd.edu`). You can verify your account at the link in @15_f8

- **immediately prep for this class by running `cs130wi21`**. (if the first one doesn't work, try `prep cs130wi21`, if the first two don't work try `module load /home/linux/ieng6/cs130wi21/public/modulefiles/cs130wi21`) Not prepping is the main cause of the issues below. You should now be able to clone a repo and work without any problems. Note that prepping will take you to a new folder: this is where you should do your work for this class. If your repo is *not* in the prepped environment, especially if you've tried to run `make`, follow the instructor steps in @122.

- You may want to consider adding `cs130wi21` to your `.bashrc` to prep whenever you log into ieng6.

## ieng6 errors:

- Quick note: the `.stack-work` folder in your PA folder is *okay*, all references to `.stack` are *specifically* about a folder called `.stack`, *not* `.stack-work`.

- *Read-only file system/permission denied*: @235, please ssh into `ieng6-640.ucsd.edu`. It seems to work where the other servers won't.

- *Errors about locks*: Make sure you're on `ieng6-640`, see bullet point above. If you are, try the steps in @237 which will use a different build workflow. Make a new post if this doesn't work.

- *`make` says "Preparing to install GHC to an isolated location"*: Follow the instructor steps in @122. We have already installed GHC for you, we don't want `stack` trying to install it again! The goal is to make sure there is no `.stack` folder.

- *error involving "disk quota" or unable to save files in vim*: Running out of disk quota means you're out of space on ieng6. Please go to your home directory (run `cd`) and try the command in @18 both before prepping (running cs130wi21) and after prepping: this will show a sorted list of files taking up space, the more space it takes up the higher it is on the list. If you have a `.stack` folder, follow the bullet point above, this is a GHC/prep issue. In terms of folders that are easy to delete to make space, `.eclipse` holds your eclipse settings on ieng6 and `.mozilla` holds ieng6 firefox settings, if you don't care about those they should be easily deleted. If you're trying VSCode Remote Development, it seems like deleting `.vscode-server` helps here (see the VSCode section below). Also, ensure that when you clone your repo, you've prepped.

- "*Device or Resource busy" when trying to delete*: try @196. If that doesn't work, try to move the busy folder (`mv folder new_name`) and try deleting it later.

- *test suite errors at the end*: @68, this is expected behavior.

- *Elsa syntax highlighting in vim (keep in mind this is an extra, if this doesn't immediately work I'd just go without)*: see @114

## Alternatives to VSCode Remote Development:

- work locally by installing 
  - [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/tutorials/wsl-vscode) if on Windows, the link shows how to use it with VSCode.
  - `stack` using a package manager like `apt` (`sudo apt install haskell-stack`), `brew`, etc or from the [stack website](docs.haskellstack.org)
  - optionally, the Haskell extension for VSCode
  - note that we cannot guarantee support for your local environment: we provide ieng6 as a stable environment. However, this should be a relatively stable setup.
- editing your files locally, pushing to github, pulling on ieng6 in the terminal, and testing there
- working entirely on ieng6 using vim, nano, or X11 forwarding to access a terminal

Specifically for elsa/hw 0:
- running elsa with the web interface per the README
- working locally by installing `elsa` locally per the README

## VSCode Remote Development:

- Please keep in mind that VSCode Remote Development is an additional bonus, it's not required for any PA. I'd recommend not spending a lot of time trying to set it up if it's giving you trouble, there are alternatives above where you can work with your own local vscode and push to github.

- *"VS Code server failed to start"*: @71 seems to help some students, deleting `.vscode-server` from the home folder of ieng6 also seems to help some people in @18_f1. You should also check the disk quota issues above: if you're out of space, vscode can't move the server files it needs to into ieng6.

- *Disconnections on windows*: go to Settings, search for conpty, unchuck "Windows Enable Conpty", or try [this support page](https://code.visualstudio.com/docs/remote/troubleshooting#_troubleshooting-hanging-or-failing-connections) (see @21_f4)

- *can't push to github from within VSCode*: Go to Settings, search "Git: Terminal Authentication" and uncheck it. It seems that VSCode is trying to send credentials from the local computer that don't exist


- *doesn't work on VSCodium/VS Code compiled from source*: [Remote Development is a closed source extension that can only be used with Microsoft binaries of VSCode.](https://github.com/VSCodium/vscodium/issues/240). Either use the alternatives above or use the Microsoft versions.

- *Haskell syntax highlighting*: Install the Haskell extension for VSCode.


