This is my emacs customization.  +Shamelessy Copied+ Heavily Inspired
from [[https://github.com/steckerhalter/][steckerhalter]].  Uses code
from [[https://github.com/overtone/emacs-live][emacs-live]],
[[https://github.com/eschulte/emacs24-starter-kit][eschulte]],
[[http://tuhdo.github.io/][tuhdo]] and many others.

## Installation Notes
The following are pre-requesites :

0. In Windows and Mac OS X, install cygwin and brew respectively.
1. Pre-requesites on all systems
   - git
   - Mercurial
   - python
   - R
   - GNU global
2. Pre-requesites on windows/cygwin (see dot-emacs-win.el for location)
   - git should be installed outside of cygwin
   - cygwin
   ```
      openssl
      gnutls
      tar
      find
      grep
      make
      perl
      (others : update as required)
   ```
3. Pre-requesites on Mac and GNU/linux
   - openssh
   - (others : update as required)
4. Python dependecies
   - Python 2.7
   - Rope
   - Synatx checker pyflakes and Also, pip install flake8
5. R dependencies
   - R version 3 or above
6. Replace .emacs with dot-emacs-OS.el and edit as required.

## TODO

1. Find a way to install auctex without throwing errors.
