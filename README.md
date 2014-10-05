This is my emacs customization organized as a set of org files.

## Installation Notes
The following are pre-requesites :

0. In Windows/Mac OS X, install cygwin/brew
1. Pre-requesites on all systems
2. Pre-requesites on windows/cygwin (see the specific .emacs file)
   - openssl
   - gnutls
   - tar
   - make
   - python
   - GNU global
3. Pre-requesites on mac/brew (TODO:: update)
   - GNU global
4. Pre-requesites on GNU/linux (TODO:: update)
   -
5. Install the latest version of org-mode using package-install
    package-install RET org RET
6. Edit .emacs as given the dot-emacs-OS

## Issues
1. Disabled upgrade on initialization because of the following patch
   Patch package-build. Change the pb/create-tar function from
```
    (defun pb/create-tar (file dir &optional files)
      "Create a tar FILE containing the contents of DIR, or just FILES if non-nil."
      (apply 'process-file
             package-build-tar-executable nil
             (get-buffer-create "*package-build-checkout*")
             nil "-cvf"
             file
             "--exclude=.svn"
             "--exclude=CVS"
             "--exclude=.git*"
             "--exclude=_darcs"
             "--exclude=.fslckout"
             "--exclude=_FOSSIL_"
             "--exclude=.bzr"
             "--exclude=.hg"
             (or (mapcar (lambda (fn) (concat dir "/" fn)) files) (list dir))))
```
  to the following
```
    (defun pb/create-tar (file dir &optional files)
      "Create a tar FILE containing the contents of DIR, or just FILES if non-nil."
      (apply 'process-file
             package-build-tar-executable nil
             (get-buffer-create "*package-build-checkout*")
             nil
        "--force-local"
        "-cvf"
             file
             "--exclude=.svn"
             "--exclude=CVS"
             "--exclude=.git*"
             "--exclude=_darcs"
             "--exclude=.fslckout"
             "--exclude=_FOSSIL_"
             "--exclude=.bzr"
             "--exclude=.hg"
             (or (mapcar (lambda (fn) (concat dir "/" fn)) files) (list dir))))
```
