This is my emacs customization organized as a set of org files.

## Installation Notes
The following are pre-requesites :

0. In Windows and Mac OS, install cygwin and brew respectively.
1. Pre-requesites on all systems
   - git
   - Mercurial
   - python
   - R
   - GNU global
2. Pre-requesites on windows/cygwin (see the specific .emacs file)
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
6. Install the latest version of org-mode using package-install
    package-install RET org RET
7. Replace .emacs with dot-emacs-OS.el and edit as required.

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
  Note that the above fix doesn't work with Mac OS X. Hence a manual tweaking
  is necessary.
2. In the same file, package-build-default-files-spec is updated to avoid throwing away *-test files. This was causing trouble with cider, which "cider-test.el" as a part of the package itself.
```
(defconst package-build-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el"))
  "Default value for :files attribute in recipes.")
```
