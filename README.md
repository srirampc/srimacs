
This is my emacs customization organized as a set of org files.

## Installation Notes
The following are pre-requesites :

0. In Windows, install cygwin and install utils not including
     openssl
     gnutls
     tar 
     make 
1. Install the latest version of org-mode using package-install
    package-install RET org RET
2. Install the latest version of package-build from melpa from 
   http://melpa.milkbox.net/#/package-build
3. Patch package-build. Change the pb/create-tar function from
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
  to the following
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
    
