# Byte-compile all Emacs Lisp files
compile:
    emacs -Q --batch -L . -L extras -f batch-byte-compile *.el extras/*.el

# Delete all byte-compiled files
clean:
    rm -f *.elc extras/*.elc

# Load all source files into the running Emacs instance
load:
    emacsclient --eval '(dolist (f (directory-files-recursively "{{justfile_directory()}}" "\\.el$")) (load-file f))'
