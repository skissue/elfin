# Byte-compile all Emacs Lisp files
compile:
    emacs -Q --batch -L . -L extras -f batch-byte-compile *.el extras/*.el

# Run offline ERT tests against source, ignoring existing bytecode
test:
    emacs -Q --batch -l tests/run-tests.el

# Delete all byte-compiled files
clean:
    rm -f *.elc extras/*.elc

# Load package sources into the running Emacs
load:
    emacsclient --eval '(dolist (dir (list "{{justfile_directory()}}" "{{justfile_directory()}}/extras")) (dolist (f (directory-files dir t "\\.el$")) (load-file f)))'
