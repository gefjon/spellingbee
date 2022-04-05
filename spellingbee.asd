(defsystem "spellingbee"
  :class :package-inferred-system
  :author "phoebe Goldman <phoebe@goldman-tribe.org>"
  :depends-on ("spellingbee/package"))

(asdf:register-system-packages "coalton"
                               '(:coalton-prelude
                                 :coalton-library/list
                                 :coalton-library/bits
                                 :coalton-library/char
                                 :coalton-library/vector
                                 :coalton-library/string
                                 :coalton-library/iterator
                                 :coalton-library/cell))
