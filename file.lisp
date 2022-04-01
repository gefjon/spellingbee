(uiop:define-package :spellingbee/file
  (:use :coalton-prelude :coalton)
  (:local-nicknames (#:iter :coalton-library/iterator))
  (:export

   ;; encodings
   #:Encoding #:UTF-8 #:UTF-16 #:LATIN-1

   ;; input files
   #:Input
   #:open-input!
   #:close-input!
   #:open-input?
   #:read-char!
   #:read-line!
   #:iterator!
   #:chars!
   #:lines!
   #:call-with-input-file #:with-input-file
   #:standard-input
   #:call-with-standard-input #:with-standard-input

   ;; output files
   #:Output
   #:open-output!
   #:close-output!
   #:open-output?
   #:write-char!
   #:write-string!
   #:write-line!
   #:newline!
   #:call-with-output-file #:with-output-file
   #:standard-output
   #:call-with-standard-output #:with-standard-output

   ;; formatted output
   #:Show #:show!))
(cl:in-package :spellingbee/file)

;; encodings

(coalton-toplevel
  (define-type Encoding
    UTF-8
    UTF-16
    LATIN-1)

  (declare encoding-name (Encoding -> Lisp-Object))
  (define (encoding-name enc)
    (match enc
      ((UTF-8) (lisp Lisp-Object () :utf-8))
      ((UTF-16) (lisp Lisp-Object () :utf-16))
      ((LATIN-1) (lisp Lisp-Object () :latin-1)))))

;; input files

(coalton-toplevel
 (define-type Input
   (%Input Lisp-Object))

 (declare open-input! (Encoding -> String -> (Optional Input)))
 (define (open-input! enc path)
   (let ((enc-name (encoding-name enc)))
     (lisp (Optional Input) (path enc-name)
       (cl:handler-case
           (cl:open path
                    :direction :input
                    :element-type 'cl:character
                    :if-does-not-exist :error
                    :external-format enc-name)
         (cl:file-error () None)
         (:no-error (f) (Some (%Input f)))))))

 (declare close-input! (Input -> Unit))
 (define (close-input! file)
   (match file
     ((%Input f) (progn (lisp :any (f)
                          (cl:close f))
                        Unit))))

 (declare open-input? (Input -> Boolean))
 (define (open-input? file)
   (match file
     ((%Input f) (lisp Boolean (f)
                   (cl:open-stream-p f)))))

 (declare read-char! (Input -> (Optional Char)))
 (define (read-char! file)
   (match file
     ((%Input f)
      (lisp (Optional Char) (f)
        (cl:handler-case
            (cl:read-char f cl:t cl:nil cl:nil)
          (cl:end-of-file () None)
          (:no-error (c) (Some c)))))))

 (declare read-line! (Input -> (Optional String)))
 (define (read-line! file)
   (match file
     ((%Input f)
      (lisp (Optional String) (f)
        (cl:handler-case
            (cl:read-line f cl:t cl:nil cl:nil)
          (cl:end-of-file () None)
          (:no-error (l missing-newline-p)
            (cl:declare (cl:ignore missing-newline-p))
            (Some l)))))))

 (declare iterator! ((Input -> (Optional :elt))
                     -> Input
                     -> (iter:Iterator :elt)))
 (define (iterator! read-elt! file)
   "Returns an iterator over elements of FILE by READ-ELT!, closing FILE after READ-ELT! returns None for the first time."
   (iter:new
    (fn ()
      (if (not (open-input? file))
          None
          (match (read-elt! file)
            ((Some elt) (Some elt))
            ((None) (progn (close-input! file)
                           None)))))))

 (declare chars! (Input -> (iter:Iterator Char)))
 (define (chars! file)
   (iterator! read-char! file))

 (declare lines! (Input -> (iter:Iterator String)))
  (define (lines! file)
   (iterator! read-line! file))

  (declare call-with-input-file (Encoding -> String -> (Input -> :res) -> (Optional :res)))
  (define (call-with-input-file enc path thunk)
    "Open PATH as an ENC-encoded input file, call THUNK with the file, then close the file."
    (match (open-input! enc path)
      ((Some file)
       (progn (let res = (thunk file))
              (close-input! file)
              (Some res)))
      ((None) None)))

 (declare standard-input (Unit -> Input))
 (define (standard-input)
   (%Input (lisp Lisp-Object () cl:*standard-input*)))

 (declare call-with-standard-input (Input -> (Unit -> :ret) -> :ret))
 (define (call-with-standard-input inp thunk)
   (match inp
     ((%Input stream)
      (lisp :ret (stream thunk)
        (cl:let ((cl:*standard-input* stream))
          (coalton-impl/codegen/function-entry:a1 thunk Unit)))))))

(cl:defmacro with-input-file ((var path cl:&optional (enc 'UTF-8)) cl:&body body)
  `(call-with-input-file ,enc ,path (fn (,var) ,@body)))

(cl:defmacro with-standard-input ((stream) cl:&body body)
  `(call-with-standard-input ,stream (fn () ,@body)))

;; output files

(coalton-toplevel
  (define-type Output
    (%Output Lisp-Object))

  (declare open-output! (Encoding -> String -> (Optional Output)))
  (define (open-output! enc path)
    "Open PATH for output using ENC as an encoding, replacing if it exists."
    (let enc-name = (encoding-name enc))
    (lisp (Optional Output) (enc-name path)
      (cl:handler-case
           (cl:open path
                    :direction :output
                    :element-type 'cl:character
                    :if-exists :supersede
                    :external-format enc-name)
         (cl:file-error () None)
         (:no-error (f) (Some (%Output f))))))

  (declare close-output! (Output -> Unit))
  (define (close-output! file)
    (match file
      ((%Output f) (progn (lisp :any (f)
                            (cl:close f))
                          Unit))))

  (declare open-output? (Output -> Boolean))
  (define (open-output? file)
    (match file
      ((%Output f) (lisp Boolean (f)
                     (cl:open-stream-p f)))))

  (declare write-char! (Output -> Char -> Unit))
  (define (write-char! file c)
    (match file
      ((%Output f) (progn (lisp :any (f c)
                            (cl:write-char c f))
                          Unit))))

  (declare write-string! (Output -> String -> Unit))
  (define (write-string! file str)
    (match file
      ((%Output f) (progn (lisp :any (f str)
                            (cl:write-string str f))
                          Unit))))

  (declare write-line! (Output -> String -> Unit))
  (define (write-line! file str)
    (match file
      ((%Output f) (progn (lisp :any (f str)
                            (cl:write-line str f))
                          Unit))))

  (declare newline! (Output -> Unit))
  (define (newline! file)
    (match file
      ((%Output f) (progn (lisp :any (f)
                            (cl:terpri f))
                          Unit))))

  (declare call-with-output-file (Encoding -> String -> (Output -> :res) -> (Optional :res)))
  (define (call-with-output-file enc path thunk)
    (match (open-output! enc path)
      ((Some file)
       (progn (let res = (thunk file))
              (close-output! file)
              (Some res)))
      ((None) None)))

  (declare standard-output (Unit -> Output))
  (define (standard-output)
    (%Output (lisp Lisp-Object () cl:*standard-output*)))

  (declare call-with-standard-output (Output -> (Unit -> :res) -> :res))
  (define (call-with-standard-output file thunk)
    (match file
      ((%Output f)
       (lisp :res (f thunk)
         (cl:let ((cl:*standard-output* f))
           (coalton-impl/codegen/function-entry:a1 thunk Unit)))))))

(cl:defmacro with-output-file ((var path cl:&optional (enc 'UTF-8)) cl:&body body)
  `(call-with-output-file ,enc ,path (fn (,var) ,@body)))

(cl:defmacro with-standard-output ((file) cl:&body body)
  `(call-with-standard-output ,file (fn () ,@body)))

;; formatted output

(coalton-toplevel
  (define-class (Show :showable)
    (show! (Output -> :showable -> Unit)))

  (define-instance (Show Char)
    (define show! write-char!))

  (define-instance (Show String)
    (define show! write-string!)))

(cl:defmacro define-show-integer (type)
  `(coalton-toplevel
     (define-instance (Show ,type)
       (define (show! file int)
         (match file
           ((%Output f)
            (lisp :any (f int)
              (cl:format f "~d" int))))
         Unit))))

(define-show-integer UFix)

(define-show-integer Integer)
