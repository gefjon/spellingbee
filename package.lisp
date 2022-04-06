(uiop:define-package :spellingbee/package
  (:use :coalton-prelude :coalton)
  (:local-nicknames (#:iter :coalton-library/iterator)
                    (#:file :spellingbee/file)
                    (#:view :spellingbee/view)
                    (#:list :coalton-library/list)
                    (#:str :coalton-library/string)
                    (#:vec :coalton-library/vector)
                    (#:bit :coalton-library/bits)
                    (#:char :coalton-library/char)
                    (#:alex :alexandria)
                    (#:cell :coalton-library/cell)
                    (#:ht :coalton-library/hashtable)))
(cl:in-package :spellingbee/package)

;;; iter extensions

(coalton-toplevel
  (declare filter-map! ((:src -> (Optional :tgt))
                        -> (iter:Iterator :src)
                        -> (iter:Iterator :tgt)))
  (define (filter-map! trans iter)
    (let ((advance! (fn (_)
                      (match (iter:next! iter)
                        ((Some src) (match (trans src)
                                      ((Some new) (Some new))
                                      ((None) (advance!))))
                        ((None) None)))))
      (iter:new advance!)))

  (declare collect-string! ((iter:Iterator Char) -> String))
  (define (collect-string! iter)
    (into (iter:collect-list! iter))))

;;; bits extensions

(coalton-toplevel
  (declare bit? (UFix -> UFix -> Boolean))
  (define (bit? idx in)
    (lisp Boolean (idx in)
      (cl:logbitp idx in))))

;;; the actual impl

(coalton-toplevel
  (declare english-char? (Char -> Boolean))
  (define (english-char? c)
    (and (>= c #\a)
         (<= c #\z)))

  (declare letter-mask (Char -> UFix))
  (define (letter-mask letter)
    (bit:shift (into (- (char:char-code letter)
                        (char:char-code #\a)))
               (the UFix 1)))

  (declare word-mask (String -> UFix))
  (define (word-mask word)
    (iter:fold! bit:or 0 (map letter-mask (iter:string-chars word))))

  (declare mask-letters (UFix -> String))
  (define (mask-letters mask)
    (collect-string!
     (filter-map! (fn (i)
                    (view:when (bit? i mask)
                      (expect "Bad char code"
                              (char:code-char (+ i (char:char-code #\a))))))
                  (iter:up-to 26))))

  (declare count-ones (Ufix -> UFix))
  (define (count-ones u)
    (lisp UFix (u)
      (cl:logcount u)))

  (declare count-unique-letters (String -> UFix))
  (define (count-unique-letters word)
    (count-ones (word-mask word)))

  (declare eligible-word? (String -> Boolean))
  (define (eligible-word? word)
    (and (<= 4 (str:length word))
         (iter:every! english-char? (iter:string-chars word))
         (>= 7 (count-unique-letters word))))

  (declare pangram? (String -> Boolean))
  (define (pangram? word)
    (== 7 (count-unique-letters word)))

  (declare read-file-data! (String -> (iter:Iterator String)))
  (define (read-file-data! input-path)
    (pipe input-path
      (file:open-input! file:LATIN-1)
      (expect (str:concat "Unable to open file " input-path))
      file:lines!
      (iter:filter! eligible-word?)))

  (declare read-data! ((iter:Iterator String) -> (Vector String)))
  (define (read-data! input-paths)
    (pipe input-paths
      (map read-file-data!)
      iter:flatten!
      iter:remove-duplicates!
      iter:collect-vector!))

  (declare relevant-scowl-filenames (List String))
  (define relevant-scowl-filenames
    (make-list "american-words.10"
               "american-words.20"
               "american-words.35"
               "american-words.40"
               "american-words.50"
               "american-words.55"
               "english-words.10"
               "english-words.20"
               "english-words.35"
               "english-words.40"
               "english-words.50"
               "english-words.55"))

  (declare scowl-data-path String)
  (define scowl-data-path
    (lisp String ()
      (cl:namestring
       (asdf:system-relative-pathname "spellingbee"
                                      "scowl-2020.12.07/final/"
                                      :type :directory)))))

(coalton-toplevel
  (declare all-words (Vector String))
  (define all-words
    (read-data! (map (str:concat scowl-data-path)
                     (iter:list-iter relevant-scowl-filenames))))

  (declare pangrams (Vector String))
  (define pangrams
    (pipe all-words
      iter:vector-iter
      (iter:filter! pangram?)
      iter:collect-vector!)))

(view:define-struct Puzzle
  (.score UFix)
  (.all-letters-mask UFix)
  (.center-letter-mask UFix)
  (.center-letter Char)
  (.outside-letters String)
  (.available-words (List String))
  (.found-words (List String)))

(view:derive-eq (Eq Puzzle)
  (and .score
       .all-letters-mask
       .center-letter-mask
       .center-letter
       .outside-letters
       .available-words
       .found-words))

(coalton-toplevel
  (declare word-mask-subset? (UFix -> UFix -> Boolean))
  (define (word-mask-subset? sub super)
    (== super (bit:or sub super)))

  (declare word-fits-mask? (UFix -> String -> Boolean))
  (define (word-fits-mask? mask word)
    (word-mask-subset? (word-mask word) mask))

  (declare word-in-puzzle? (UFix -> UFix -> String -> Boolean))
  (define (word-in-puzzle? all-letters center-letter word)
    (let mask = (word-mask word))
    (and (word-mask-subset? mask all-letters)
         (word-mask-subset? center-letter mask)))

  (declare random-char (String -> (Optional Char)))
  (define (random-char str)
    (view:when (> (str:length str) 0)
      (lisp Char (str)
        (alex:random-elt str))))

  (declare make-puzzle (String -> Char -> Puzzle))
  (define (make-puzzle pangram center-letter)
    (let mask = (word-mask pangram))
    (let center-mask = (letter-mask center-letter))
    (let outside-mask = (bit:xor mask center-mask))
    (let outside-letters = (mask-letters outside-mask))
    (let words = (pipe all-words
                   iter:vector-iter
                   (iter:filter! (word-in-puzzle? mask center-mask))
                   iter:collect-list!))
    (Puzzle 0
            mask
            center-mask
            center-letter
            outside-letters
            words
            Nil))

  (declare pangram-puzzle (String -> Puzzle))
  (define (pangram-puzzle pangram)
    (let letters = (mask-letters (word-mask pangram)))
    (let center-letter = (unwrap (random-char letters)))
    (make-puzzle pangram center-letter))

  (declare random-elt ((Vector :elt) -> (Optional :elt)))
  (define (random-elt vec)
    (view:when (> (vec:length vec) 0)
      (lisp :elt (vec)
        (alex:random-elt vec))))

  (declare random-puzzle (Unit -> Puzzle))
  (define (random-puzzle)
    (pangram-puzzle (expect "No available pangrams"
                            (random-elt pangrams))))

  (declare word-score (String -> UFix))
  (define (word-score word)
    (let len = (the UFix (fromInt (str:length word))))
    (if (== 4 len) 1
        (+ len
           (if (pangram? word) 7 0))))

  (declare remove-if ((:elt -> Boolean) -> (List :elt) -> (Optional (List :elt))))
  (define (remove-if this? lst)
    (match lst
      ((Nil) None)
      ((Cons fst rst)
       (if (this? fst)
           (Some rst)
           (match (remove-if this? rst)
             ((None) None)
             ((Some rst) (Some (Cons fst rst))))))))

  (declare remove ((Eq :elt) => (:elt -> (List :elt) -> (Optional (List :elt)))))
  (define (remove elt lst)
    (remove-if (== elt) lst))

  (declare guess (Puzzle -> String -> (Optional Puzzle)))
  (define (guess puz word)
    (if (word-in-puzzle? (view:get .all-letters-mask puz)
                         (view:get .center-letter-mask puz)
                         word)
        (match (view:try-update .available-words (remove word) puz)
          ((None) None)
          ((Some puz)
           (view:update .score
                        (+ (word-score word))
                        (unwrap (view:update .found-words
                                             (Cons word)
                                             puz)))))
        None)))

;; printing puzzles

(coalton-toplevel
  (define-instance (file:Show Puzzle)
    (define (file:show! file puz)
      (let str = (file:write-string! file))
      (let char = (file:write-char! file))
      (let print = (file:show! file))
      (let line = (file:write-line! file))
      (let newline = (fn () (file:newline! file)))

      (str "current score: ")
      (print (view:get .score puz))
      (newline)

      (str "center letter: ")
      (char (view:get .center-letter puz))
      (newline)
      (str "outside letters: ")
      (line (view:get .outside-letters puz))

      (str "remaining words: ")
      (print (fromInt (length (view:get .available-words puz))))
      (newline))))

;; commands during gameplay

(view:define-struct Command
  (.name String)
  (.description String)
  (.action (file:Input
            -> file:Output
            -> Puzzle
            -> Unit)))

(coalton-toplevel
  (define-instance (file:Show Command)
    (define (file:show! file com)
      (match com
        ((Command name description _)
         (progn
           (file:write-char! file #\:)
           (file:write-string! file name)
           (file:write-string! file " - ")
           (file:write-line! file description))))))

  (declare commands (cell:Cell (List Command)))
  (define commands (cell:new Nil))

  (declare list-replace ((:elt -> Boolean) -> :elt -> (List :elt) -> (List :elt)))
  (define (list-replace old? new lst)
    (Cons new (with-default lst (remove-if old? lst))))

  (declare install-command (Command -> Unit))
  (define (install-command com)
    (let same? = (view:eq? .name com))
    (cell:update! (list-replace same? com)
                  commands)
    Unit)

  (declare input-command? (String -> Boolean))
  (define (input-command? input)
    (== #\:
        (with-default #\space
          (str:ref input 0))))

  (declare skipping-substring? (UFix -> String -> String -> Boolean))
  (define (skipping-substring? skip super sub)
    (lisp Boolean (skip super sub)
      (cl:not (cl:not (cl:string= sub super :start2 skip)))))

  (declare find-command (String -> (Optional Command)))
  (define (find-command input)
    (let this? =
      (compose (skipping-substring? 1 input)
               (view:get .name)))
    (iter:find! this? (iter:list-iter (cell:read commands))))

  (declare run-command! (Command -> file:Input -> file:Output -> Puzzle -> Unit))
  (define (run-command! com)
    (view:get .action com)))

(cl:defmacro define-command (name description (in out puz) cl:&body body)
  (cl:let ((process-command (alex:symbolicate '#:do- name))
           (command-obj (alex:symbolicate '#:command- name)))
    `(cl:progn
       (coalton-toplevel
         (declare ,process-command (file:Input -> file:Output -> Puzzle -> Unit))
         (define (,process-command ,in ,out ,puz)
           ,@body)
         (declare ,command-obj Command)
         (define ,command-obj (Command ,name ,description ,process-command)))
       (cl:eval-when (:load-toplevel)
         (coalton (install-command ,command-obj))))))

;; displaying already-found words

(coalton-toplevel
  (declare indented-word-list! (file:Output -> (List String) -> Unit))
  (define (indented-word-list! file words)
    (let indent = (fn () (file:write-string! file "  ")))
    (let line = (file:write-line! file))

    (let print-word =
      (fn (word)
        (indent)
        (line word)))

    (iter:for-each! print-word
                    (iter:list-iter words)))

  (declare print-found-words! (file:Output -> (List String) -> Unit))
  (define (print-found-words! file words)
    (file:write-line! file "found words:")
    (indented-word-list! file words))

  (declare print-spoiler! (file:Output -> (List String) -> Unit))
  (define (print-spoiler! file words)
    (file:write-line! file "remaining words:")
    (indented-word-list! file words)))

(define-command "found" "show words you've already found"
    (_ out puz)
  (print-found-words! out (view:get .found-words puz)))

(define-command "spoil" "show remaining words"
    (_ out puz)
  (print-found-words! out (view:get .available-words puz)))

;; bigram-based hints

(coalton-toplevel
  (declare leading-bigram (String -> String))
  (define (leading-bigram word)
    (str:substring word 0 2))

  (declare starts-with? (String -> String -> Boolean))
  (define (starts-with? start str)
    (let len = (str:length start))
    (lisp Boolean (start str len)
      (cl:not (cl:not ; hackery: `string=' is listed in the spec as returning "generalized boolean", so any
                      ; non-`nil' object may be used to denote truth. `not not' coerces this into an actual
                      ; `boolean'.
               (cl:string= start str :end2 len)))))

  (declare bigrams ((List String) -> (List String)))
  (define (bigrams words)
    (pipe words
      iter:list-iter
      (map leading-bigram)
      iter:remove-duplicates!
      iter:collect-list!
      list:sort))

  (declare bigram-words ((List String) -> String -> UFix))
  (define (bigram-words words bigram)
    (pipe words
      iter:list-iter
      (iter:filter! (starts-with? bigram))
      iter:count!
      fromInt))

  (declare print-bigrams! (file:Output -> (List String) -> Unit))
  (define (print-bigrams! file words)
    (let str = (file:write-string! file))
    (let print = (file:show! file))
    (let newline = (fn () (file:newline! file)))

    (let print-bigram =
      (fn (bi)
        (str bi)
        (str ": ")
        (print (bigram-words words bi))
        (newline)))

    (iter:for-each! print-bigram
                    (iter:list-iter (bigrams words)))))

(define-command "bigrams" "display the number of remaining words for each leading bigram"
    (_ out puz)
  (print-bigrams! out (view:get .available-words puz)))

;; help message

(define-command "help" "print this message"
    (_ out puz)
  (file:write-line! out "Type a word to guess it.
Available commands are:")
  (let print-command =
    (fn (com)
      (file:write-string! out "  ")
      (file:show! out com)))
  (iter:for-each! print-command
                  (iter:list-iter (cell:read commands))))

;; matrix-based hints

(coalton-toplevel
  (declare ht-get-or-insert! ((Hash :key) =>
                              (:key -> :value)
                              -> (Hashtable :key :value)
                              -> :key
                              -> :value))
  (define (ht-get-or-insert! default ht key)
    (match (ht:get ht key)
      ((Some val) val)
      ((None) (progn (let val = (default key))
                     (ht:set! ht key val)
                     val))))

  (declare vec-of-zeros (UFix -> (Vector UFix)))
  (define (vec-of-zeros len)
    (lisp (Vector UFix) (len)
      (cl:make-array len
                     :initial-element 0
                     :element-type 'cl:t
                     :adjustable 'cl:t
                     :fill-pointer len)))

  (declare vec-update! ((:elt -> :elt) -> (Vector :elt) -> Integer -> Unit))
  (define (vec-update! func vec idx)
    (let old = (expect "OOB in `vec-update!'"
                       (vec:index idx vec)))
    (let new = (func old))
    (vec:set! idx new vec))

  (declare compute-matrix ((List String) -> (Vector (Tuple Char (Vector UFix)))))
  (define (compute-matrix words)
    (let ht = (ht:with-capacity 7))
    (let longest = (into (expect "No words in `compute-matrix'"
                                 (iter:max! (map str:length (iter:list-iter words))))))

    (let make-vec =
      (the (:any -> (Vector UFix))
           (fn (_)
             (vec-of-zeros (- longest 3)))))

    (let get-vec =
      (fn (char)
        (ht-get-or-insert! make-vec ht char)))

    (iter:for-each!
     (fn (word)
       (let first-letter = (expect "Word of length zero in `compute-matrix'"
                                   (str:ref word 0)))
       (let vec = (get-vec first-letter))
       (let len = (str:length word))
       (vec-update! 1+ vec (- len 4)))

     (iter:list-iter words))

    (into (the (List (Tuple Char (Vector UFix))) (ht:entries ht))))

  (declare write-width-3! (file:Output -> Integer -> Unit))
  (define (write-width-3! out int)
    (view:destructuring-let ((file::%Output stream) out)
      (lisp :any (stream int)
        (cl:format stream "~3d" int)))
    Unit)

  (declare dotimes ((Num :int) (Ord :int) => :int -> (:int -> :any) -> Unit))
  (define (dotimes n thunk)
    (iter:for-each! thunk
                    (iter:up-to n)))

  (declare print-horiz-separator! ((Num :int) (Ord :int) => (file:Output -> :int -> Unit)))
  (define (print-horiz-separator! out length-in-cells)
    (dotimes (* 3 length-in-cells)
      (fn (_) (file:write-char! out #\-)))
    (file:newline! out))

  (declare print-header! (file:Output -> (Vector (Tuple Char (Vector UFix))) -> Unit))
  (define (print-header! out mat)
    (file:write-string! out "   ")
    (print-vert-separator! out)
    (dotimes (1- (vec:length mat))
      (fn (idx)
        (write-width-3! out (+ 4 idx))))
    (print-vert-separator! out)
    (write-centered-3! out #\greek_capital_letter_sigma)
    (file:newline! out))

  (declare write-centered-3! (file:Output -> Char -> Unit))
  (define (write-centered-3! out ch)
    (file:write-char! out #\space)
    (file:write-char! out ch)
    (file:write-char! out #\space))

  (declare print-vert-separator! (file:Output -> Unit))
  (define (print-vert-separator! file)
    (write-centered-3! file #\|))

  (declare print-row! (file:Output -> Char -> (Vector UFix) -> Unit))
  (define (print-row! out ch row)
    (write-centered-3! out ch)
    (print-vert-separator! out)

    (iter:for-each! (fn (count)
                      (write-width-3! out (into count)))
                    (iter:vector-iter row))

    (print-vert-separator! out)

    (write-width-3! out (into (iter:sum! (iter:vector-iter row))))

    (file:newline! out))

  (declare print-footer! (file:Output -> (Vector (Tuple Char (Vector UFix))) -> Integer -> Unit))
  (define (print-footer! out mat sum)
    (write-centered-3! out #\greek_capital_letter_sigma)
    (print-vert-separator! out)

    (let column-sum =
      (fn (col-idx)
        (into (iter:sum! (map (compose (expect "Row too short in `print-footer!' `column-sum'")
                                       (compose (vec:index col-idx)
                                                snd))
                              (iter:vector-iter mat))))))

    (view:destructuring-let ((Tuple _ row0) (expect "no rows in `print-footer!'"
                                                    (vec:index 0 mat)))
      (dotimes (vec:length row0)
        (compose (write-width-3! out) column-sum)))
    (print-vert-separator! out)
    (write-width-3! out sum)
    (file:newline! out))

  (declare sort-matrix! ((Vector (Tuple Char (Vector UFix)))
                         -> (Vector (Tuple Char (Vector UFix)))))
  (define (sort-matrix! mat)
    (vec:sort-by! (fn (left right)
                    (< (fst left) (fst right)))
                  mat)
    mat)

  (declare print-matrix! (file:Output -> (List String) -> Unit))
  (define (print-matrix! out words)
    (let mat = (sort-matrix! (compute-matrix words)))
    (let ncells = (+ (fromInt (vec:length mat))
                     3 ; one for the letters on the left, one for the sum on the right, two for vertical
                       ; separators on each, minus one because vec:length is off-by-one
                     ))
    (print-header! out mat)
    (print-horiz-separator! out ncells)
    (iter:for-each! (uncurry (print-row! out))
                    (iter:vector-iter mat))
    (print-horiz-separator! out ncells)
    (print-footer! out mat (list:length words))))

(define-command "matrix" "display a matrix of counts of remaining words by first letter and length"
    (_ out puz)
  (print-matrix! out (view:get .available-words puz)))

;; number of pangrams available

(define-command "pangrams" "display the number of available pangrams"
    (_ out puz)
  (let count-by! =
    (fn (tst?)
      (iter:count! (iter:filter! tst? (iter:list-iter (view:get .available-words puz))))))

  (let pangrams = (count-by! pangram?))
  (let perfect-pangrams = (count-by! (fn (word)
                                       (and (pangram? word)
                                            (== (str:length word) 7)))))
  (file:write-string! out "pangrams: ")
  (file:show! out pangrams)
  (when (> perfect-pangrams 0)
    (file:write-char! out #\()
    (file:show! out perfect-pangrams)
    (file:write-string! out " perfect)"))
  (file:newline! out))

;; the game itself

(coalton-toplevel
  (declare run-puzzle! (file:Input -> file:Output -> Puzzle -> Puzzle))
  (define (run-puzzle! in out puz)
    (let ((loop
            (fn (puz)
              (file:show! out puz)
              (prompt)
              (let word = (expect "Unable to read a guess"
                                  (file:read-line! in)))
              (cond
                ((input-command? word)
                 (command-dispatch word puz))
                ((not (eligible-word? word))
                 (progn (file:write-line! out "Invalid guess; try again.")
                        (loop puz)))
                (True (make-guess puz word)))))

          (prompt (fn () (file:write-string! out "> ")))

          (command-dispatch
            (fn (com puz)
              (match (find-command com)
                ((Some com)
                 (run-command! com in out puz))
                ((None) (file:write-line! out "Unknown command. Try :help to see a list of available commands.")))
               (loop puz)))

          (make-guess
            (fn (puz word)
              (match (guess puz word)
                ((Some puz) (correct-guess puz word))
                ((None)
                 (progn (file:write-line! out
                                          "Nope! guess again.")
                        (loop puz))))))

          (correct-guess
            (fn (puz word)
              (let gained-score = (word-score word))
              (file:write-string! out "Correct! Scored ")
              (file:show! out gained-score)
              (file:write-line! out " points.")
              (if (list:null? (view:get .available-words puz))
                  puz
                  (loop puz)))))

      (loop puz))))
