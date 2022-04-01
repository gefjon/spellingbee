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
                    (#:alex :alexandria)))
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
  (.all-letters-mask UFix)
  (.center-letter-mask UFix)
  (.center-letter Char)
  (.outside-letters String)
  (.available-words (List String))
  (.found-words (List String)))

(view:derive-eq (Eq Puzzle)
  (and .all-letters-mask
       .all-letters
       .center-letter-mask
       .center-letter
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
    (Puzzle mask
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

  (declare remove ((Eq :elt) => :elt -> (List :elt) -> (Optional (List :elt))))
  (define (remove elt lst)
    (match lst
      ((Nil) None)
      ((Cons fst rst)
       (if (== fst elt)
           (Some rst)
           (match (remove elt rst)
             ((None) None)
             ((Some rst) (Some (Cons fst rst))))))))

  (declare guess (Puzzle -> String -> (Optional Puzzle)))
  (define (guess puz word)
    (if (word-in-puzzle? (view:get .all-letters-mask puz)
                         (view:get .center-letter-mask puz)
                         word)
        (match (view:try-update .available-words (remove word) puz)
          ((None) None)
          ((Some puz) (view:update .found-words (Cons word) puz)))
        None)))

(coalton-toplevel
  (define-instance (file:Show Puzzle)
    (define (file:show! file puz)
      (let str = (file:write-string! file))
      (let char = (file:write-char! file))
      (let print = (file:show! file))
      (let line = (file:write-line! file))
      (let newline = (fn () (file:newline! file)))
      (let indent = (fn () (str "  ")))
      
      (line "found words:")
      (iter:for-each! (fn (word)
                        (indent)
                        (line word))
                      (iter:list-iter (view:get .found-words puz)))

      (str "center letter: ")
      (char (view:get .center-letter puz))
      (newline)
      (str "outside letters: ")
      (line (view:get .outside-letters puz))

      (str "remaining words: ")
      (print (length (view:get .available-words puz)))
      (newline))))

(coalton-toplevel
  (declare word-score (String -> UFix))
  (define (word-score word)
    (let len = (the UFix (fromInt (str:length word))))
    (if (== 4 len) 1
        (+ len
           (if (pangram? word) 7 0))))
  
  (declare run-puzzle! (file:Input -> file:Output -> Puzzle -> UFix))
  (define (run-puzzle! in out puz)
    (let ((loop
            (fn (puz score)
              (file:show! out puz)
              (let word = (expect "Unable to read a guess"
                                  (file:read-line! in)))
              (if (not (eligible-word? word))
                  (progn (file:write-line! out "Invalid guess; try again.")
                         (loop puz score))
                  (make-guess puz score word))))
          (make-guess
            (fn (puz score word)
              (match (guess puz word)
                ((Some puz) (correct-guess puz score word))
                ((None)
                 (progn (file:write-line! out
                                          "Nope! guess again.")
                        (loop puz score))))))
          (correct-guess
            (fn (puz score word)
              (let gained-score = (word-score word))
              (let new-score = (+ score gained-score))
              (file:write-string! out "Correct! Current score: ")
              (file:show! out new-score)
              (file:newline! out)
              (if (list:null? (view:get .available-words puz))
                  new-score
                  (loop puz new-score)))))
      (loop puz 0))))
