(uiop:define-package :spellingbee/view
  (:use :coalton :coalton-prelude)
  (:shadow #:when #:compose)
  (:local-nicknames (#:alex :alexandria)
                    (#:iter :coalton-library/iterator)
                    (#:vec :coalton-library/vector)
                    (#:trivia :trivia))
  (:export

   ;; utility macros
   #:when #:destructuring-let

   ;; type, constructor fundamental operators
   #:View #:inspect #:get #:replace #:set

   ;; mapping
   #:try-update #:update

   ;; composition
   #:compose

   ;; equality through views
   #:eq? #:neq?

   ;; views with predicates imposed
   #:guarded

   ;; views and view constructors
   #:.vector-at #:.stack-pusher #:.ok #:.err #:.some #:.none #:.id

   ;; type definition with views
   #:define-struct #:define-enum

   ;; `Eq' instances with views
   #:derive-eq))
(cl:in-package :spellingbee/view)

(cl:defmacro when (cond cl:&body body)
  `(if ,cond
       (Some (progn ,@body))
       None))

(cl:defmacro destructuring-let ((pat term) cl:&body body)
  `(match ,term
     (,pat (progn ,@body))
     (_ (error ,(cl:format cl:nil "term ~a failed to match pattern ~a in DESTRUCTURING-LET"
                           term pat)))))

(coalton-toplevel
  (define-type (View :container :elt)
    (View (:container -> (Optional :elt))
          (:container -> :elt -> (Optional :container))))

  (declare inspect ((View :container :elt) -> :container -> (Optional :elt)))
  (define (inspect view container)
    (destructuring-let ((View inspector _) view)
      (inspector container)))

  (declare get ((View :container :elt) -> :container -> :elt))
  (define (get view container)
    (expect "`inspect' returned none in `get'"
            (inspect view container)))

  (declare replace ((View :container :elt)
                    -> :container
                    -> :elt
                    -> (Optional :container)))
  (define (replace view container new-elt)
    (destructuring-let ((View _ replacer) view)
      (replacer container new-elt)))

  (declare set ((View :container :elt)
                -> :container
                -> :elt
                -> :container))
  (define (set view container new-elt)
    (expect "`replace' returned none in `set'"
            (replace view container new-elt)))

  (declare try-update ((View :container :elt)
                            -> (:elt -> (Optional :elt))
                            -> :container
                            -> (Optional :container)))
  (define (try-update view update-elt container)
    (match (inspect view container)
      ((None) None)
      ((Some elt) (match (update-elt elt)
                    ((Some new-elt) (replace view container new-elt))
                    ((None) None)))))

  (declare update ((View :container :elt)
                        -> (:elt -> :elt)
                        -> :container
                        -> (Optional :container)))
  (define (update view update-elt container)
    (try-update view (coalton-prelude:compose Some update-elt) container))

  (declare compose ((View :container :middle)
                         -> (View :middle :elt)
                         -> (View :container :elt)))
  (define (compose outer inner)
    (View (fn (container)
            (match (inspect outer container)
              ((Some middle) (inspect inner middle))
              (_ None)))
          (fn (container new-elt)
            (match (inspect outer container)
              ((Some middle) (match (replace inner middle new-elt)
                               ((Some new-middle) (replace outer container new-middle))
                               (_ None)))
              (_ None)))))

  (declare .vector-at (UFix -> (View (Vector :elt) :elt)))
  (define (.vector-at idx)
    (View (fn (vec) (vec:index (into idx) vec))
          (fn (vec new)
            (when (< (into idx) (vec:length vec))
              (let copied = (vec:copy vec))
              (vec:set! (into idx) new copied)
              copied))))

  (declare eq? ((Eq :elt) => (View :container :elt) -> :container -> :container -> Boolean))
  (define (eq? view left right)
    (match (Tuple (inspect view left)
                  (inspect view right))
      ((Tuple (Some left) (Some right))
       (== left right))
      (_ False)))

  (declare neq? ((Eq :elt) => (View :container :elt) -> :container -> :container -> Boolean))
  (define (neq? view left right)
    (match (Tuple (inspect view left)
                  (inspect view right))
      ((Tuple (Some left) (Some right))
       (/= left right))
      (_ False)))

  (declare guarded ((View :container :elt)
                         -> (:container -> :elt -> Boolean)
                         -> (View :container :elt)))
  (define (guarded unchecked-view ok?)
    (destructuring-let ((View inspector unchecked-replacer) unchecked-view)
      (View inspector
            (fn (container new-elt)
              (if (ok? container new-elt)
                  (unchecked-replacer container new-elt)
                  None)))))

  (declare .stack-pusher ((View (List :elt) :elt)))
  (define .stack-pusher
    "A view whose `inspect' returns the top item of the stack, and whose `replace' adds the new item to the top of the stack."
    (View head
          (fn (lst new-head) (Some (Cons new-head lst)))))

  (declare .ok (View (Result :err :ok) :ok))
  (define .ok
    (View (fn (res) (match res
                      ((Ok elt) (Some elt))
                      (_ None)))
          (fn (_ new) (Some (Ok new)))))

  (declare .err (View (Result :err :ok) :err))
  (define .err
    (View (fn (res) (match res
                      ((Err elt) (Some elt))
                      (_ None)))
          (fn (_ new) (Some (Err new)))))

  (declare .some (View (Optional :elt) :elt))
  (define .some
    (View id
          (fn (_ new) (Some (Some new)))))

  (declare .none (View (Optional :elt) Unit))
  (define .none
    (View (fn (opt) (if (isNone opt) (Some Unit) None))
          (fn (_ _) (Some None))))

  (declare .id (View :any :any))
  (define .id
    (View Some
          (fn (_ thing) (Some thing)))))

(cl:defmacro define-struct (name-and-type-params cl:&body slots)
  (cl:let ((name (cl:etypecase name-and-type-params
                   (cl:symbol name-and-type-params)
                   (cl:list (cl:first name-and-type-params))))
           (n-slots (cl:length (cl:the cl:list slots))))
    (cl:flet ((constructor-lens (view-name field-type index)
                (alex:with-gensyms (container field-name new-value getter setter)
                  (cl:let* ((setter-match-names (cl:loop :for i :below n-slots
                                                   :when (cl:= i index)
                                                     :collect '_
                                                   :else :collect (cl:gensym)))
                            (setter-ctor-names (cl:loop :for i :below n-slots
                                                  :for name :in setter-match-names
                                                  :when (cl:= i index)
                                                    :collect field-name
                                                  :else :collect name))
                            (getter-match-names (cl:loop :for i :below n-slots
                                                   :when (cl:= i index)
                                                     :collect field-name
                                                   :else :collect '_)))
                    `((declare ,view-name (View ,name-and-type-params ,field-type))
                      (define ,view-name
                        (let ((,getter (fn (,container)
                                         (destructuring-let ((,name ,@getter-match-names)
                                                             ,container)
                                           (Some ,field-name))))
                              (,setter (fn (,container ,new-value)
                                         (destructuring-let ((,name ,@setter-match-names)
                                                             ,container)
                                           (let ((,field-name ,new-value))
                                             (Some (,name ,@setter-ctor-names)))))))
                          (View ,getter ,setter))))))))
      `(coalton-toplevel
         (define-type ,name-and-type-params
           (,name ,@(cl:mapcar #'cl:second slots)))
         ,@(cl:loop
              :for (lens-name slot-type) :in slots
              :for idx :below n-slots
              :append (constructor-lens lens-name slot-type idx))))))

(cl:defmacro define-enum (type-name-and-params cl:&body variants)
  (cl:flet ((emit-variant (variant)
              (cl:etypecase variant
                (cl:symbol variant)
                (cl:list (cl:rest variant))))
            (variant-prism (variant)
              (cl:etypecase variant
                (cl:symbol nil)
                (cl:list (cl:destructuring-bind (view-name variant-name inner-type) variant
                           (alex:with-gensyms (outer inner replacer inspector)
                             `((declare ,view-name (View ,type-name-and-params ,inner-type))
                               (define ,view-name
                                 (let ((,inspector (fn (,outer)
                                                     (match ,outer
                                                       ((,variant-name ,inner) (Some ,inner))
                                                       (_ None))))
                                       (,replacer (fn (_ ,inner)
                                                    (Some (,variant-name ,inner)))))
                                   (View ,inspector ,replacer))))))))))
    `(coalton-toplevel
       (define-type ,type-name-and-params
         ,@(cl:mapcar #'emit-variant variants))
       ,@(cl:mapcan #'variant-prism variants))))

(cl:defmacro derive-eq (instance-identifier cl:&body (view-expr))
  (alex:with-gensyms (left right)
    (cl:labels ((compile-and-expression (exprs)
                  `(and ,@(cl:mapcar #'compile-viewed-eq-expression exprs)))
                (compile-or-expression (exprs)
                  `(or ,@(cl:mapcar #'compile-viewed-eq-expression exprs)))
                (compile-viewed-eq-expression (expr)
                  (trivia:match expr
                    ((cl:cons 'and and-views)
                     (compile-and-expression and-views))
                    ((cl:cons 'or or-views)
                     (compile-or-expression or-views))
                    (view `(eq? ,view ,left ,right)))))
      `(coalton-toplevel
         (define-instance ,instance-identifier
           (define (== ,left ,right)
             ,(compile-viewed-eq-expression view-expr)))))))
