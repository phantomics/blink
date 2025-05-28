;;;; utilities.lisp

(in-package #:blink)

(defvar *blink-idiom* nil)

(define-symbol-macro this-idiom *ktest-idiom*)

(defvar *idiom-native-symbols* '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵ ∇ ∇∇ index-origin print-precision *digit-vector*
                                 *alphabet-vector* *apl-timestamp* to-output output-stream))

;; (defvar *system-variables* '(:index-origin *index-origin* :print-precision *print-precision*
;;                              :comparison-tolerance *comparison-tolerance* :division-method *division-method*
;;                              :rngs *rngs*))

(defvar *system-variables* '(:index-origin *index-origin* :print-precision *print-precision*
                             :comparison-tolerance *comparison-tolerance* ;; :rngs *rngs*
                             ))

(defparameter *package-name-string* (package-name *package*))


(let ((this-package (package-name *package*)))
  (defmacro in-blink-workspace (name &body body)
    "Macro that interns symbols in the current workspace; works in tandem with ⊏ reader macro."
    (let* ((space-name (concatenate 'string "BLINK-WORKSPACE-" (string-upcase name)))
           (lex-space-name (concatenate 'string space-name "-LEX"))
           ;; build list of values assigned in the (april) call; these are stored as dynamic vars
           (top-level-instrs (mapcar (lambda (item) (string (cadar item))) (cdadar body)))
           (symacro-lex) (symacro-syms))
      (labels ((replace-symbols (form &optional inside-function)
                 (loop :for item :in form :for ix :from 0
                    :collect (cond ((listp item)
                                    ;; assign sublexicon based on symbol macros for invocation
                                    (when (and (not symacro-lex) (symbolp (first item))
                                               (eql 'symbol-macrolet (first item)))
                                      (setf symacro-lex (second item)
                                            symacro-syms (loop :for i :below (length (second item))
                                                            :collect (gensym))))
                                    (when (and (symbolp (first item))
                                               (eql 'sub-lex (first item)))
                                      ;; replace sub-lex forms with sub-lexicon instance
                                      (setf item `(let ,(loop :for l :in symacro-lex
                                                           :for s :in symacro-syms
                                                              :collect (list s (first l)))
                                                    (declare (ignorable ,@symacro-syms))
                                                    (symbol-macrolet
                                                        ,(loop :for l :in symacro-lex
                                                            :for s :in symacro-syms
                                                            :collect (list (first l) s))
                                                      (sub-aliasing ,(second item))))))
                                    (if (and (second item) (not (third item))
                                             (symbolp (second item)) (position (first item) #(inws inwsd)
                                                                               :test #'eql))
                                        (let ((istring (string (second item))))
                                          (intern (string (second item))
                                                  (if (and inside-function
                                                           (not (eql 'inwsd (first item)))
                                                           (not (char= #\* (aref istring 0)))
                                                           (loop :for str :in top-level-instrs
                                                              :never (string= str istring)))
                                                      lex-space-name space-name)))
                                        ;; don't lex-intern functions like #'⊏|fn|
                                        (replace-symbols item (and (not (eql 'function (first item)))
                                                                   (or inside-function
                                                                       (position (first item)
                                                                                 #(alambda olambda)
                                                                                 :test #'eql))))))
                                      ((and (symbolp item) (string= "+WORKSPACE-NAME+" (string-upcase item)))
                                       (list 'quote (find-symbol (string-upcase name) this-package)))
                                      (t item)))))
        (replace-symbols (first body))))))

(defmacro apl-fn (glyph &rest initial-args)
  "Wrap a glyph referencing a lexical function, and if more parameters are passed, use them as a list of implicit args for the primary function represented by that glyph, the resulting secondary function to be called on the argumants passed in the APL code."
  (let ((symbol (if (fboundp (find-symbol (format nil "BLINK-LEX-VFN-~a" glyph)
                                          *package-name-string*))
                    (find-symbol (format nil "BLINK-LEX-VFN-~a" glyph) *package-name-string*)
                    (find-symbol (format nil "BLINK-LEX-FN-~a" glyph)  *package-name-string*))))
    (if initial-args (cons symbol initial-args)
        (list 'function symbol))))

(defmacro apl-fn-s (glyph &rest initial-args)
  "Wrap a glyph referencing a scalar lexical function, with axes handled appropriately and defaulting to the (apl-fn) handling of ."
  (let ((args (gensym)) (axes-sym (gensym))
        (axes (when (listp (first initial-args))
                (first initial-args))))
    (if axes `(let ((,axes-sym ,@(when axes (list axes))))
                (lambda (&rest ,args)
                  (if (eq :get-metadata (first ,args))
                      ,(append '(list :scalar t))
                      (a-call (apl-fn-s ,glyph)
                              (first ,args) (second ,args)
                              ,axes-sym))))
        (cons 'apl-fn (cons glyph initial-args)))))

(defun is-atom (item)
  (let ((shape (shape-of item)))
    (if shape 0 1)))

(defun operate-each (operand)
  (op-compose 'vacomp-each :left operand))

(defun operate-variant (operand)
  (lambda (item &optional alpha)
    (let ((varray (funcall operand item alpha)))
      (typecase varray
        (vacomp-each
         (let* ((pairs (ash (first (vader-shape item)) -1)))
           (make-instance 'vacomp-each
                          :left (lambda (item) (make-instance 'vader-reduce :left operand :omega item))
                          :omega (make-instance 'vader-partition
                                                :base (vader-base varray)
                                                :argument (make-instance 'vapri-apro-vector
                                                                         :number pairs :repeat 2)))))
        (vacomp-reduce
         (make-instance 'vacomp-each :left (varray::vacmp-left varray)
                                     :omega (varray::vacmp-omega varray)
                                     :alpha (make-instance 'vader-enclose :base (varray::vacmp-alpha varray))))
        (vacomp-scan
         (make-instance 'vacomp-each :left (varray::vacmp-left varray)
                                     :omega (make-instance 'vader-enclose :base (varray::vacmp-omega varray))
                                     :alpha (varray::vacmp-alpha varray)))))))
