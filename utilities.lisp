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
  (lambda (omega &optional alpha)
    (let ((varray (funcall operand omega alpha))
          (count (ash (first (shape-of omega)) -1)))
      (flet ((left (oo aa) (declare (ignore aa)) oo))
        (typecase varray
          (vacomp-each
           ;; (let ((arvec (make-instance 'vapri-apro-vector :repeat 2 :origin 1 :number count)))
           ;; (make-instance 'vacomp-each
           ;;                :left  (lambda (item) (make-instance 'vacomp-reduce :left operand :omega item))
           ;;                :omega (make-instance 'vader-partition :argument arvec :axis :last :base omega))
           (make-instance 'vacomp-each
                          :left  (lambda (item) (make-instance 'vacomp-reduce :left operand :omega item))
                          :omega (make-instance 'vacomp-stencil :right 2 :omega omega :left #'left)))
          (vacomp-reduce
           (make-instance 'vacomp-each :left  (vacmp-left varray) :omega (vacmp-omega varray)
                                       :alpha (make-instance 'vader-enclose :base (vacmp-alpha varray))))
          (vacomp-scan
           (make-instance 'vacomp-each :left  (vacmp-left varray) :alpha (vacmp-alpha varray)
                                       :omega (make-instance 'vader-enclose :base (vacmp-omega varray)))))))))

(defun process-glyph-token (string index end scratch tokens idiom)
  (declare (ignore scratch end))
  ;;; (print (list :x index string (< index (length string))))
  (let* ((operator-mod) (char (aref string index))
         (prior-space (and (not (zerop index))
                           (member (aref string (1- index)) '(#\  #\Tab) :test #'char=)))
         (prefix (cond ((and (of-lexicons idiom char :operators)
                             (not (or prior-space (zerop index))) ;; adverbs may not have a preceding space
                             (or (symbolp (first tokens)) ;; the prior token is a symbol or token
                                 (and (listp (first tokens)) ;; symbols are checked for function identity later
                                      (member (caar tokens) '(:fn :op)))))
                        (when (char= #\: (aref string (1+ index)))
                          ;; set the operator modification in case it's followed by :
                          (setf operator-mod '(:op :lateral #\⍠))
                          (incf index)) ;; increment the index to skip past the following :
                        :op)
                       ((of-lexicons idiom char :functions)
                        :fn)
                       ((of-lexicons idiom char :statements) :st)))
         (tag (if (eq :op prefix)
                  (cond ((of-lexicons idiom char :operators-lateral) :lateral)
                        ((of-lexicons idiom char :operators-pivotal) :pivotal))
                  (and (eq :st prefix) :unitary)))
         (out (or (and (not (eq :op prefix))
                       (determine-symbolic-form idiom char))
                  (and prefix (cons prefix (if tag (list tag char)
                                               (list char)))))))
    (when out (push out tokens))
    (when operator-mod (push operator-mod tokens))

    (values tokens (1+ index))))

(let ((id-vars) (id-cons))
  (flet ((match-varisym-char (char &optional first)
           ;; match regular symbols used for assigned variable/function names
           (or (is-alphanumeric char) (char= char #\_) ;; ¯
               (and (not first) (char= #\. char)))))
    
    (defun process-symbol-token (string index end scratch tokens idiom)
      "Process characters that may make up part of a symbol token. This is complex enough it is implementd here instead of directly inside the April idiom spec."
      (let ((symout) (path-start) (pre-symbol))
        (unless id-vars (setf id-vars (rest (assoc :variable (idiom-symbols idiom)))))
        (unless id-cons (setf id-cons (rest (assoc :constant (idiom-symbols idiom)))))

        ;; match regular symbols used for variable/function names; may continue a path
        (unless (or symout (not (match-varisym-char (aref string index) (not path-start))))
          (when (char= #\⎕ (aref string index)) (setf pre-symbol t)) ;; identify quad-prefixed names

          ;; if a path is already started, as for ⍵.a.b, the symbol may begin with .,
          ;; otherwise the symbol is not valid; paths also may not start with a number
          (when (and (or path-start (not (char= #\. (aref string index))))
                     (not (digit-char-p (aref string index))))

            (vector-push (aref string index) scratch)
            (incf index)

            (loop :while (and (< index end) (match-varisym-char (aref string index)))
                  :do (vector-push (aref string index) scratch)
                      (incf index))

            ;; in the case of a ⎕ quad-prefixed name, fetch the referenced symbol
            (when pre-symbol (setf pre-symbol (find-symbol (string-upcase scratch) "APRIL")))
            (setf symout (or (and pre-symbol (or (getf id-vars pre-symbol)
                                                 (getf id-cons pre-symbol)))
                             (intern scratch)))))
        
        (and symout (values (cons symout tokens) index))))))
