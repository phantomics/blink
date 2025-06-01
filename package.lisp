;;;; package.lisp

(defpackage #:blink
  (:use #:cl #:april #:aplesque #:vex #:varray)
  (:shadowing-import-from #:april #:foldin #:process-fnspecs #:parse-apl-number-string
                          #:format-value #:compile-form #:provision-function-builder
                          #:build-variable-declarations
                          #:provision-code-builder #:is-alphanumeric
                          #:inws #:inwsd #:reverse-op
                          #:λω #:λωα
                          #:a-out #:scalar-function #:liminally-controlled
                          #:omega #:alpha
                          #:binary-not #:scalar-compare #:apl-expt #:apl-floor #:count-to
                          #:at-index #:sub-aliasing #:ac-wrap
                          #:operate-each #:operate-grouping)
  (:shadowing-import-from #:parse-number #:parse-number)
  (:shadowing-import-from #:symbol-munger #:lisp->camel-case)
  (:shadowing-import-from #:simple-date-time #:now #:year-of #:month-of #:day-of #:hour-of
                          #:minute-of #:second-of #:millisecond-of)
  (:shadowing-import-from #:trivia #:match #:guard)
  (:shadowing-import-from #:random-state #:make-generator #:random-int #:random-float))
