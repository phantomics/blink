;;;; package.lisp

(defpackage #:blink
  (:use #:cl #:april #:aplesque #:vex #:varray)
  (:shadowing-import-from #:april #:foldin #:process-fnspecs #:parse-apl-number-string
                          ;; #:format-value
                          #:determine-symbolic-form
                          #:compile-form #:provision-function-builder
                          #:build-variable-declarations
                          #:provision-code-builder #:is-alphanumeric
                          #:inws #:inwsd #:reverse-op #:find-char
                          #:λω #:λωα #:compare-by
                          #:a-out #:scalar-function #:liminally-controlled #:apl-if
                          #:omega #:alpha
                          #:binary-not #:scalar-compare #:apl-expt #:apl-floor #:count-to
                          #:at-index #:sub-aliasing #:ac-wrap
                          #:operate-each #:operate-beside #:operate-grouping)
  (:shadowing-import-from #:varray #:vacmp-left #:vacmp-right #:vacmp-alpha #:vacmp-omega)
  (:shadowing-import-from #:parse-number #:parse-number)
  (:shadowing-import-from #:symbol-munger #:lisp->camel-case)
  (:shadowing-import-from #:simple-date-time #:now #:year-of #:month-of #:day-of #:hour-of
                          #:minute-of #:second-of #:millisecond-of)
  (:shadowing-import-from #:trivia #:match #:guard)
  (:shadowing-import-from #:random-state #:make-generator #:random-int #:random-float))
