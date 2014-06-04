(asdf:defsystem #:mighty
  :defsystem-depends-on (#:sj-lisp)
  :components ((:file "delta")
               (:file "test-case"
                      :depends-on ("delta"))))

(defpackage #:mighty
  (:use #:cl)
  (:import-from #:sj-lisp
                #:exchangef
                #:setf->changed
                #:with-hygienic-names)
  (:export ;; delta
           #:language
           #:empty-language
           #:null-language
           #:terminal-language
           #:repeat-language
           #:delta-recursive
           #:delta-recursivep
           #:alternate-language
           #:catenate-language
           #:nullablep
           ;; test-case
           #:make-testcase
           #:map-language
           #:lreset
           #:lreset-all
           #:verify
           #:verify-all))
