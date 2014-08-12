(defpackage #:mighty
  (:use #:cl)
  (:export ;; delta2
           #:empty-language
           #:null-language
           #:terminal-language
           #:repeat-language
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

(asdf:defsystem #:mighty
  :defsystem-depends-on (#:sj-lisp)
  :components ((:file "delta2")
               (:file "test-case"
                      :depends-on ("delta2"))))

(in-package :mighty)
(import '(sj-lisp:exchangef
          sj-lisp:setf->changed))
