(defpackage #:mighty
  (:use #:cl)
  (:export ;; delta
           #:+empty+
           #:+null+
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
  :components ((:file "delta")
               (:file "test-case"
                      :depends-on ("delta"))))

(in-package :mighty)
(import '(sj-lisp:exchangef
          sj-lisp:setf->changed))
