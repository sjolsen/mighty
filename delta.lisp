(require '#:sj-lisp)

(eval-when (:compile-toplevel)
  (import '(sj-lisp:exchangef
            sj-lisp:setf->changed
            sj-lisp:with-hygienic-names)))

(defclass language ()
  ())

(defclass empty-language (language)
  ())

(defclass null-language  (language)
  ())

(defclass terminal-language (language)
  ())

(defclass repeat-language (language)
  ())



(defclass delta-recursive ()
  ((delta-cache :type boolean
                :initform nil
                :accessor delta-cache)
   (delta-fixed :type boolean
                :initform nil
                :accessor delta-fixed)
   (delta-visited :type boolean
                  :initform nil
                  :accessor delta-visited)))

(defun delta-recursivep (L)
  (typep L 'delta-recursive))

(defclass alternate-language (language delta-recursive)
  ((left :type language
         :initarg :left
         :accessor left)
   (right :type language
          :initarg :right
          :accessor right)))

(defclass catenate-language (language delta-recursive)
  ((left :type language
         :initarg :left
         :accessor left)
   (right :type language
          :initarg :right
          :accessor right)))

(defun reset-visitedness (L)
  (when (delta-recursivep L)
    (when (sj-lisp:exchangef (delta-visited L) nil)
      (reset-visitedness (left L))
      (reset-visitedness (right L)))))



(defmacro or/or (first &rest rest)
  (if rest
      (with-hygienic-names (fvalue1 fvalue2 rvalue1 rvalue2)
        `(multiple-value-bind (fvalue1 fvalue2)
             ,first
           (if fvalue1
               (values fvalue1 fvalue2)
             (multiple-value-bind (rvalue1 rvalue2)
                 (or/or ,@rest)
               (values (or fvalue1 rvalue1)
                       (or fvalue2 rvalue2))))))
    first))

(defmacro and/or (first &rest rest)
  (if rest
      (with-hygienic-names (fvalue1 fvalue2 rvalue1 rvalue2)
        `(multiple-value-bind (fvalue1 fvalue2)
             ,first
           (if (not fvalue1)
               (values fvalue1 fvalue2)
             (multiple-value-bind (rvalue1 rvalue2)
                 (and/or ,@rest)
               (values (and fvalue1 rvalue1)
                       (or fvalue2 rvalue2))))))
    first))

(defun nullablep (L)
  (labels ((delta (L)
             (declare (ftype (function (language) (values boolean boolean)) delta/changed))
             (etypecase L
               (empty-language     nil)
               (null-language      t)
               (terminal-language  nil)
               (repeat-language    t)
               (alternate-language (or/or (delta/changed (left L))
                                          (delta/changed (right L))))
               (catenate-language  (and/or (delta/changed (left L))
                                           (delta/changed (right L))))))
           (delta/changed (L)
             (cond ((not (delta-recursivep L))
                    (delta L))
                   ((delta-fixed L)
                    (delta-cache L))
                   ((sj-lisp:exchangef (delta-visited L) t)
                    (delta-cache L))
                   (t
                    (multiple-value-bind (new-delta changed1) (delta L)
                      (let ((changed2 (sj-lisp:setf->changed (delta-cache L) new-delta)))
                        (when (eql new-delta t) ;; Optimization - at the top of the lattice
                          (setf (delta-fixed L) t))
                        (values new-delta (or changed1 changed2))))))))
    (multiple-value-bind (result changed)
        (delta/changed L)
      (reset-visitedness L)
      (if changed
          (nullablep L)
        (progn
          (when (delta-recursivep L)
            (setf (delta-fixed L) t))
          result)))))
