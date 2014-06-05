;;;; Basic stuff for nullablility

(in-package #:mighty)



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
    (when (exchangef (delta-visited L) nil)
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

;;; This works basically the same way as the code in derp-latest, with the
;;; following important differences:
;;;
;;; - Associative metadata are stored intrusively.
;;;
;;;     This is a big win on speed, and maybe not so much on space (though the
;;;     upper bound on space is almost certainly much better).
;;;
;;; - The cache is handled slightly differently (apart from the above).
;;;
;;;     The cache logic in derp is actually slightly more complex than it has to
;;;     be; eliminating the CACHED? predicate and simply initializing the cache
;;;     to the bottom of the lattice (in this case, false) is equivalent.
;;;
;;;     There's another important optimization here that depends on the
;;;     monotonicity that makes the algorithm work in the first place: if the
;;;     cached value is the top value of the lattice (true), then it is
;;;     _necessarily_ the correct value. This is probably not terribly useful in
;;;     large lattices, but here the lattice consists entirely of false and
;;;     true, so it's important. The effect of the optimization is to let us
;;;     rely implicitly on the guesses for many intermediate values without
;;;     running NULLABLEP on them from the toplevel, as would be necessary
;;;     otherwise.
;;;
;;; - The logic otherwise handled by RUNNING? is cleanly separated.
;;;
;;;     In the sake of generality, the original DEFINE/FIXED is designed to make
;;;     the toplevel function a valid entry point for the internal recursive
;;;     algorithm. Apart from hindering delicious local function optimization,
;;;     this means that every iteration along the language chain is branching on
;;;     a dynamically scoped value. Ick.
;;;
;;; - Changedness is tracked through local, lexical variables.
;;;
;;;     Basically the same idea as above. Make life as easy for the compiler as
;;;     possible. Dynamic variables, though certainly an effective solution, are
;;;     not the easiest things to statically analyze.
;;;
;;; - If one sublanguage of a catenate/alternate language is easily computable,
;;;   do it first.
;;;
;;;     Evaluating both sublanguages' nullability isn't actually necessary in
;;;     many cases to decide nullability for the toplevel language (i.e., the
;;;     argument to NULLABLEP). There's a limit to how deep it's useful to look
;;;     ahead (and a small one, too, thanks to exponential growth), but we can
;;;     avoid a fair amount of computation just by computing the delta of (RIGHT
;;;     L) first if it's non–delta-recursive.
;;;
;;; The sum effect of these optimizations is that NULLABLEP performs _zero_
;;; dynamic memory allocations. In effect, the memory that would otherwise be
;;; dynamically allocated is preallocated in nice, high-locality slots. I'm not
;;; actually sure exactly how SBCL/PCL represents class instances, but I suspect
;;; the difference is an additional three immediate 64-bit pointers for each
;;; delta-recursive language, as opposed to who knows how much on average for an
;;; entry in a full-blown hash-table.
;;;
;;; Some other thoughts:
;;;
;;; - Short-circuiting the value test in AND/OR and OR/OR helps
;;;   performance. Short-circuiting the changedness test kills it.
;;;
;;; - While this implementation does no significant additional heap allocation,
;;;   it munches stack like there's no tomorrow.
;;;
;;;     Not all is lost, though; note that at any given language L, there is
;;;     only one dormant activation record for (DELTA L) at any given time. This
;;;     suggests to me that the way to ensure constant stack usage (at the cost
;;;     of still more overhead for interesting language objects) is to store the
;;;     relevant information _in the language object_ and do control flow by
;;;     hand. How to do that, though, is unclear. It might be possible to get
;;;     away with just a parent pointer.
;;;
;;; - This just does language nullability. I suspect the implementation will map
;;;   more or less directly to parser nullability, but I wouldn't bet money on
;;;   it just yet.
;;;
;;; - These methods may actually be conducive to implementation in, e.g., C++.
;;;
;;;     There's dynamic typing here, and fairly extensive type predicating, so
;;;     plain old "virtual" dynamic typing ain't gonna cut it. Still, though,
;;;     the hierarchy is effectively closed, there's no funky memory allocation
;;;     happening beyond the very cyclic-graph nature of the languages, and C++
;;;     gives you _much_ more control over memory layout than Common Lisp, so I
;;;     bet it's doable.
;;;
;;;     With respect to the stackless optimization mentioned above… maybe.
;;;     Hopefully it can be done without poorly reinventing half of Common Lisp.
;;;
(defun nullablep (L)
  (labels ((delta-would-recurse (L)
             (and (delta-recursivep L)
                  (not (delta-visited L))
                  (not (delta-fixed L))))
           (delta (L)
             (declare (ftype (function (language) (values boolean boolean)) delta/changed))
             (macrolet ((combine-L (combine)
                          (let ((delta-left  `(delta/changed (left L)))
                                (delta-right `(delta/changed (right L))))
                            `(if (delta-would-recurse (right L)) ;; Optimization - only recurse if necessary
                                 (,combine ,delta-left  ,delta-right)
                                 (,combine ,delta-right ,delta-left)))))
               (etypecase L
                 (empty-language     nil)
                 (null-language      t)
                 (terminal-language  nil)
                 (repeat-language    t)
                 (alternate-language (combine-L or/or))
                 (catenate-language  (combine-L and/or)))))
           (delta/changed (L)
             (cond ((not (delta-recursivep L))
                    (delta L))
                   ((delta-fixed L)
                    (delta-cache L))
                   ((exchangef (delta-visited L) t)
                    (delta-cache L))
                   (t
                    (multiple-value-bind (new-delta changed1) (delta L)
                      (let ((changed2 (setf->changed (delta-cache L) new-delta)))
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
