;;;; Basic stuff for nullablility

(in-package #:mighty)



(eval-when (:compile-toplevel :load-toplevel)
  (defstruct empty-language)
  (defstruct null-language))

(unless (boundp '+empty+)
  (defconstant +empty+ (make-empty-language)))

(unless (boundp '+null+)
  (defconstant +null+ (make-null-language)))

(defstruct (terminal-language
             (:conc-name nil))
  (terminal nil :read-only t))

(defstruct (repeat-language
             (:conc-name nil))
  (repeated-language +empty+ :read-only t))

(defstruct (delta-recursive
             (:conc-name nil))
  (left +empty+)
  (right +empty+)
  (delta-cache nil :type boolean)
  (delta-fixed nil :type boolean)
  (delta-visited nil)
  (delta-continuation nil :type symbol))

(defstruct (alternate-language
             (:include delta-recursive)))

(defstruct (catenate-language
             (:include delta-recursive)))



(defun reset-visitedness (L)
  (labels ((visitedp (L)
             (and (delta-recursive-p L)
                  (delta-visited L)))
           (visited-byp (L parent)
             (and (visitedp L)
                  (eq (delta-visited L) parent)))
           (bottom (L)
             (when (visitedp L)
               (cond ((visited-byp (left L) L)
                      (bottom (left L)))
                     ((visited-byp (right L) L)
                      (bottom (right L)))
                     (t
                      L))))
           (clear (L)
             (when (visitedp L)
               (cond ((visited-byp (right L) L)
                      (clear (bottom (right L))))
                     (t
                      (clear (exchangef (delta-visited L) nil)))))))
    (clear (bottom L))))



;;; This works basically the same way as the code in derp-latest, with the
;;; following important differences:
;;;
;;; - Associative metadata are stored intrusively.
;;;
;;;     This is a big win on speed, and maybe not so much on space (though the
;;;     upper bound on space is almost certainly much better).
;;;
;;; - This implementation uses bounded stack and heap space.
;;;
;;;     The bounded heap is the result of the last point. The particulars of the
;;;     algorithm let us use those same metadata to eliminate stack growth for
;;;     free (and by "free," I mean at the cost of having to manually transform
;;;     the already-complex code into a bastardized sort of CPS).
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
;;; the difference is an additional four immediate 64-bit pointers for each
;;; delta-recursive language, as opposed to who knows how much on average for an
;;; entry in a full-blown hash-table.
;;;
;;; Some other thoughts:
;;;
;;; - Short-circuiting the value test in AND/OR and OR/OR helps
;;;   performance. Short-circuiting the changedness test kills it.
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
(defun nullablep (L)
  (labels ((delta-base (L)
             (etypecase L
               (empty-language     nil)
               (null-language      t)
               (terminal-language  nil)
               (repeat-language    t)
               (alternate-language (delta-cache L))
               (catenate-language  (delta-cache L))))
           (run->changed (L caller changed)
             (labels ((delta-would-recurse (L)
                        (and (delta-recursive-p L)
                             (not (delta-visited L))
                             (not (delta-fixed L)))))
               (cond ((not (delta-would-recurse L))
                      (do-continuation caller changed))
                     ((delta-would-recurse (right L))
                      (setf (delta-continuation L) :other-right)
                      (setf (delta-visited L) caller)
                      (run->changed (left L) L changed))
                     (t
                      (setf (delta-continuation L) :other-left)
                      (setf (delta-visited L) caller)
                      (run->changed (right L) L changed)))))
           (do-continuation (L changed)
             (labels ((should-short-circuit (L child)
                        (etypecase L
                          (alternate-language (delta-base child))
                          (catenate-language (not (delta-base child)))))
                      (combine-child-deltas (L)
                        (etypecase L
                          (alternate-language (or (delta-base (left L))
                                                  (delta-base (right L))))
                          (catenate-language (and (delta-base (left L))
                                                  (delta-base (right L)))))))
               (if (eq L :root)
                   changed
                 (ecase (delta-continuation L)
                   (:other-right
                    (setf (delta-continuation L) :combine)
                    (if (should-short-circuit L (left L))
                        (do-continuation L changed)
                      (run->changed (right L) L changed)))
                   (:other-left
                    (setf (delta-continuation L) :combine)
                    (if (should-short-circuit L (right L))
                        (do-continuation L changed)
                      (run->changed (left L) L changed)))
                   (:combine
                    (let* ((new-delta (combine-child-deltas L))
                           (changed-here (setf->changed (delta-cache L) new-delta)))
                      (setf (delta-fixed L) new-delta) ;; Optimization - at the top of the lattice
                      (do-continuation (delta-visited L) (or changed changed-here)))))))))
    (loop
       initially (reset-visitedness L)
       while (run->changed L :root nil)
       do (reset-visitedness L)
       finally
         (when (delta-recursive-p L)
           (setf (delta-fixed L) t))
         (return-from nullablep (delta-base L)))))
