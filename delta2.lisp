(in-package #:mighty)



(defclass language () ())

(eval-when (:compile-toplevel :load-toplevel)
  (defclass empty-language (language) ())
  (defclass null-language (language) ()))

(unless (boundp '+empty+)
  (defconstant +empty+ (make-instance 'empty-language)))
(unless (boundp '+null+)
  (defconstant +null+ (make-instance 'null-language)))

(defclass compound ()
  ((derive-cache :type list
                 :initform nil
                 :accessor derive-cache)
   (simple-lang :type language
                :initform nil
                :accessor simple-lang)
   (simple-fixed :type boolean
                 :initform nil
                 :accessor simple-fixed)
   (visited :type boolean
            :initform nil
            :accessor visited)))

(defmethod initialize-instance :after ((L compound) &rest initargs)
  (setf (simple-lang L) L))

(defmethod simple-lang ((L language))
  L)

(defmethod simple-fixed ((L language))
  t)

(defclass terminal-language (language)
  ((terminal :type (or language symbol)
             :initarg :terminal
             :accessor terminal)))

(defclass repeat-language (compound language)
  ((repeated-language :type (or language symbol)
                      :initarg :repeated-language
                      :accessor repeated-language)))

(defclass delta-recursive ()
  ((left :type (or language symbol)
         :initarg :left
         :accessor left)
   (right :type language
          :initarg :right
          :accessor right)
   (delta-cache :type boolean
                :initform nil
                :accessor delta-cache)
   (delta-fixed :type boolean
                :initform nil
                :accessor delta-fixed)
   (empty-cache :type boolean
                :initform t
                :accessor empty-cache)
   (empty-fixed :type boolean
                :initform nil
                :accessor empty-fixed)))

(defclass alternate-language (delta-recursive compound language) ())
(defclass catenate-language (delta-recursive compound language) ())

(defun delta-recursive-p (L)
  (typep L 'delta-recursive))

(defmethod delta-fixed ((L language))
  t)

(defgeneric reset-visited (L)
  (:method ((L language)))
  (:method ((L delta-recursive))
    (when (visited L)
      (setf (visited L) nil)
      (reset-visited (left L))
      (reset-visited (right L)))))



(defun full-or (&rest args)
  (when args
    (or (first args)
        (apply #'full-or (rest args)))))

(defgeneric delta-base (L)
  (:method ((L empty-language))    nil)
  (:method ((L null-language))     t)
  (:method ((L terminal-language)) nil)
  (:method ((L repeat-language))   t)
  (:method ((L delta-recursive))   (delta-cache L)))

(defgeneric combine (L)
  (:method ((L alternate-language))
    (or (delta-base (left L))
        (delta-base (right L))))
  (:method ((L catenate-language))
    (and (delta-base (left L))
         (delta-base (right L)))))

(defgeneric run->changed (L)
  (:method ((L language)) nil)
  (:method ((L delta-recursive))
    (unless (delta-fixed L)
      (when (setf->changed (visited L) t)
        (full-or (run->changed (left L))
                 (run->changed (right L))
                 (setf->changed (delta-cache L)
                                (setf (delta-fixed L)
                                      (combine L))))))))

(defgeneric nullablep (L)
  (:method ((L language))
    (delta-base L))
  (:method ((L delta-recursive))
    (loop
       do (reset-visited L)
       while (run->changed L)
       finally
         (setf (delta-fixed L) t)
         (return (delta-base L)))))



(defgeneric empty-base (L)
  (:method ((L empty-language))    t)
  (:method ((L null-language))     nil)
  (:method ((L terminal-language)) nil)
  (:method ((L repeat-language))   nil)
  (:method ((L delta-recursive))   (empty-cache L)))

(defgeneric empty-combine (L)
  (:method ((L alternate-language))
    (and (empty-base (left L))
         (empty-base (right L))))
  (:method ((L catenate-language))
    (or (empty-base (left L))
        (empty-base (right L)))))

(defgeneric empty->changed (L)
  (:method ((L language)) nil)
  (:method ((L delta-recursive))
    (unless (empty-fixed L)
      (when (setf->changed (visited L) t)
        (full-or (empty->changed (left L))
                 (empty->changed (right L))
                 (setf->changed (empty-cache L)
                                (not (setf (empty-fixed L)
                                           (not (empty-combine L))))))))))

(defgeneric emptyp (L)
  (:method ((L language))
    (empty-base L))
  (:method ((L delta-recursive))
    (loop
       do (reset-visited L)
       while (empty->changed L)
       finally
         (setf (empty-fixed L) t)
         (return (empty-base L)))))



(defun and/or/epsilon->repeat (L)
  (labels ((match-or (O L)
             (and (typep O 'alternate-language)
                  (or (and (eq (left O) L)
                           (typep (right O) 'null-language))
                      (and (eq (right O) L)
                           (typep (left O) 'null-language))))))
    (cond
      ((match-or (left L) L)  (make-instance 'repeat-language :repeated-language (right L)))
      ((match-or (right L) L) (make-instance 'repeat-language :repeated-language (left L)))
      (t nil))))

(defgeneric simplify-base (L)
  (:method ((L language)) L)
  (:method ((L repeat-language))
    (typecase (repeated-language L)
      (empty-language  +null+) ;; {}*  => {e}
      (null-language   +null+) ;; {e}* => {e}
      (repeat-language (repeated-language L)) ;; L . L** => L*
      (t               L)))
  (:method ((L alternate-language))
    (cond
      ((emptyp (left L))  (right L)) ;; L . {}|L => L
      ((emptyp (right L)) (left L))  ;; L . L|{} => L
      ((and (typep (left L) 'null-language)
            (typep (right L) 'repeat-language)) (right L))
      ((and (typep (right L) 'null-language)
            (typep (left L) 'repeat-language)) (left L))
      (t                  L)))
  (:method ((L catenate-language))
    (cond
      ((typep (left L) 'null-language)  (right L)) ;; L . {e}L => L
      ((typep (right L) 'null-language) (left L))  ;; L . {e}L => L
      ((emptyp (left L))                +empty+)   ;; L . {}L  => {}
      ((emptyp (right L))               +empty+)   ;; L . L{}  => {}
      ((and (typep (left L) 'repeat-language)
            (typep (right L) 'repeat-language)
            (eq (left L) (right L))) (left L))
      (t (let ((rep (and/or/epsilon->repeat L)))
           (if rep rep L))))))

(defgeneric simplify->changed (L)
  (:method ((L language))
    nil)
  (:method :around ((L compound))
    (unless (simple-fixed L)
      (when (setf->changed (visited L) t)
        (call-next-method L))))
  (:method ((L repeat-language))
    (full-or (simplify->changed (repeated-language L))
             (setf->changed (repeated-language L)
                            (simple-lang (repeated-language L)))
             (setf->changed (simple-lang L)
                            (simplify-base L))))
  (:method ((L delta-recursive))
    (full-or (simplify->changed (left L))
             (simplify->changed (right L))
             (setf->changed (left L)
                            (simple-lang (left L)))
             (setf->changed (right L)
                            (simple-lang (right L)))
             (setf->changed (simple-lang L)
                            (simplify-base L)))))

(defgeneric simplify-finalize (L)
  (:method ((L language)))
  (:method :around ((L compound))
    (when (setf->changed (visited L) t)
      (setf (simple-fixed L) t)
      (call-next-method L)))
  (:method ((L repeat-language))
    (simplify-finalize (repeated-language L)))
  (:method ((L delta-recursive))
    (simplify-finalize (left L))
    (simplify-finalize (right L))))

(defgeneric simplify (L)
  (:method ((L language))
    (simplify-base L))
  (:method ((L compound))
    (loop
       do (setf L (simple-lang L))
       do (reset-visited L)
       while (simplify->changed L)
       finally
         (reset-visited L)
         (simplify-finalize L)
         (return L))))



(defgeneric derive-base (L c))

(defgeneric make-derivative (L)
  (:method ((L repeat-language))
    (make-instance 'catenate-language))
  (:method ((L alternate-language))
    (make-instance 'alternate-language))
  (:method ((L catenate-language))
    (if (nullablep (left L))
        (make-instance 'alternate-language)
        (make-instance 'catenate-language))))

(defgeneric finish-derivative (L c D)
  (:method ((L repeat-language) c D)
    (setf (left D) (derive-base (repeated-language L) c)
          (right D) L))
  (:method ((L alternate-language) c D)
    (setf (left D) (derive-base (left L) c)
          (right D) (derive-base (right L) c)))
  (:method ((L catenate-language) c (D alternate-language))
    (setf (left D) (derive-base (right L) c)
          (right D) (make-instance 'catenate-language
                                   :left (derive-base (left L) c)
                                   :right (right L))))
  (:method ((L catenate-language) c (D catenate-language))
    (setf (left D) (derive-base (left L) c)
          (right D) (right L))))

(defmethod derive-base ((L empty-language) c)
  +empty+)
(defmethod derive-base ((L null-language) c)
  +empty+)
(defmethod derive-base ((L terminal-language) c)
  (if (equal c (terminal L))
      +null+
      +empty+))
(defmethod derive-base ((L compound) c)
  (let ((cell (assoc c (derive-cache L))))
    (if cell
        (cdr cell)
        (let ((D (make-derivative L)))
          (push (cons c D) (derive-cache L))
          (finish-derivative L c D)
          D))))

(defun derive (L c)
  (simplify (derive-base L c)))



(defun lang-rep (arg)
  (make-instance 'repeat-language
                 :repeated-language arg))

(defun lang-and (args)
  (if args
      (make-instance 'catenate-language
                     :left (first args)
                     :right (lang-and (rest args)))
      +null+))

(defun lang-or (args)
  (if args
      (make-instance 'alternate-language
                     :left (first args)
                     :right (lang-or (rest args)))
      +empty+))

(defun lang-char (c)
  (make-instance 'terminal-language :terminal c))

(defun lang-string (s)
  (loop
     for c across s
     collecting (lang-char c) into langs
     finally (return (lang-and langs))))

(defun parse-rule (rule-body)
  (etypecase rule-body
    (string    (lang-string rule-body))
    (character (lang-char rule-body))
    (symbol    rule-body)
    (language  rule-body)
    (list (ecase (car rule-body)
            (rep (lang-rep (parse-rule (cadr rule-body))))
            (and (lang-and (mapcar #'parse-rule (cdr rule-body))))
            (or  (lang-or  (mapcar #'parse-rule (cdr rule-body))))))))

(defun resolve-rule (L rule-table)
  (etypecase L
    (delta-recursive
     (setf (left L)
           (resolve-rule (left L) rule-table))
     (setf (right L)
           (resolve-rule (right L) rule-table))
     L)
    (repeat-language
     (setf (repeated-language L)
           (resolve-rule (repeated-language L) rule-table))
     L)
    (symbol
     (multiple-value-bind (rule found)
         (gethash L rule-table)
       (assert found)
       rule))
    (language
     L)))

(defun uniquify-rules (rules)
  (labels ((merge-rules (rules acc)
             (cond
               ((null rules)
                acc)
               ((eq (caar rules) (caar acc))
                (setf (cdar acc)
                      `(or ,(cdar acc) ,(cdar rules)))
                (merge-rules (cdr rules) acc))
               (t
                (let ((rule (pop rules)))
                  (merge-rules rules (push rule acc)))))))
    (merge-rules (sort rules #'< :key (lambda (rule) (sxhash (car rule))))
                 nil)))

(defun make-grammar (S rules)
  (let ((rule-table (make-hash-table)))
    (loop
       with unique-rules = (uniquify-rules rules)
       for rule in unique-rules
       do (setf (gethash (car rule) rule-table)
                (parse-rule (cdr rule))))
    (loop
       for name being each hash-key in rule-table using (hash-value rule-body)
       do (setf (gethash name rule-table) (resolve-rule rule-body rule-table)))
    (simplify (gethash S rule-table))))

(defun match (input L)
  (if (zerop (length input))
      (nullablep L)
      (match (subseq input 1) (derive L (elt input 0)))))



(defun lang-size (L &optional (visited (make-hash-table)))
  (if (gethash L visited)
      0
      (progn
        (setf (gethash L visited) t)
        (etypecase L
          (repeat-language (+ 1 (lang-size (repeated-language L) visited)))
          (delta-recursive (+ 1 (lang-size (left L) visited)
                                (lang-size (right L) visited)))
          (language 1)))))

(defun make-graph (L filename)
  (labels ((label-node (L)
             (etypecase L
               (empty-language     "{}")
               (null-language      "{epsilon}")
               (terminal-language  (format nil "{~A}" (terminal L)))
               (repeat-language    "*")
               (alternate-language "or")
               (catenate-language  "and")))
           (name-node (L)
             (sxhash L))
           (collect-nodes (L visited)
             (if (gethash L visited)
                 nil
                 (progn
                   (setf (gethash L visited) t)
                   (etypecase L
                     (repeat-language (list* L (collect-nodes (repeated-language L) visited)))
                     (delta-recursive (list* L (nconc (collect-nodes (left L) visited)
                                                      (collect-nodes (right L) visited))))
                     (language (list L))))))
           (print-node (stream L)
             (let ((name  (name-node L))
                   (label (label-node L)))
               (format stream "  ~A [label=~W];~%" name label)
               (typecase L
                 (repeat-language (format stream "  ~A -> ~A;~%" name (name-node (repeated-language L))))
                 (delta-recursive (format stream "  ~A -> ~A [tailport=\"sw\"];~%" name (name-node (left L)))
                                  (format stream "  ~A -> ~A [tailport=\"se\"];~%" name (name-node (right L))))
                 (t)))))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (format stream "digraph G {~%")
      (format stream "  ~A [peripheries=2];~%" (name-node L))
      (mapcar (lambda (L) (print-node stream L)) (collect-nodes L (make-hash-table)))
      (format stream "}~%"))))
