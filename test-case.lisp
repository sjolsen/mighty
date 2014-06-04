;;;; Testing for nullability

(in-package #:mighty)



(defmethod print-object :around ((o language) str)
  (declare (ignore o str))
  (let ((*print-circle* t)
        (*print-level* 20))
    (call-next-method)))

(defmethod print-object ((o empty-language) str)
  (format str "{}"))
(defmethod print-object ((o null-language) str)
  (format str "{e}"))
(defmethod print-object ((o terminal-language) str)
  (format str "{c}"))
(defmethod print-object ((o repeat-language) str)
  (format str "{c}*"))

(defmethod print-object ((o alternate-language) str)
  (format str "~A" (list 'or (left o) (right o))))
(defmethod print-object ((o catenate-language) str)
  (format str "~A" (list 'and (left o) (right o))))



(defun make-testcase (n)
  (labels ((random-language ()
             (let ((p (random 100)))
               (make-instance
                 (cond ((< p 45) 'alternate-language)
                       ((< p 90) 'catenate-language)
                       ((< p 95)  'empty-language)
                       (t        'null-language))))))
    (let ((v (make-array n :element-type '(or language null))))
      (loop
         for i from 0 below n
         do (setf (aref v i) (random-language)))
      (loop
         for L across v
         when (delta-recursivep L)
           do (setf (left L) (aref v (random n))
                    (right L) (aref v (random n))))
      (find-if #'delta-recursivep v))))

(defun map-language (f L)
  (labels ((do-it (L visited)
             (unless (exchangef (gethash L visited) t)
               (funcall f L)
               (when (delta-recursivep L)
                 (do-it (left L) visited)
                 (do-it (right L) visited)))))
    (do-it L (make-hash-table))))

(defun lreset (L)
  (when (delta-recursivep L)
    (setf (delta-cache L) nil
          (delta-fixed L) nil
          (delta-visited L) nil)))

(defun lreset-all (L)
  (map-language #'lreset L))

(defgeneric verify (L)
  (:method ((L empty-language))
    (assert (not (nullablep L))))
  (:method ((L null-language))
    (assert (nullablep L)))
  (:method ((L empty-language))
    (assert (not (nullablep L))))
  (:method ((L repeat-language))
    (assert (nullablep L)))
  (:method ((L alternate-language))
    (assert (eql (nullablep L)
                 (or (nullablep (left L))
                     (nullablep (right L))))))
  (:method ((L catenate-language))
    (assert (eql (nullablep L)
                 (and (nullablep (left L))
                      (nullablep (right L)))))))

(defun verify-all (L)
  (map-language #'verify L))
