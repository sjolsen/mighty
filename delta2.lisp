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
   (visited :type boolean
            :initform nil
            :accessor visited)))

(defclass terminal-language (language)
  ((terminal :type language
             :initarg :terminal
             :accessor terminal)))

(defclass repeat-language (compound language)
  ((repeated-language :type language
                      :initarg :repeated-language
                      :accessor repeated-language)))

(defclass delta-recursive ()
  ((left :type language
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
                :accessor delta-fixed)))

(defclass alternate-language (delta-recursive compound language) ())
(defclass catenate-language (delta-recursive compound language) ())

(defun delta-recursive-p (L)
  (typep L 'delta-recursive))

(defgeneric reset-visited (L)
  (:method ((L language)))
  (:method ((L delta-recursive))
    (when (visited L)
      (setf (visited L) nil)
      (reset-visited (left L))
      (reset-visited (right L)))))



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
        (or (run->changed (left L))
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
