(in-package #:mighty)



(defclass language () ())
(defclass empty-language (language) ())
(defclass null-language (language) ())

(defclass terminal-language (language)
  ((terminal :type language
             :initarg :terminal
             :accessor terminal)))

(defclass repeat-language (language)
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
                :accessor delta-fixed)
   (delta-visited :type boolean
                  :initform nil
                  :accessor delta-visited)))

(defclass alternate-language (delta-recursive language) ())
(defclass catenate-language (delta-recursive language) ())

(defun delta-recursive-p (L)
  (typep L 'delta-recursive))

(defgeneric reset-visited (L)
  (:method ((L language)))
  (:method ((L delta-recursive))
    (when (delta-visited L)
      (setf (delta-visited L) nil)
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
      (when (setf->changed (delta-visited L) t)
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
