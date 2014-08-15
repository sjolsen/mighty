(defun lang-and (&rest args)
  (if args
      (make-instance 'catenate-language
                     :left (first args)
                     :right (apply #'lang-and (rest args)))
      +null+))

(defun lang-or (&rest args)
  (if args
      (make-instance 'alternate-language
                     :left (first args)
                     :right (apply #'lang-or (rest args)))
      +empty+))

(defun lang-string (s)
  (loop
     for c across s
     collecting (make-instance 'terminal-language :terminal c) into langs
     finally (return (apply #'lang-and langs))))
