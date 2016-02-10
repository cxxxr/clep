(in-package :cl-user)
(defpackage clep
  (:use :cl)
  (:export
   :clep-file
   :clep-sexp))
(in-package :clep)

(defstruct state
  form
  binds
  footprints
  pathname
  form-count)

(defun %match (pattern expr binds)
  (cond ((and (symbolp pattern)
              (symbolp expr)
              (equal (symbol-name pattern)
                     (symbol-name expr)))
         (values binds t))
        ((and (consp pattern)
              (keywordp (car pattern)))
         (ecase (car pattern)
           ((:lisp)
            (when (funcall (cadr pattern) expr)
              (values (cons expr binds) t)))
           ((:*)
            (values (cons expr binds) t))))
        ((let ((match-p))
           (and (consp pattern)
                (consp expr)
                (progn
                  (setf (values binds match-p)
                        (%match (car pattern)
                                (car expr)
                                binds))
                  match-p)))
         (%match (cdr pattern)
                 (cdr expr)
                 binds))
        (t
         (values nil nil))))

(defun match (pattern expr)
  (multiple-value-bind (binds matched-p)
      (%match pattern expr nil)
    (when matched-p
      (values (nreverse binds) t))))

(defun match-search (pattern tree matched-fn &optional (ncdr 0) footprints)
  (multiple-value-bind (binds matched-p)
      (match pattern tree)
    (when matched-p
      (funcall matched-fn tree binds (reverse footprints))))
  (when (consp tree)
    (match-search pattern
		  (car tree)
		  matched-fn
		  0
		  (cons ncdr footprints))
    (match-search pattern
		  (cdr tree)
		  matched-fn
		  (1+ ncdr)
		  footprints)))

(defun clep-file-internal (pattern pathname)
  (with-open-file (in pathname)
    (let ((acc))
      (loop :with eof-value := (gensym)
        :for form-count :from 0 :by 1
        :for x := (read in nil eof-value)
        :until (eq x eof-value)
        :do (match-search pattern x
                          #'(lambda (tree binds footprints)
                              (push (make-state :pathname pathname
						:form-count form-count
						:form tree
						:binds binds
						:footprints footprints)
                                    acc))))
      (nreverse acc))))

(defun collect-lisp-files ()
  (loop :for pathname :in (uiop/filesystem:directory-files ".")
    :when (equal "lisp" (pathname-type pathname))
    :collect pathname))

(defun arg-to-pathnames (arg)
  (etypecase arg
    (pathname (list arg))
    (string (list (pathname arg)))
    (list (args-to-pathnames arg))))

(defun args-to-pathnames (args)
  (remove-duplicates (mapcan #'arg-to-pathnames args)
		     :test #'equal))

(defun clep-file (pattern &rest args)
  (let ((pathnames
	  (if (null args)
	      (collect-lisp-files)
	      (args-to-pathnames args))))
    (loop :for pathname :in pathnames
	  :append (clep-file-internal pattern pathname))))

(defun clep-sexp (pattern x)
  (let ((acc))
    (match-search pattern x
                  #'(lambda (tree binds footprints)
                      (push (make-state :form tree
					:binds binds
					:footprints footprints)
			    acc)))
    (nreverse acc)))
