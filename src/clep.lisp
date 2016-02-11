(in-package :cl-user)
(defpackage clep
  (:use :cl)
  (:export
   :search-result
   :search-result-p
   :search-result-form
   :search-result-binds
   :search-result-footprints
   :search-result-pathname
   :search-result-form-count
   :clep-files
   :clep-sexp))
(in-package :clep)

(defstruct search-result
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
      (values (coerce (nreverse binds) 'vector) t))))

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
				  (push (make-search-result
					 :pathname pathname
					 :form-count form-count
					 :form tree
					 :binds binds
					 :footprints footprints)
					acc))))
      (nreverse acc))))

(defun clep-files (pattern files)
  (when (not (listp files))
    (setf files (list files)))
  (let ((pathnames
	  (delete-duplicates (mapcar #'pathname files)
			     :test #'equal)))
    (loop :for pathname :in pathnames
	  :append (clep-file-internal pattern pathname))))

(defun clep-sexp (pattern x)
  (let ((acc))
    (match-search pattern x
                  #'(lambda (tree binds footprints)
                      (push (make-search-result :form tree
						:binds binds
						:footprints footprints)
			    acc)))
    (nreverse acc)))
