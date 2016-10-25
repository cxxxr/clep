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
   :clep-sexp
   :clep-stream
   :clep-files))
(in-package :clep)

(defstruct search-result
  form
  binds
  file-position
  footprints
  pathname
  form-count)

(defvar *operators* (make-hash-table))

(defmacro define-operator ((operator &rest operator-args) (expr) &body body)
  (check-type operator symbol)
  (check-type operator-args list)
  (check-type expr symbol)
  `(setf (gethash ',operator *operators*)
         (cons (lambda (,expr ,@operator-args)
                 ,@body)
               ,(length operator-args))))

(define-operator (:*) (expr)
  (declare (ignore expr))
  t)

(define-operator (:lisp fun) (expr)
  (funcall fun expr))

(defun %match (pattern expr binds)
  (cond ((and (symbolp pattern)
              (symbolp expr)
              (string= pattern expr))
         (values binds t))
        ((and (consp pattern)
              (let ((matcher-and-argnum (gethash (car pattern) *operators*)))
                (when matcher-and-argnum
                  (destructuring-bind (matcher . argnum) matcher-and-argnum
                    (when (and (= argnum (length (cdr pattern)))
                               (apply matcher expr (cdr pattern)))
                      (push expr binds)
                      t)))))
         (values binds t))
        ((and (consp pattern)
              (consp expr)
              (let ((match-p nil))
                (setf (values binds match-p)
                      (%match (car pattern)
                              (car expr)
                              binds))
                match-p))
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

(defun clep-sexp (pattern x)
  (let ((acc))
    (match-search pattern x
                  #'(lambda (tree binds footprints)
                      (push (make-search-result :form tree
						:binds binds
						:footprints footprints)
			    acc)))
    (nreverse acc)))

(defun clep-stream-internal (pattern stream pathname)
  (let ((acc))
    (loop :with eof-value := (gensym)
	  :for form-count :from 0 :by 1
	  :for filepos := (progn
			    (peek-char t stream nil)
			    (file-position stream))
	  :for x := (read stream nil eof-value)
	  :until (eq x eof-value)
	  :do (match-search pattern x
			    #'(lambda (tree binds footprints)
				(push (make-search-result
				       :pathname pathname
				       :file-position filepos
				       :form-count form-count
				       :form tree
				       :binds binds
				       :footprints footprints)
				      acc))))
    (nreverse acc)))

(defun clep-stream (pattern stream)
  (clep-stream-internal pattern stream nil))

(defun clep-file-internal (pattern pathname)
  (with-open-file (in pathname)
    (clep-stream-internal pattern in pathname)))

(defun clep-files (pattern files)
  (when (not (listp files))
    (setf files (list files)))
  (let ((pathnames
	  (delete-duplicates (mapcar #'pathname files)
			     :test #'equal)))
    (loop :for pathname :in pathnames
	  :append (clep-file-internal pattern pathname))))
