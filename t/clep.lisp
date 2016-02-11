(in-package :cl-user)
(defpackage clep-test
  (:use
   :cl
   :clep
   :prove))
(in-package :clep-test)

(plan nil)

(defun test (search-result-list &rest args-list)
  (loop :for search-result :in search-result-list
	:for args :in args-list
	:do (destructuring-bind
		(&key form binds footprints pathname form-count)
		args
	      (is-type search-result 'search-result)
	      (is (search-result-form search-result) form :test #'equal)
	      (is (search-result-binds search-result) binds :test #'equalp)
	      (is (search-result-footprints search-result) footprints :test #'equal)
	      (is (search-result-pathname search-result) pathname :test #'equal)
	      (is (search-result-form-count search-result) form-count :test #'equal))))

(test (clep-sexp 'foo 'foo)
      '(:form foo
	:binds #()))

(test (clep-sexp '(:*) 'foo)
      '(:form foo
	:binds #(foo)))

(test (clep-sexp '(setf (:lisp symbolp) (:*))
		 '(setf var (f x)))
      '(:form (setf var (f x))
	:binds #(var (f x))))

(test (clep-sexp `(setf (:lisp ,#'symbolp) (:*))
		 '(setf var (f x)))
      '(:form (setf var (f x))
	:binds #(var (f x))))

(defvar *base-directory*
  (merge-pathnames "t/samples/" (asdf:system-source-directory :clep)))

(defun test-files (pattern filenames &rest args)
  (apply #'test
	 (clep-files pattern
		     (mapcar #'(lambda (filename)
				 (merge-pathnames filename *base-directory*))
			     filenames))
	 args))

(test-files '(princ (:*))
	    '("dice_of_doom_v1.lisp"
	      "svg.lisp")
	    `(:FORM (PRINC "  ")
	      :BINDS #("  ")
	      :FOOTPRINTS (3 6 2 4)
	      :PATHNAME ,(merge-pathnames "dice_of_doom_v1.lisp" *base-directory*)
	      :FORM-COUNT 7)
	    `(:FORM (PRINC "choose your move:")
	      :BINDS #("choose your move:")
	      :FOOTPRINTS (4)
	      :PATHNAME ,(merge-pathnames "dice_of_doom_v1.lisp" *base-directory*)
	      :FORM-COUNT 16)
	    `(:FORM (PRINC "end turn")
	      :BINDS #("end turn")
	      :FOOTPRINTS (5 2 10 4 3)
	      :PATHNAME ,(merge-pathnames "dice_of_doom_v1.lisp" *base-directory*)
	      :FORM-COUNT 16)
	    `(:FORM (PRINC #\<)
	      :BINDS #(#\<)
	      :FOOTPRINTS (3)
	      :PATHNAME ,(merge-pathnames "svg.lisp" *base-directory*)
	      :FORM-COUNT 3)
	    `(:FORM (PRINC #\/)
	      :BINDS #(#\/)
	      :FOOTPRINTS (4 2)
	      :PATHNAME ,(merge-pathnames "svg.lisp" *base-directory*)
	      :FORM-COUNT 3)
	    `(:FORM (PRINC (STRING-DOWNCASE NAME))
	      :BINDS #((STRING-DOWNCASE NAME))
	      :FOOTPRINTS (5)
	      :PATHNAME ,(merge-pathnames "svg.lisp" *base-directory*)
	      :FORM-COUNT 3)
	    `(:FORM (PRINC #\>)
	      :BINDS #(#\>)
	      :FOOTPRINTS (7)
	      :PATHNAME ,(merge-pathnames "svg.lisp" *base-directory*)
	      :FORM-COUNT 3))

(finalize)
