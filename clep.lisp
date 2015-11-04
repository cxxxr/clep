;; -*- Mode: LISP; Package: CLEP -*-

(defpackage clep
  (:use :cl)
  (:export :clep))

(in-package :clep)

(defvar *operator-mark* (gensym "OP"))

(set-macro-character #\} (get-macro-character #\)))
(set-macro-character #\{
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (cons *operator-mark*
                               (read-delimited-list
                                #\} stream t))))

(defun %match (pattern expr binds)
  (cond ((and (symbolp pattern)
              (symbolp expr)
              (equal (symbol-name pattern)
                     (symbol-name expr)))
         (values binds t))
        ((and (consp pattern)
              (eq *operator-mark* (first pattern)))
         (ecase (second pattern)
           ((:lisp)
            (when (funcall (third pattern) expr)
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

(defun match-search (pattern tree matched-fn)
  (multiple-value-bind (binds matched-p)
      (match pattern tree)
    (when matched-p
      (funcall matched-fn tree binds)))
  (when (consp tree)
    (match-search pattern (car tree) matched-fn)
    (match-search pattern (cdr tree) matched-fn)))

(defun clep-file (pattern filename)
  (with-open-file (in filename)
    (let ((acc))
      (loop :with eof-value := (gensym)
        :for form-count :from 0 :by 1
        :for x := (read in nil eof-value)
        :until (eq x eof-value)
        :do (match-search pattern x
                          #'(lambda (tree binds)
                              (push (list filename
                                          form-count
                                          tree binds)
                                    acc))))
      (nreverse acc))))

(defun collect-lisp-files ()
  (loop :for pathname :in (cl-fad:list-directory "." :follow-symlinks nil)
    :when (equal "lisp" (pathname-type pathname))
    :collect (namestring pathname)))

(defun clep (pattern &rest args)
  (let ((filenames
         (if (null args)
             (collect-lisp-files)
             (mapcan #'(lambda (arg)
                         (etypecase arg
                           (string (list arg))
                           (list arg)))
                     args))))
    (loop :for filename :in filenames
      :append (clep-file pattern filename))))
