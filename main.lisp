;; -*- Lisp -*-

(defpackage :main
  (:use :cl :alexandria))

(in-package :main)

(defparameter *file* "life.s")

(setf *print-length* 40)

(defun sorted-symbols (hash-table)
  (sort (loop for key being the hash-keys of hash-table collect key) #'string-lessp))

(defun assemble ()
  (multiple-value-bind (output env) (cl-6502:asm (read-file-into-string *file*))
    (dolist (symbol (sorted-symbols env))
      (format t "~20,,,' A ~4,'0X~%" symbol (gethash symbol env)))
    output))

(defun run ()
  (setf (cl-6502:get-range 0) (assemble))
  (cl-6502:reset cl-6502:*cpu*)
  (cl-6502:execute cl-6502:*cpu*)
  (cl-6502:get-range #x2400))

