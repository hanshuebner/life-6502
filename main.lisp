;; -*- Lisp -*-

(defpackage :main
  (:use :cl :alexandria))

(in-package :main)

(defparameter *file* "life.s")

(setf *print-length* 40)

(defun assemble (listp)
  (multiple-value-bind (memory env) (cl-6502:asm (read-file-into-string *file*) :listp listp)
    (setf (cl-6502:get-range 0) memory)
    (setf cl-6502:*cpu* (make-instance '6502::symbolic-cpu :env env))))

(defun run (listp)
  (assemble listp)
  (cl-6502:execute cl-6502:*cpu*)
  (values cl-6502:*cpu* (cl-6502:get-range #x2400)))

