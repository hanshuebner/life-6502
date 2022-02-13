;; -*- Lisp -*-

(defpackage :main
  (:use :cl :alexandria))

(in-package :main)

(defparameter *file* "life.s")

(setf *print-length* 40)

(defun assemble (listp)
  (multiple-value-bind (memory env)
      (cl-6502:asm (read-file-into-string *file*) :listp listp)
    (setf (cl-6502:get-range 0) memory)
    (setf cl-6502:*cpu* (make-instance '6502::symbolic-cpu :env env))))

(defun dump-neighbors ()
  (loop with base = #x2400
        with memory = (cl-6502:get-range base)
        for row below 26
        do (format t "$~4,'0X  " (+ base (* row 40)))
        do (format t "~2,'0D  " row)
        do (loop for col below 40
                 do (format t "~D " (aref memory (+ (* row 40) col))))
        do (terpri)))

(defun dump-state (name base)
  "LSB first as that is how we determine neighbors, too"
  (format t ";; ~A:~%" name)
  (loop with memory = (cl-6502:get-range base)
        for row below 24
        do (format t "$~4,'0X  " (+ base (* row 5)))
        do (format t "~2,'0D  " row)
        do (loop for offset below 5
                 do (loop with byte = (aref memory (+ (* row 5) offset))
                          for i below 8
                          do (princ (if (zerop (logand byte #x01))
                                        #\0 #\1))
                          do (setf byte (ash byte -1))))
        do (terpri)))

(defun debug-info ()
  (let ((zp (cl-6502:get-range 0 #x100)))
    (format nil "; inbyte $~2,'0X outbyte $~2,'0X row $~2,'0X inrow $~2,'0X~2,'0X outrow $~2,'0X~2,'0X neighrow $~2,'0X~2,'0X~%~A"
            (aref zp #x05)
            (aref zp #x06)
            (aref zp #x07)
            (aref zp #x09)
            (aref zp #x08)
            (aref zp #x0b)
            (aref zp #x0a)
            (aref zp #x0d)
            (aref zp #x0c)
            cl-6502:*cpu*)))

(defun run (listp)
  (assemble listp)
  (cl-6502:execute cl-6502:*cpu*)
  (dump-state "buf0" #x2000)
  (dump-state "buf1" #x2078)
  (dump-neighbors)
  cl-6502:*cpu*)

