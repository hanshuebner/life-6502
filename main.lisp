;; -*- Lisp -*-

(defpackage :main
  (:use :cl :alexandria))

(in-package :main)

(defparameter *file* "life.s")

(setf *print-length* 40)

(defun assemble (&optional (listp t))
  (multiple-value-bind (memory env)
      (cl-6502:asm (read-file-into-string *file*) :listp listp)
    (setf (cl-6502:get-range 0) memory)
    (setf cl-6502:*cpu* (make-instance '6502::symbolic-cpu :env env))))

(defun resolve-address (address)
  (if (stringp address)
      (gethash address (6502::cpu-env cl-6502:*cpu*))
      address))

(defun pokew (address value)
  (let ((mem (cl-6502:get-range 0))
        (address (resolve-address address)))
    (setf (aref mem address) (logand value #xff)
          (aref mem (1+ address)) (ash value -8))))

(defun peek (address)
  (aref (cl-6502:get-range 0 #x100) (resolve-address address)))

(defun peekw (address)
  (let ((address (resolve-address address)))
    (logior (peek address)
            (ash (peek (1+ address)) 8))))

(defun dump-neighbors ()
  (loop with base = (resolve-address "wrap_top_neighbors")
        with memory = (cl-6502:get-range base)
        for row below 26
        do (format t "$~4,'0X  " (+ base (* row 40)))
        do (format t "~2,'0D  " (1- row))
        do (loop for col below 40
                 do (format t "~D " (aref memory (+ (* row 40) col))))
        do (terpri)))

(defun dump-state (name base)
  "LSB first as that is how we determine neighbors, too"
  (format t ";; ~A:~%" name)
  (loop with memory = (cl-6502:get-range (resolve-address base))
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
  (format nil "; inbyte $~2,'0X outbyte $~2,'0X row $~2,'0X inrow $~4,'0X outrow $~4,'0X neighrow $~4,'0X~%~A"
          (peek "inbyte")
          (peek "outbyte")
          (peek "row")
          (peekw "inrow")
          (peekw "outrow")
          (peekw "neighrow")
          cl-6502:*cpu*))

(defun apple-ii-text-row-starts ()
  (loop for row below 24
        do (format t " .word  $~2,'0X~2,'0X~%"
                   (logior #b00000100 (logand #b00000011 (ash row -1)))
                   (logior (ash (logand row #b00000001) 7)
                           (ash (logand row #b00011000) 2)
                           (logand row #b00011000)))))

(defun show ()
  (dump-state "buf0" "buf0")
  (dump-state "buf1" "buf1")
  (dump-neighbors)
  cl-6502:*cpu*)

(defun tick ()
  (let ((generation (peek "generation")))
    (cl-6502:execute cl-6502:*cpu*)
    (dump-state (format nil "Generation ~D" generation)
                (if (oddp generation) #x2000 #x2078))))

(defun run (&optional listp)
  (assemble listp)
  (tick))

