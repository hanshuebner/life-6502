;; -*- Lisp -*-

(defpackage :main
  (:use :cl :alexandria))

(in-package :main)

(defparameter *filename* "life.s")

(setf *print-length* 40)

(defun assemble (&optional (listp t))
  (multiple-value-bind (memory env)
      (cl-6502:asm (read-file-into-string *filename*) :listp listp)
    (setf (cl-6502:get-range 0) memory)
    (setf cl-6502:*cpu* (make-instance '6502::symbolic-cpu :env env))))

(defun assemble-to-files (&key (filename *filename*) (origin #x2000))
  (let* ((listing-filename (make-pathname :type "lis" :defaults filename))
         (memory (with-open-file (*standard-output* listing-filename
                                                    :direction :output
                                                    :if-does-not-exist :create
                                                    :if-exists :supersede)
                   (cl-6502:asm (read-file-into-string filename) :listp t))))
    (format t "assembled, listing file is ~A~%" listing-filename)
    (with-open-file (binary-file (make-pathname :type "bin" :defaults filename)
                                 :element-type '(unsigned-byte 8)
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
      (format t "Writing ~A bytes to ~A~%" (length (subseq memory origin)) (pathname binary-file))
      (write-sequence (subseq memory origin) binary-file)
      (values (pathname binary-file)
              listing-filename))))

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
  (loop with base = (resolve-address base)
        with memory = (cl-6502:get-range base)
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
                (if (oddp generation) "buf0" "buf1"))))

(defun run (&optional listp)
  (assemble listp)
  (tick))

(defun sh (program arguments &key input)
  (format t "$ ~A ~A~%" program arguments)
  (let* ((process (sb-ext:run-program program arguments
                                      :search t :output :stream :error :output
                                      :input input))
         (output (read-stream-content-into-string (sb-ext:process-output process))))
    (format t "~A" output)
    (unless (zerop (sb-ext:process-exit-code process))
      (error "Exit status ~A" (sb-ext:process-exit-code process)))))

(defun make ()
  (let* ((binary-filename (namestring (assemble-to-files)))
         (image-filename (namestring (make-pathname :type "img" :defaults binary-filename)))
         (name (pathname-name binary-filename)))
    (format t "deleting old files~%")
    (sh "acx" `("rm" "-d" ,image-filename "*"))
    (format t "saving binary~%")
    (with-open-file (binary binary-filename :element-type '(unsigned-byte 8))
      (sh "ac" `("-p" ,image-filename ,name "bin")
          :input binary))
    (format t "done~%")))
