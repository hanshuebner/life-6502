;; -*- Lisp -*-

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push *default-pathname-defaults* asdf:*central-registry*)
(ql:quickload :life-6502)
