#!r6rs
(library (ucl prelude data)
  (export get-data put-data read-file-data write-file-data with-file-data)
  (import (rnrs))

;; GET-DATA port
;;   Read a sequence of scheme data from PORT until EOF, and
;;   return the data as a list
(define (get-data port)
  (let ((datum (get-datum port)))
    (if (eof-object? datum) '() (cons datum (get-data port)))))

;; READ-FILE-DATA file
;;   Read a sequence of scheme data from FILE, returning the
;;   empty list if it doesn't exist
(define (read-file-data file)
  (if (file-exists? file)
      (call-with-input-file file get-data)
      '()))

;; PUT-DATA port
;;   Write a list of values to PORT, separated by newlines
(define (put-data port data)
  (unless (null? data)
    (put-datum port (car data))
    (newline port)
    (put-data port (cdr data))))

;; WRITE-FILE-DATA file data
;;   Write a list of values to FILE, one per line, overwriting
;;   any previous contents the file may have had
(define (write-file-data file data)
  (when (file-exists? file) (delete-file file))
  (call-with-output-file file (lambda (port) (put-data port data))))

;; WITH-FILE-DATA file proc
;;   Read the contents of FILE and pass them to PROC, and if
;;   PROC returns, writing the return value back to FILE. If
;;   PROC never returns, for example, because of an exception,
;;   the contents of FILE will be untouched.
(define (with-file-data file proc)
  (write-file-data file (proc (read-file-data file))))
)
