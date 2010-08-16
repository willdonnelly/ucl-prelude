#!r6rs
(library (ucl prelude data)
  (export get-data put-data read-file-data write-file-data with-file-data)
  (import (rnrs))

(define (get-data port)
  (let ((datum (get-datum port)))
    (if (eof-object? datum) '() (cons datum (get-data port)))))

(define (read-file-data file)
  (if (file-exists? file)
      (call-with-input-file file get-data)
      '()))

(define (put-data port data)
  (unless (null? data)
    (put-datum port (car data))
    (newline port)
    (put-data port (cdr data))))

(define (write-file-data file data)
  (when (file-exists? file) (delete-file file))
  (call-with-output-file file (lambda (port) (put-data port data))))

(define (with-file-data file proc)
  (write-file-data file (proc (read-file-data file))))
)
