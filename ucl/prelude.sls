#!r6rs
(library (ucl prelude)
  (export curry compose
          nub nub-by
          show template print
          intersperse
          break-on break-string
          for range
          with-warning with-error replace-error
          cleanup

          ;; ucl prelude data
          get-data put-data
          read-file-data write-file-data
          with-file-data)
  (import (rnrs) (ucl prelude data))

;; CURRY f . xs
;;   Partially apply function F with arguments XS.
(define (curry f . xs) (lambda y (apply f (append xs y))))

;; COMPOSE . fs
;;   Compose the functions like (lambda (x) (f1 (f2 x)))
(define (compose . fs)
  (if (null? fs)
    (lambda (x) x)
    (lambda (x) ((car fs) ((apply compose (cdr fs)) x)))))

;; NUB xs
;;  Remove duplicate elements from XS
(define (nub xs) (curry nub-by equal?))

;; NUB-BY xs
;;  Remove duplicate elements from XS according to predicate EQ
(define (nub-by eq xs)
  (if (null? xs)
      '()
      (let ((e (car xs)) (es (cdr xs)))
        (cons e (filter (lambda (x) (not (eq e x))) es)))))

;; SHOW obj
;;   Display OBJ to a string exactly as it would be output by DISPLAY
(define (show obj) (call-with-string-output-port (curry display obj)))

;; TEMPLATE str . vals
;;   Return STR, but with % characters replaced with the
;;   provided values, as they would be rendered by DISPLAY.
(define (template str . vals)
  (let* ((chunks (break-on (curry equal? #\%) (string->list str)))
         (strs   (map list->string chunks))
         (objs   (map show vals))
         (all    (cons (car strs) (apply append (map list objs (cdr strs))))))
    (apply string-append all)))

;; PRINT str . vals
;;   Apply the DISPLAY function to the result of (TEMPLATE str . vals)
(define (print str . vals) (display (apply template str vals)))

;; INTERSPERSE e l
;;   Intersperses value E between the elements of L.
(define (intersperse e l) (cdr (apply append (map (lambda x (cons e x)) l))))

;; BREAK-ON p xs
;;   Divide a list into multiple lists, with their boundaries
;;   at the elements for which (p x) is true. Values satisfying
;;   p are removed entirely.
(define (break-on p xs)
   (let loop ((xs xs) (ys '()) (zs '()))
     (if (equal? xs '())
         (reverse (cons (reverse ys) zs))
         (if (p (car xs))
             (loop (cdr xs) '() (cons (reverse ys) zs))
             (loop (cdr xs) (cons (car xs) ys) zs)))))

;; BREAK-STRING chr str
;;  Break the string STR into substrings separated by CHR
(define (break-string chr str)
  (map list->string
    (filter (lambda (x) (not (null? x)))
      (break-on (curry equal? chr)
        (string->list str)))))

;; FOR ((name (value ...)) ...) expr ...
;;   Like LET, but iterates over all possible combinations of
;;   binding choices.
(define-syntax for
  (syntax-rules (for)
    ((for () f ...) (begin f ...))
    ((for ((name vals) . rest) f ...)
     (map (lambda (name) (for rest f ...)) vals))))

;; RANGE x y
;;   Return the numeric range [X,Y)
(define (range x y) (do ((i (- y 1) (- i 1)) (a '() (cons i a))) ((< i x) a)))

;; WITH-WARNING msg ret thunk
;;   Execute THUNK, trapping all errors and emitting MSG if they occur,
;;   eventually continuing with the value RET
(define-syntax with-warning
  (syntax-rules ()
    ((_ msg ret thunk) (guard (ex (#t (begin (display msg) ret))) thunk))))

;; WITH-ERROR msg thunk
;;   Execute THUNK, emitting MSG on error and then aborting the program
(define-syntax with-error
  (syntax-rules ()
    ((_ msg thunk) (guard (ex (#t (begin (display msg) (exit #f)))) thunk))))

;; REPLACE-ERROR err thunk
;;   Execute THUNK, catching any errors and then throwing ERR
(define-syntax replace-error
  (syntax-rules ()
    ((_ err thunk) (guard (ex (#t err)) thunk))))

;; CLEANUP c e
;;   Evaluate expression E, perform cleanup action C, return the
;;   value from evaluating E
(define-syntax cleanup (syntax-rules () ((_ c e) (let ((r e)) c r))))

)
