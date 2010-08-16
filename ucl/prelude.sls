#!r6rs
(library (ucl prelude)
  (export curry compose
          show print
          nub nub-by intersperse break-on break-string drop-while group-by
          for range with-warning with-error
          get-data put-data read-file-data write-file-data with-file-data)
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

;; SHOW obj
;;   Display OBJ to a string exactly as it would be output by DISPLAY
(define (show obj) (call-with-string-output-port (curry display obj)))

;; PRINT str . vals
;;   Return STR, but with % characters replaced with the
;;   provided values, as they would be rendered by DISPLAY.
(define (print str . vals)
  (let* ((chunks (break-on (curry equal? #\%) (string->list str)))
         (strs   (map list->string chunks))
         (objs   (map show vals))
         (all    (cons (car strs) (apply append (map list objs (cdr strs))))))
    (apply string-append all)))

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

;; FOR ((name (value ...)) ...) expr ...
;;   Like LET, but iterates over all possible combinations of
;;   binding choices.
(define-syntax for
  (syntax-rules (for)
    ((for () f ...) (begin f ...))
    ((for ((name vals) . rest) f ...)
     (map (lambda (name) (for rest f ...)) vals))))

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

;; DROP-WHILE p xs
;;  Drop elements from the head of the list until (p (car xs)) is false
(define (drop-while p xs) (if (p (car xs)) (drop-while p (cdr xs)) xs))

;; INTERSPERSE e l
;;   Intersperses value E between the elements of L.
(define (intersperse e l) (cdr (apply append (map (lambda x (cons e x)) l))))

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

;; BREAK-STRING chr str
;;  Break the string STR into substrings separated by CHR
(define (break-string chr str) (map list->string (filter (lambda (x) (not (null? x))) (break-on (curry equal? chr) (string->list str)))))

;; RANGE a b
;;   Return the numeric range [A,B)
(define (range a b) (let loop ((x b) (xs '())) (if (< a x) (loop (- x 1) (cons (- x 1) xs)) xs)))

;; GROUP-BY eq xs
;;   Group sets of elements that EQ says are equal
(define (group-by eq xs)
  (define (group-by* eq xs gs)
    (if (null? xs) gs
      (if (eq (car xs) (caar gs))
          (group-by* eq (cdr xs) (cons (cons (car xs) (car gs)) (cdr gs)))
          (group-by* eq (cdr xs) (cons (list (car xs)) gs)))))
  (reverse (map reverse
    (group-by* eq (cdr xs) `((,(car xs)))))))

)
