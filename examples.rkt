#lang racket

(require "parser.rkt")

(defparser (symbol)
  (p-let ((f (letter))
	  (r (many (either (letter)
			   (digit)))))
    (always (list->string (cons f r)))))


(defparser (ben-integer)
  (between (char #\i) (char #\e)
	   (either
	    (positive-int)
	    (negative-int))))

(defparser (block)
  (p-let ((exprs (between (char #\{) (char #\})
                          (many1 (expression)))))
     (always (list 'block exprs))))

(defparser (expression)
  (choice (assignment)))

(defparser (literal)
  (choice (symbol)
          (positive-int)
          (negative-int)))

(defparser (assignment)
  (p-let ((var (symbol))
          (_ (char #\=))
          (value (choice
                  (expression)
                  (literal))))
          
    (always (list 'assign var value))))

;;; useful for testing
(define (identity2 name) (lambda (a b) (list name a b)))
(define (identity1 name) (lambda (a) (list name a)))
(define (identity* name) (lambda a (cons name a)))

;;; how to use it
;((always2 "foo") nil (identity2 'cok) (identity2 'cerr) (identity2 'eok) (identity1 'eerr))
;(set! *s* (state-from-stream (make-string-buffer "foobar") *position-zero*))

(define (run-parser p input)
  (p (state-from-stream input *position-zero*)
     (identity2 'cok) (identity1 'cerr) (identity2 'eok) (identity1 'eerr)))

(define (parse-string p str)
  (run-parser p (open-input-string str)))
