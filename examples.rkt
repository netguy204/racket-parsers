#lang racket

(require "parser.rkt")

(defparser (symbol)
  (p-let ((f (letter))
	  (r (many (either (letter)
			   (digit)))))
    (always (list->string (cons f r)))))

(define (skip-whitespace parser)
  (nxt (many (whitespace)) parser))

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
  (skip-whitespace
   (choice (assignment))))

(defparser (literal)
  (choice (symbol)
          (positive-int)
          (negative-int)))

(defparser (assignment)
  (p-let ((var (symbol))
          (_ (skip-whitespace (char #\=)))
          (value (skip-whitespace
                  (choice
                   (expression)
                   (literal)))))
          
    (always (list 'assign var value))))

;;; useful for testing
(define (identity2 name) (lambda (a b) (list name a b)))
(define (identity1 name) (lambda (a) (list name a)))
(define (identity* name) (lambda a (cons name a)))

;;; how to use it

(define (run-parser p input)
  (p (state-from-stream input *position-zero*)
     (identity2 'cok) (identity1 'cerr) (identity2 'eok) (identity1 'eerr)))

(define (parse-string p str)
  (run-parser p (open-input-string str)))

;;; and finally, doing something
(parse-string (block) "{a = b = 16}")

;;; yeah, that was exciting.