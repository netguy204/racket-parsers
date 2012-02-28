#lang racket

(require "parser.rkt")

(defparser (symbol)
  (p-let ((f (letter))
	  (r (many (either (letter)
			   (digit)))))
    (always (list->string (cons f r)))))

(define (skip-many skipped parser)
  (nxt (many skipped) parser))

(defparser (ben-integer)
  (between (char #\i) (char #\e)
	   (either
	    (positive-int)
	    (negative-int))))

(defparser (expression-break)
  (choice (char #\n)
          (char #\;)))

(defparser (horizontal-whitespace)
  (choice (char #\space)
          (char #\tab)))

(defparser (block)
  (between (char #\{) (char #\})
    (p-let ((first (expression))
            (rest (many (skip-many (expression-break) (expression)))))
      (always (cons 'block (cons first rest))))))

(defparser (expression)
  (skip-many (whitespace)
   (choice (assignment))))

(defparser (literal)
  (choice (symbol)
          (positive-int)
          (negative-int)))

(defparser (assignment)
  (p-let ((var (symbol))
          (_ (skip-many (whitespace) (char #\=)))
          (value (skip-many (whitespace)
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
(parse-string (block) "{
a = b = 16; c = 12
d = 15}")

;;; yeah, that was exciting.