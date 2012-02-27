#lang racket

(require racket/class)

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

(define (parse-error msg)
  (list 'parse-error msg))

(define (merge-errors . list)
  list)

;;; quick interlude to define an infinitely back-trackable view of a
;;; stream that we can use to efficiently always allow backtracking
;;; without requiring that everything be in memory before we begin or
;;; requiring that fully consumed tokens remain in memory
(define-syntax promise
  (syntax-rules ()
    ((promise . body)
     (mcons '() (lambda () (begin . body))))))

(define (deliver p)
  "deliver the result of computing the body of the promise, possibly
by evaluating body"
  (if (not (null? (mcdr p)))
      (let ((val ((mcdr p))))
	(set-mcar! p val)
	(set-mcdr! p '())
	val)
      (mcar p)))

;"represents a 'immutable' state of a stream during parse"
(define parser-state%
  (class object%
    (super-new)
    (init-field promise-stream
                value
                position)
    
    (define/public (state-current-item)
      (get-field value this))
    
    (define/public (state-advance-state)
      (deliver (get-field promise-stream this)))
    
    (define/public (state-empty?)
      (eof-object? (get-field value this)))))

(define (make-position row column)
  (cons row column))

(define (position-row pos)
  (car pos))

(define (position-column pos)
  (cdr pos))

(define (incr-pos pos value)
  (if (equal? value #\newline)
      (make-position (+ (position-row pos) 1)
		     1)
      (make-position (position-row pos)
		     (+ (position-column pos) 1))))

(define *position-zero* (make-position 1 1))

(define (state-from-stream stream pos)
  (let* ((value (read-char stream))
	 (next (promise (state-from-stream stream (incr-pos pos value)))))
    (new parser-state%
         [promise-stream next]
         [value value]
         [position pos])))

;;; that's the end of the interlude... on with the combinators

(define (always x)
  "always succeed with x as a value"
  (lambda (state cok cerr eok err)
    (eok x state)))

(define (nxt p q)
  "parse p then parse q"
  (lambda (state cok cerr eok eerr)
    (let ((pcok (lambda (item state)
		  (q state cok cerr cok cerr)))
	  (peok (lambda (item state)
		  (q state cok cerr eok eerr))))
      (p state pcok cerr peok eerr))))

(define (bind p f)
  "parse p and give the results to f to produce a new parser q which
will then be parsed"
  (lambda (state cok cerr eok eerr)
    (let ((pcok (lambda (item state)
		  (let ((q (f item)))
		    (q state cok cerr cok cerr))))
	  (peok (lambda (item state)
		  (let ((q (f item)))
		    (q state cok cerr eok eerr)))))
      (p state pcok cerr peok eerr))))

(define-syntax defparser
  (syntax-rules ()
    ((defparser name-and-arguments . body)
     (define name-and-arguments
       (lambda (state cok cerr eok eerr)
         (let ((parser (begin . body)))
           (parser state cok cerr eok eerr)))))))
  
(define-syntax >>
  (syntax-rules ()
    ((>> only-one)
     only-one)
    ((>> first . args)
     (nxt first (>> . args)))))

(define-syntax p-let
  (syntax-rules ()
    ((p-let ((var parser)) . body)
     (bind parser (lambda (var) . body)))
    ((p-let ((var parser) . bindings) . body)
     (bind parser (lambda (var) (p-let bindings . body))))))

(define (never)
  (lambda (state cok cerr eok eerr)
    (eerr (parse-error 'unknown))))

(define (either p q)
  (lambda (state cok cerr eok eerr)
    (let ((peerr (lambda (err-from-p)
		   (let ((qeerr (lambda (err-from-q)
				  (eerr (merge-errors err-from-p err-from-q)))))
		     (q state cok cerr eok qeerr)))))
      (p state cok cerr eok peerr))))

(define (token consume?)
  (lambda (state cok cerr eok eerr)
    (if (send state state-empty?)
	(eerr (parse-error "Input is empty"))
	(let ((item (send state state-current-item))
	      (next-state (send state state-advance-state)))
	  (if (consume? item)
	      (cok item next-state)
	      (eerr (parse-error (list 'unexpected item))))))))

(define (many p)
  "execute a parser 0 or more times"
  (lambda (state cok cerr eok eerr)
    (letrec ((many-err (lambda (x y)
			 (error "Combinator '*' applied to a parser that accepts the empty string")))
	     (pcok (lambda (coll)
		     (lambda (item state)
		       (let ((exit-cok (lambda (x)
					 (cok (reverse (cons item coll)) state))))
			 (p state (pcok (cons item coll)) cerr many-err exit-cok)))))
	     (peerr (lambda (x)
		      (eok '() state))))
      (p state (pcok '()) cerr many-err peerr))))

;; skipping times

(define (choice . parsers)
  (if (null? parsers)
      (never)
      (let ((p (first parsers)))
	(either p (apply choice (rest parsers))))))

(define (eof)
  (lambda (state cok cerr eok eerr)
    (if (send state state-empty?)
	(eok '() state)
	(eerr (parse-error "expected end of input")))))

(define satisfy token)

(define (char ch)
  (satisfy (lambda (val) (eq? val ch))))

(define (any-char)
  (satisfy (lambda (ch) #t)))

(define (char-in-bounds? ch lower upper)
  (and
   (>= (char->integer ch) (char->integer lower))
   (<= (char->integer ch) (char->integer upper))))

(define (digit? ch)
  (char-in-bounds? ch #\0 #\9))

(define (alpha? ch)
  (or (char-in-bounds? ch #\a #\z)
      (char-in-bounds? ch #\A #\Z)))

(define (digit)
  (satisfy (lambda (val) (digit? val))))

(define (letter)
  (satisfy (lambda (val) (alpha? val))))

(define (between open close p)
  (p-let ((_ open)
	  (x p)
	  (_ close))
    (always x)))

(define (many1 p)
  (p-let ((x p)
	  (xs (many p)))
    (always (cons x xs))))

(define (char->number ch)
  (- (char->integer ch) (char->integer #\0)))

(define (string-list->integer digits)
  (let loop ((digits (reverse digits))
             (place 1)
             (value 0))
    (if (null? digits)
        value
        (loop (cdr digits)
              (* place 10)
              (+ value (* (char->number (car digits)) place))))))

(defparser (positive-int)
  (p-let ((digits (many1 (digit))))
    (always (string-list->integer digits))))

(defparser (negative-int)
  (p-let ((num (>> (char #\-) (positive-int))))
    (always (- num))))

(defparser (whitespace)
  (many1 (choice (char #\newline)
		 (char #\space)
		 (char #\tab))))

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
  (between (char #\{) (char #\})
           (many1 (expression))))

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
