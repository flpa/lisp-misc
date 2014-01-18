; sloppy implementation of a word puzzle game - finding all words in a randomly 
; generated field of characters, e.g. :
;
; e k t
; n d x
; y q m
;
; contains 'end':
;
; e - - 
; n d -
; - - -
;
; TODOs
; refactor to use extracted dict utils 
; pretty print output with colors, bolded first letter, arrows between chars?
; each word can only be found once - watch out when implementing location memory
; location memory - remember where a word has been found
; filter words that are too long
; handle unicode chars
; profiling
; performance improvements
; other solutions (trie, sqlite db?)
; sorting output (will require collect instead of print, prevent direct printing)
; multi threading?


;  (ql:quickload :lisp-unit)
 ; (ql:quickload :log4cl)

(in-package :cl-user)

(defpackage :com.github.flpa.word-puzzle
  (:use :common-lisp :lisp-unit :log4cl))

(in-package :com.github.flpa.word-puzzle)

(defun random-character ()
  (code-char (+ (random 26) 97)))

(defparameter *field-size* 4)
(defparameter *field* (make-array (list *field-size* *field-size*)))


  

(defun init-field ()
  (log:info "Initializing field with size ~a" *field-size*)
  (loop
     for i
     from 0
     below (array-total-size *field*)
     do (setf
	 (row-major-aref *field* i)
	 (random-character))))

(defun print-field ()
  (loop for row from 0 below *field-size* do 
       (progn
	 (loop for col from 0 below *field-size* do
	      (format t "~c " (aref *field* row col)))
	 (format t "~%"))))

(defun string-starts-with (input prefix-candidate)
  (or
   (string-equal prefix-candidate input)
   (>= (string/= prefix-candidate input) (length prefix-candidate))))

(define-test strings-starts-with
  (assert-true (string-starts-with "abc" "a"))
  (assert-true (string-starts-with "a" "a"))
  (assert-true (string-starts-with "abc" ""))
  (assert-true (string-starts-with "" ""))
  (assert-true (string-starts-with "abc" "abc"))
  (assert-false (string-starts-with "bc" "a"))
  (assert-false (string-starts-with "" "a"))
  (assert-false (string-starts-with "bc" "a")))

(defparameter *test-words* `())

(defun words-starting-with (seq)
  (remove-if-not (lambda (x) (string-starts-with x seq)) *test-words*))

(defun get-neighbour-positions (pos)
  (remove-if `pos-out-of-field
	     (list
	      (- pos 1)
	      (+ pos 1)
	      (- 1 (pos-cell-next-row pos))
	      (pos-cell-next-row pos)
	      (+ 1 (pos-cell-next-row pos))
	      (- 1 (pos-cell-prev-row pos))
	      (pos-cell-prev-row pos)
	      (+ 1 (pos-cell-prev-row pos)))))

(defun pos-out-of-field (pos)
  (or
   (< pos 0)
   (>= pos (array-total-size *field*))))

(defun pos-cell-next-row (pos)
  (+ pos *field-size*))

(defun pos-cell-prev-row (pos)
  (- pos *field-size*))

(defun get-neighbour-chars (pos)
  (map `list `get-character-at-pos (get-neighbour-positions pos)))

(define-test get-neighbour-chars
  (let ((*field* #2A(
		     (#\a #\b #\c)
		     (#\d #\e #\f)
		     (#\g #\h #\i))))
    (assert-true (find #\b (get-neighbour-chars 0)))))
  
(defun get-character-at-pos (pos)
  (row-major-aref *field* pos))

(defun word-puzzle-simple ()
  "first version of word puzzle"
  (load-dict)
  (init-field)
  (print-field)
  (play-word-puzzle-simple))

(defun play-word-puzzle-simple ()
  (log:info "Playing Word puzzle simple")
  (loop for start-char-pos 
     from 0 
     below (array-total-size *field*) 
     do
       (let
	   ((visited-pos (list start-char-pos))
	    (current-string (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)))
	 (check-pos start-char-pos current-string visited-pos))))

(defun find-matching-word (words word)
  (find word words :test `string-equal))

(defconstant +min-word-size+ 3)

(defun check-pos (pos current-string visited-pos)
  (vector-push-extend (get-character-at-pos pos) current-string)
  (let ((remaining-words (words-starting-with current-string)))
    (if (> (length remaining-words) 0)
	(progn
	  (if (and
	       (>= (length current-string) +min-word-size+)
	       (find-matching-word remaining-words current-string))
	      (progn
		(format t "found: ~a~%" current-string)
		(force-output t)))
	  (loop for neighbour in (get-neighbour-positions pos) do
	       (if (not (find neighbour visited-pos))
		   (check-pos neighbour current-string visited-pos)))
	  ))
    (vector-pop current-string)))

(defconstant +dict-location+ "/usr/share/dict/words")

(defun load-dict ()
  (log:info "Loading dictionary")
  (defparameter *test-words* `(""))
  (let ((in (open +dict-location+ :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
         while line do (nconc *test-words* (list line)))
      (close in))))