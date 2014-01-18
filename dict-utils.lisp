;; utils initially copied from word puzzle, need cleanup

;(defparameter *dict-location* "/usr/share/dict/words")
;(defparameter *dict-location* "/tmp/mywords")
(defparameter *dict-location* "/usr/share/dict/british-english-small")

(defparameter *words* `())

(defun load-dict (&optional (filter (lambda (x) t)))
  "Loads the dictionary defined by *dict-location* into *words*. Lines can be filtered by providing a filter-function."
  (defparameter *words* `(""))
  (let ((in (open *dict-location* :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
         while line
	 when (funcall filter line)
	 do (nconc *words* (list line)))
      (close in))))

(defun string-starts-with (input prefix-candidate)
  (or
   (string-equal prefix-candidate input)
   (>= (string/= prefix-candidate input) (length prefix-candidate))))

;; (define-test strings-starts-with
;;   (assert-true (string-starts-with "abc" "a"))
;;   (assert-true (string-starts-with "a" "a"))
;;   (assert-true (string-starts-with "abc" ""))
;;   (assert-true (string-starts-with "" ""))
;;   (assert-true (string-starts-with "abc" "abc"))
;;   (assert-false (string-starts-with "bc" "a"))
;;   (assert-false (string-starts-with "" "a"))
;;   (assert-false (string-starts-with "bc" "a")))

(defun words-starting-with (seq)
  (remove-if-not (lambda (x) (string-starts-with x seq)) *words*))

(defun get-if (fn list &optional (count 0))
  "Returns a list of elements of list satisfying fn, optionally limited by count."
  (loop 
     for val in list 
     when (funcall fn val)
     collect val into result
     until (> (length result) count)
     finally (return result)))

(defun words-starting-with (string &optional (count (length *words*)))
  (get-if (lambda (x) (string-starts-with x string)) *words* count))

