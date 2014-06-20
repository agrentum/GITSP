;;;;; Collection of small games and utilities in Common Lisp
;;;;; A little supplement to solution.

;;;; First of all, let me throw some useful functions at you.
;;;; They are pretty self-explanatory by name, but I will try to
;;;; comment them a bit.

;;;; setq - It allows assiging value to a named variable.
;;;; read - you can type your input into program! I know, it is
;;;; just that simple!
;;;; random - returns a random number from 0 to some parameter - 1.
;;;; here is an example that returns result from 0 to 5:
;;;; (random 6)
;;;; But that does not look appealing for most applications. Lets make
;;;; six-sided die:
;;;; (+ 1 (random 6))
;;;; That is it.
;;;; and... that is all. I have already explained how If and format
;;;; works. What is interesting is the fact that you can combine them
;;;; to make more complex logical decision trees.
;;;; I have tried to name everything in game below in descriptive and
;;;; clear way, while using small number of functions.

;;; Guess the number
(defun *guess-the-number* ()
  (setq number-of-tries 0)
  (setq target-number (+ 1 (random 100)))
  (format t "Guess what number I have in memory, from 1 to 100~%")
  (loop
     (setq number-of-tries (+ 1 number-of-tries))
     (format t "Your ~D try, give me a number: " nr-of-tries)
     (setq number-guess (read))
     (if (equal number-guess target-number) (return "CORRECT!")
	 (if (> number-guess target-number)
	     (format t "I am thinking of a lower number.~%")
	     (format t "I am thinking of a higher number.~%")))))
