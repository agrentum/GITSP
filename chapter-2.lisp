;;;;; Here are solutions to chapter 2 of awesome book "Gentle Introduction
;;;;; to Symbolic Computation" [1] by even more awesome David S. Touretzky,
;;;;; who provided his work free of charge. Thank you.
;;;; To finish some rough edges I have supplemented myself with tutorial
;;;; http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial.html  [2]

;; Before anything else: I'm mostly a C programmer, far from real professional
;; coder. All input and critique is appreciated.

;;;;;;; This is VERY preliminary work! It is at the moment barely commented,
;;;;;;; but bare with me: lists are fundamental in Lisp. I want to be certain
;;;;;;; that what I am about to explain is going to be both true and clear.
;;;;;;; Comments are very welcome.

;;;; Before anything related to this chapter, I want to give you one useful
;;;; utility: assigning variables.
;;;; Variable is pretty much the same as famous x in mathematics. Only you
;;;; are not looking for its value, but can assign it youself. I will try to
;;;; use proper coding way, that is naming variables in a way that is descriptive
;;;; in terms of use of variable. It is pretty important difference from
;;;; variables in mathematics or physics. When you are implementing some code
;;;; most variables and functions will have specific use. It is just not clear
;;;; for other people (and in really big projects: to author) that x is name of
;;;; a client whos account balance is y etc. It is matter of convenience.
;;;; Some rules: common lisp interpretes are case-insensitive.
;;;; Variables should be written with '-' if name consists of many separate
;;;; words. So, lets see three ways of naming range of weapon as variable:
;;;; Range of a rifle - wrong variable name, we can't use spaces and case has
;;;; no meaning.
;;;; range-of-a-rifle - proper name of a variable, good style.
;;;; x - atrocious style, but valid name.

;;;; But how does assignment work? Simplest way I am going to use (not the only
;;;; one, but I don't want to confuse) is setq function.
;;;; Here is the sample syntax:
;;;; (setq name-of-the-variable value-assigned-to-variable)
;;;; So if we want to make use of variable range-of-a-rifle, and assign it value
;;;; 100 it goes like that:
;;;; (setq range-of-a-rifle 100)
;;;; and that is it.

;;;; Some more syntax and types: we can make any arbitrary set into a list by
;;;; putting " ' " before parenthesis. Experiment. What is different between
;;;; (+ 1 2) and '(+ 1 2)? Try it in interactive Common Lisp.

;;;; Fully commented code will come in small steps. I'm still pretty ill.

(defun *2-1* ()
  (write '(TO BE OR NOT TO BE)))

(defun *2-2* ()
  (format t "~S~%" '(A B (C)))
  (format t "~S~%" '((A) (B)))
  (format t "~S~%" '((A B) (C D)))
  (format t "~S~%" '(A (B (C)))))

(defun *2-3* ()
  '(PLEASE (BE MY) VALENTINE))

(defun *2-4* ()
  '((BOWS ARROWS) (FLOWERS CHOCOLATES)))

(defun *2-5* ()
  (setq first-list '(OPEN THE POD BAY DOORS HAL))
  (format t "~S consists of ~D elements.~%" first-list (length first-list))
  (setq second-list '((OPEN) (THE POD BAY DOORS) HAL))
  (format t "~S consists of ~D elements.~%" second-list (length second-list))
  (setq third-list '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
  (format t "~S consists of ~D elements.~%" third-list (length third-list)))