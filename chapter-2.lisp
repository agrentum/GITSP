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
;;;; are not looking for its value, but can assign it yourself. I will try to
;;;; use proper coding way, that is naming variables in a way that is descriptive
;;;; in terms of use of variable. It is pretty important difference from
;;;; variables in mathematics or physics. When you are implementing some code
;;;; most variables and functions will have specific use. It is just not clear
;;;; for other people (and in really big projects: to author) that x is name of
;;;; a client whose account balance is y etc. It is matter of convenience.
;;;; Some rules: common lisp interpreters are case-insensitive.
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
;;;; I'm going to use them in a bad fashion, interchangeably.
;;;; Fully commented code will come in small steps. I'm still pretty ill.
;;;; And as you can see, it has severe impact on my coding performance.

(defun 2-1 ()
  (write (list 'TO 'BE 'OR 'NOT 'TO 'BE)))

(defun 2-2 ()
  (format t "~S~%" (list 'A 'B '('C)))
  (format t "~S~%" '((A) (B)))
  (format t "~S~%" '((A B) (C D)))
  (format t "~S~%" '(A (B (C)))))

(defun 2-3 ()
  '(PLEASE (BE MY) VALENTINE))

(defun 2-4 ()
  '((BOWS ARROWS) (FLOWERS CHOCOLATES)))

;;; Before going further, let me tell you how sorry I am for not telling
;;; it sooner, but I do not want to throw a lot of stuff from the beginning
;;; but there was some other material along the way.
;;; To use variables and give them value you need to declare it.
;;; Chapter 1 was full of warnings caused by the fact that I have been lazy.
;;; It still works, but it is not proper way to code.
;;; We declare variables by use of defvar. Basic syntax is simply:
;;; (defar name-of-variable)

(defvar *first-list*)
(defvar *second-list*)
(defvar *third-list*)

;;; But if we know what should be the value of variable, we can assign it
;;; while declaring the variable.
;;; Like you can see below, syntax is straightforward:
;;; (defar name-of-variable value-of-variable)
;;; and that value can change later, by the use of setq or other methods.
(defvar *fourth-list* (list 'There 'Were 'Some 'States 'In 'Original 'example))

;;; One last thing: you can see that I am writing it all different now.
;;; I have been reading up about notation used, and it seems that I knew some
;;; elements in pretty ecclecting and wrong way.

;;; Global variable is a variable that is available for all code. Functions can
;;; use them directly, can change them etc. They are written with asterisks (*)
;;; around, to distinguish between them. I'm sorry for making a mess, chapter 1
;;; is going to be corrected.

(defun 2-5 ()
  (setq *first-list* '(OPEN THE POD BAY DOORS HAL))
  (format t "~S consists of ~D elements.~%" *first-list* (length *first-list*))
  (setq *second-list* '((OPEN) (THE POD BAY DOORS) HAL))
  (format t "~S consists of ~D elements.~%" *second-list* (length *second-list*))
  (setq *third-list* '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
  (format t "~S consists of ~D elements.~%" *third-list* (length *third-list*))
	(format t "~S consists of ~D elements.~%" *fourth-list* (length *fourth-list*)))

;;; Problem 2-6 is one where instead of implementation I'm going to try and show
;;; my reasoning that followed reading section 6, chapter 2 [1].
;;; It is fairly simple. Let's start from revising concept of nested list.
;;; This is simplest list (list 'element1 'element2)
;;; Nested list is a list within a list, so:
;;; (list (list 'element1-list1 'element2-list1) (list 'element1-list2 'element2-list2))
;;; there is no limit on nesting lists (theoretically speaking, You could probably
;;; waste all of your memory on it and prove it wrong ;P) and lists are not required
;;; to have equal number of elements.
;;;;; Find better English translation.
