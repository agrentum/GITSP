;;;; Here are solutions to chapter 1 of awesome book "Gentle Introduction
;;;; to Symbolic Computation" [1] by even more awesome David S. Touretzky.
;;; To finish some rough edges I have supplemented myself with tutorial
;;; http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial.html  [2]

;; Everything here was coded in Emacs 23 with SLIME and autocomplete tool.
;; +++++
;; Stallman protects.
;; +++++

;; Solution to problem 1-1.
(defun *1-1* ()
  ; How does format t syntax work? As presented in chapter 8 of [2],
  ; ~D lets you to pass number to printed sequence. equivalent to %d
					; in C.
  ; ~% makes new line. Equivalent to \n
  (format t "Result of 6 + 7 is ~D ~%" (+ 6 7))
  (format t "Result of x * 4 = 12 <=> x = 12/4 = ~D ~%" (/ 12 4))
  (format t "Result of 16 / 8 = ~D~%" (/ 16 8))
  (format t "Result of 4 - x = 1 <=> x = 4 - 1 ~D~%" (- 4 1))
  (format t "Result of |-3| = ~D~%" (abs -3))
  ; Problem below can be solved in more straightforward way, but I wanted
					; to show how some more of the
					;reverse polish notation
  (format t "Result of (-8)*6 = ~D~%" (* (- 0 8) 6))
  (format t "Result of 15 / 9 = ~D~%" (/ 15 9))
  (format t "Result of 8 + x = 8 <=> x = 8 - 8 ~D~%" (- 8 8))
  (format t "Result of 5 ? 6 = -1, guessing substraction: ~S~%" (= -1 (- 5 6)))
  (format t "Result of 1 - 1/3 = ~D~%" (- 1 (/ 1 3)))
  (format t "Result of x = | -5 + 3 | = ~D~%" (abs (+ (- 0 5) 3)))
  (format t "That is all!~%"))

;; Solution to problem 1-2
(defun *1-2* ()
  (format t ""))