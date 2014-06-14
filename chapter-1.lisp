;;;; Here are solutions to chapter 1 of awesome book "Gentle Introduction
;;;; to Symbolic Computation" [1] by even more awesome David S. Touretzky.
;;;; who provided his work free of charge. Thank you.
;;; To finish some rough edges I have supplemented myself with tutorial
;;; http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial.html  [2]

;; Everything here was coded in Emacs 23 with SLIME.
;; +++++
;; Stallman protects.
;; +++++


;; Defined function that returns absolute value of passed number.
;; It uses conditional statement 'if'. The syntax is fairly straightforward:
;; (if (condition) (what is returned if condition is indeed true)
;; (what is returned if condition is false)
;; They are covered in chapter 4 of [1], but considering how simple of
;; a concept is absolute value, it can't really hurt.
;; Read further if you find it confusing. It is used once.
(defun *custom-abs* (a)
  (if (> a 0) a
      (- 0 a)))

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
  ; Now we need to use builtin function abs, it returns absolute value
					;of passed number.
  ; But there exist another way, so this problem will be printed twice.
  (format t "Result of |-3| = ~D~%" (abs -3))
  (format t "Now with my *custom-abs* function x = | -3 | = ~D~%" (*custom-abs* -3))
  ; Problem below can be solved in more straightforward way, but I wanted
					; to show how some more of the
					;reverse polish notation
  (format t "Result of (-8)*6 = ~D~%" (* (- 0 8) 6))
  (format t "Result of 15 / 9 = ~D~%" (/ 15 9))
  (format t "Result of 8 + x = 8 <=> x = 8 - 8 ~D~%" (- 8 8))
  (format t "Result of 5 ? 6 = -1, guessing substraction: ~S~%" (= -1 (- 5 6)))
  (format t "Result of 1 - 1/3 = ~D~%" (- 1 (/ 1 3)))
  ; Like before, I could use -5 straight. Formula would be (abs (+ -5 3))
  (format t "Result of x = | -5 + 3 | = ~D~%" (abs (+ (- 0 5) 3)))
  (format t "That is all!~%"))

;; Solution to problem 1-2. To be honest, I'm thinking how to do it without
;; passing string
(defun *1-2* ()
  (format t ""))

;; Solution to problem 1-3.
(defun *1-3* ()
  ; Most functions should be straightforwad or explained with string in output.
  ; In other situations I will explain what each function does.
  (format t "About outputs:~%T means True, NIL means False~%")
  (format t "Is 7 < 11? ~S~%" (< 7 11))
  (format t "Is 12 an odd number? ~S~%" (oddp 12))
  ; Function equal tests if compared arguments are equal. "Kirk" is equal to "Kirk",
  ; but not "kirk".
  (format t "Is Kirk Spocks equal? In terms of strings, at least. ~S~%" (equal "Kirk" "Spock"))
  (format t "Is 12 a number? ~S~%" (numberp 12))
  (format t "Is -4 < -3? ~S~%" (< -4 -3))
  (format t "Is 0 a zero? ~S~%" (zerop 0))
  (format t "That is all!~%"))


;;;Functions defined in 1.9 [1]

;; ADD1, adds 1 to passed argument.
(defun *ADD1* (number)
  (+ number 1))

;; ADD2, two ways:
; Simple
(defun *ADD2* (number)
  (+ number 2))
; With use of previously define ADD1
(defun *ADD2-WITH-ADD1* (number)
  (*ADD1* (*ADD1* number)))

;; TWOP, checks if argument is exactly 2.
;; Important: this works only for integer 2.
;; You can test it by passing 2.0 as argument.
(defun *TWOP* (number)
  (equal number 2))

;; Solution to problem 1-4.
(defun *SUB2* (number)
  (- number 2))

;; Solution to problem 1-5
; Logic of function:
; If number passed - 2 is equal to 0 ( number - 2 = 0 )
; then number is equal to 2 ( number = 2 )
(defun *TWOP-WITH-ZEROP-AND-SUB2* (number)
  (zerop (*SUB2* number)))

