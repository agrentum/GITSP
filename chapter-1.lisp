;;;;; Here are solutions to chapter 1 of awesome book "Gentle Introduction
;;;;; to Symbolic Computation" [1] by even more awesome David S. Touretzky.
;;;;; who provided his work free of charge. Thank you.
;;;; To finish some rough edges I have supplemented myself with tutorial
;;;; http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial.html  [2]

;; Before anything else: I'm mostly a C programmer, far from real professional
;; coder. All input and critique is appreciated.

;;; Lets start the from beginning. Lines that are starting with ';' are comments,
;;; and they are not being read by interpreter/compiler as executable code.
;;; Comments are a way for coder to provide some explanations of his/hers code.
;;; General convention states (or so I was told) that the more important comment
;;; is, the more ';' are used.

;;; Another basic block is the concept of function. It does not really differ
;;; from mathematical definition. For example, f(x) = <expression in terms of x>
;;; is a function named f, that takes argument x and returns some value that
;;; depends on the value of argument.
;;; Syntax:
;;; (defun NAME-OF-YOUR-FUNCTION (argument)
;;; (expression))
;;; You can use more arguments, provided you will separate them with space ' '.

;;; Bit more about operation: basic syntax starts and ends with parenthesis,
;;; and operator is used before argument(s) it operates on.
;;; Example:
;;; 'Normal' addition: 3 + 2
;;; Lisp addition: (+ 3 2)
;;; If order of operation is important (for example: dividing two numbers)
;;; you read from left to right.
;;; Example:
;;; 'Normal' division: 2 / 3
;;; Lisp division: (/ 2 3)

;; Defined function that returns absolute value of passed number.
;; It uses conditional statement 'if'. The syntax is fairly straightforward:
;; (if (condition) (what is returned if condition is indeed true)
;; (what is returned if condition is false))
;; They are covered in chapter 4 of [1], but considering how simple of
;; a concept is absolute value, it can't really hurt.
;; Read further if you find it confusing. It is used once.
;; It is worth to note, that it is not the only possible way of implementing
;; abs function.
(defun CUSTOM-ABS (number)
  (if (> number 0) number
      (- 0 number)))

;;; Well, I have one more thing to add. If you want to load this file and
;;; work on it from under your Common Lisp interpreter, this is how you
;;; load it:
;;; [1]> (load "chapter1.lisp")
;;; My preffered method is to open file in emacs and then run SLIME.
;;; Here are some general pointers:
;;; http://common-lisp.net/project/slime/
;;; But, if you are Debian (and possibly Ubuntu or other ditributuion) user
;;; you can install emacs with slime extension even via Synaptic.


;; Solution to problem 1-1.
(defun 1-1 ()
  ; How does format t syntax work? As presented in chapter 8 of [2],
  ; ~D lets you to pass number to printed sequence. equivalent to %d
					; in C.
  ; ~% makes new line. Equivalent to \n
  ; It is important to note, that syntax of format t is presented in chapter 9[1]
  ; To be honest, I don't really understand the reason behind it, but that
  ; can't even count to evaluation of book in general.
  (format t "Result of 6 + 7 is ~D ~%" (+ 6 7))
  (format t "Result of x * 4 = 12 <=> x = 12/4 = ~D ~%" (/ 12 4))
  (format t "Result of 16 / 8 = ~D~%" (/ 16 8))
  (format t "Result of 4 - x = 1 <=> x = 4 - 1 ~D~%" (- 4 1))
  ; Now we need to use built-in function abs, it returns absolute value
					;of passed number.
  ; But there exist another way, so this problem will be printed twice.
  (format t "Result of |-3| = ~D~%" (abs -3))
  (format t "Now with my *custom-abs* function x = | -3 | = ~D~%" (*CUSTOM-ABS* -3))
  ; Problem below can be solved in more straightforward way, but I wanted
					; to show how some more of the
					; reverse polish notation
  ; You might ask, how it works. I'll try to explain:
					; (-8) * 6 = -42, agreed.
					; 0 - 8 = -8,
					; now, (* -8 6) can be transformed
					; to (* (- 0 8) 6). It works, because
					; operations are conducted from innermost
					; pair of parentheses.
  (format t "Result of (-8)*6 = ~D~%" (* (- 0 8) 6))
  (format t "Result of 15 / 9 = ~D~%" (/ 15 9))
  (format t "Result of 8 + x = 8 <=> x = 8 - 8 ~D~%" (- 8 8))
  (format t "Result of 5 ? 6 = -1, guessing subtraction: ~S~%" (= -1 (- 5 6)))
  (format t "Result of 1 - 1/3 = ~D~%" (- 1 (/ 1 3)))
  ; Like before, I could use -5 straight. Formula would be (abs (+ -5 3))
  (format t "Result of x = | -5 + 3 | = ~D~%" (abs (+ (- 0 5) 3)))
  (format t "That is all!~%"))

;; Solution to problem 1-2. There must be something more elegant.
;(defun 1-2 ()
;  (format t "1-2-3-GO ~S~%" (symbolp '1-2-3-GO)))

;; Solution to problem 1-3.
(defun 1-3 ()
  ; Most functions should be straightforward or explained with string in output.
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
(defun ADD1 (number)
  (+ number 1))

;; ADD2, two ways:
; Simple way, just add 2 to passed argument.
(defun ADD2 (number)
  (+ number 2))
; and equally good but with use of previously defined ADD1
; we make use of previously mentioned in problem 1-1 'convolution' of functions.
; How it works: ((number + 1) + 1) = number + 2.
; In Lisp syntax, without use of previously defined function it becomes:
; (+ 1 (+ 1 number))
(defun ADD2-WITH-ADD1 (number)
  (ADD1 (ADD1 number)))

;; TWOP, checks if argument is exactly 2.
;; Important: this works only for integer 2.
;; You can test it by passing 2.0 as argument.
(defun TWOP (number)
  (equal number 2))

;; Solution to problem 1-4.
; We just need to subtract 2 from number passed as argument. Remember
; that order is important in subtraction!
(defun SUB2 (number)
  (- number 2))

;; Solution to problem 1-5
; Logic of function:
; If number passed - 2 is equal to 0 ( number - 2 = 0 )
; then number is equal to 2 ( number = 2 )
(defun TWOP-WITH-ZEROP-AND-SUB2 (number)
  (zerop (*SUB2* number)))

;; Solution to problem 1-6
; Here we divide number by two.
; number / 2 <=> (/ number 2)
(defun HALF-1 (number)
  (/ number 2))
; Here we are multiplying number by 1/2.
; number * 1/2 <=> (* number (/ 1 2))
(defun HALF-2 (number)
  (* number (/ 1 2)))

;; Solution to problem 1-7
; We are just comparing number with digit 9. Since 9 is highest one-digit number
; any number higher then 9 is by default multidigit.
; What happens when you pass negative numbers? How can you solve this problem?
; Solution to this brain-teaser is somewhere in the end. Read all, and you will
; know how to do it.
(defun MULTI-DIGIT-P (number)
  (> number 9))

;; Solution to problem 1-8
; Quite straightforward, returns number with reversed sign. It was
; explained in definition od *1-1* function.
(defun 1-8 (number)
  (- 0 number))

;; Solution to problem 1-9
(defun TWOMOREP (number1 number2)
  (equal number1 (*ADD2* number2)))

;; Solution to problem 1-10
(defun TWOMOREP-1 (number1 number2)
  (equal (SUB2 number1) number2))

;; Solution to problem 1-11
(defun AVERAGE (number1 number2)
  (format t "Average of ~D and ~D is exactly ~D~%" number1 number2 (/ (+ number1 number2) 2)) 
  (/ (+ number1 number2) 2))

;; Solution to problem 1-12
(defun MORE-THEN-A-HALF (number1 number2)
  (format t "Is ~D > ~D? ~S~%" number1 (/ number2 2) (> number1 (/ number2 2)))
  (> number1 (/ number2 2)))

;; Solution to problem 1-13
; Function numberp tests if parameter is a number. Returns symbol T if parameter
; is indeed number and NIL otherwise.
; Function symbolp does the same, but tests whether passed parameter (in this case
; the RESULT of (numberp parameter), not parameter!) is symbol or not.
; Since both T and NIL are symbols, you will always get T as a result.
; In mathematics/logic that kind of structure is called tautology. Statement
; that is always true.
(defun 1-13 (parameter)
  (symbolp (numberp parameter)))

;; Solution to problem 1-14
(defun 1-14 ()
  (format t "(not NIL) = ~S~%" (not NIL))
  (format t "(not 12) = ~S~%" (not 12))
  ;; I think I have cheated a bit with this last problem. But it is only way
  ;; that did not result in constant errors.
  ;; I would like to hear some commets about it.
  (format t "(not not) = ~S~%" (not 'not)))

;;; Functions defined in 1.11 [1]

;; Definition of function *NOT-EQUAL*, that tests whether two passed arguments
;; are not exactly the same.
; Here, instead of me explaining the details, I want you to perform some tests.
; Pass anything. "Kirk" and "kirk", 2 and 2.0, anything. Try putting expresions
; as parameters.
; Try to explain the result of following test:
; (NOT-EQUAL (+ 1.0 1.0) (/ 4 2))
; Experiment with more complex types and operations. Try to work out why they
; are not equal. The explanation is given explicitly in this source code and
; in the book [1] itself.
(defun NOT-EQUAL (parameter1 parameter2)
  (not (equal parameter1 parameter2)))

;; Solution to problem 1-15
(defun NOT-ONEP (number1 number2)
  (format t "Is ~D = ~D + 1? ~S~%" number1 number2 (equal number1 (*ADD1* number2)))
  (equal number1 (*ADD1* number2)))

;; Solution to problem 1-16
; This one is really easy, but shows again that order of operations is important.
; All we do is comparison of number. If number < 0 <=> (< number 0) then T is
; returned, else NIL.
(defun NOT-PLUSP (number)
  (not (< number 0)))

;; Solution to problem 1-17
;; Here we need to negate result of asking if number is odd, we do that with
;; function oddp tests if number is odd. If it is, returns T (true). If it is not
;; returns NIL (false). Since natural number can be either odd or even (not odd)
;; Now, if number is odd, we negate that result and final returned value is NIL.
;; Therefore, only if number passed is even oddp returns NIL and negated NIL
;; is T.
(defun EVENP (number)
  (not (oddp number)))

;; Solution to problem 1-18
; (x + 1) + 1 == 0 <=> x == -2
(defun 1-18 (number)
  (format t "Is (1 + ~D) + 1 == 0? ~S~%" number (zerop (*ADD1* (*ADD1* number))))
  (zerop (*ADD1* (*ADD1* number))))

;; Solution to problem 1-19
; Experiment with arguments!
(defun 1-19 (argument)
  (not (not argument)))

;; Solution to problem 1-20
;; XOR is exclusive OR statement. Alternative (OR) is true if at least one element
;; of it is true. Exclusive OR (XOR) returns value T (true) ONLY if one and only
;; one element is true. This is, just like other problems, not the only way of
;; implementing solution.
(defun XOR (a b)
  (and (not (and a b)) (or a b)))

;;; Some ending remarks:
;;; You are now, assuming you worked through the book and not just my source code,
;;; familiar with some simple functions. You also know how to define functions
;;; on your own!
;;; If something seems problematic, or my supplementary remarks seemed insufficient,
;;; please send me a message. Just have in mind, that I'm just a beginner myself.

;;; What you actually should know by now:
;;; Arithmetic operations (add, subtract, multiply, divide) and how to use them
;;; together in forming more complex equations.
;;; Basic logic and operations on functors. You can also see that you can define
;;; more sophisticated operators.
;;; You had a glimpse of conditional statement.
;;; You know basics of formatting strings and displaying arguments.

;;; What about the problem with multidigit numbers? Simple! I believe you have
;;; recalled absolute value from problem 1-1. If by any reason your implementation
;;; does not work, here is my solution:
(defun BETTER-MULTI-DIGIT-P (number)
  (> (abs number) 9))

;;; There are quite a lot of 'mistakes' like that in this chapter. I hope you will
;;; try to find them and solve them by yoursef. If by any reason you have problems,
;;; pease, post it on original reddit thread:
;;; http://www.reddit.com/r/learnlisp/comments/2868si/common_lisp_gentle_introduction_to_symbolic/
;;; or send me an email / private message on reddit. (I go by username Agrentum).

;;; I hope you had fun and this code helped you in even the slightest possible way.
;;; Even if this help is in form of poor examples.