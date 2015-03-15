;;;
;;; File:   AGM.lisp
;;; Author: ***PSI***
;;; Date:   Wed Oct 10 00:56:32 2012
;;;
;;; Description:
;;;    This file provides utilities for Gauss' Arithmetic-Geometric Mean (AGM) iteration and computing pi.
;;;
;;;      Initial Conditions:  a(0) >= b(0) > 0  //  Use:  a(0) = 1  ;  b(0) = 2^(-1/2)
;;;
;;;      Iteration:  a(n+1) = (1/2) * (a(n) + b(n))  ;  b(n+1) = (a(n) * b(n))^(1/2)  ;  c(n+1) = (1/2) * (a(n) - b(n))
;;;
;;;      AGM(a(0), b(0)) = lim (n ==> inf) a(n) = lim (n ==> inf) b(n)
;;;
;;;      pi = (4 * AGM(1, 2 ^ (-1/2)) ^ 2) / (1 - Sum(i=1 to inf) (2 ^ (i+1)) * (c(i)) ^ 2)
;;;
;;;    This program uses Lisp Bignums to represent (scaled) real numbers, so to
;;;    get the actual numerical value of pi, simply shift the decimal point back
;;;    to the left, so that it is just to the right of the first digit, i.e., 3.
;;;
;;;    Using a maximum number of iterations ("n") of 20 is sufficient for accurately
;;;    computing 1 million decimal digits of pi.
;;;
;;;    References:
;;;
;;;     [1] Eugene Salamin, "Computation of pi Using Arithmetic-Geometric Mean," _Mathematics of Computation_,
;;;            volume 30, number 135, (July 1976), pages 565-570.
;;;
;;;     [2] Jonathan M. Borwein and Peter B. Borwein, _Pi and the AGM: A Study in Analytic Number Theory
;;;            and Computational Complexity_, (Wiley, New York, NY, 1987).
;;;
;;;     [3] Dario Castellanos, "The Ubiquitous pi" (Part II), _Mathematics Magazine_,
;;;            volume 61, number 3, (June 1988), pages 148-163.
;;;
;;;     [4] Jonathan M. Borwein, Peter B. Borwein, David H. Bailey, "Ramanujan, Modular Equations, and Approximations to Pi
;;;            or How to Compute One Billion Digits of Pi", _The American Mathematical Monthly_,
;;;            volume 96, number 3, (March 1989), pages 201-219.
;;;
;;; Copyright (c) 2012-2015 Joseph J. Mankoski ***PSI***
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;

;;;
;;; Using CMUCL:
;;;
;;;   * (agm a0 b0 3)
;;;
;;;   (0.8472130847939792d0 0.847213084793979d0 1.1102230246251565d-16
;;;    (0.14644660940672627d0 0.14644660940672627d0 0.006328487669779614d0
;;;     1.1818088301362994d-5))
;;;
;;;   * (api)
;;;
;;;   3.141592653589794d0
;;;
;;;   * pi
;;;
;;;   3.141592653589793d0
;;;
;;;   * (- (api) pi)
;;;
;;;   8.881784197001252d-16
;;;
;;;   * (n-digits-of-pi 100)
;;;
;;;   31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170669
;;;

(defvar A0 1d0
  "Initial value of a(n).")

(defvar B0 (/ 1d0 (sqrt 2d0))
  "Initial value of b(n).")

(defvar *I-NUM-DIGITS* 100)

;;; Note:  17 iterations achieves the maximum precision (all but the last 3 digits agree with reference.)
;;(defvar *I-NUM-DIGITS* 320000)

;;(defvar *I-NUM-DIGITS* 1000000)

;;; Allow bigger bignums, if necessary.  (Note:  This is specific to CMUCL (and derivatives)!)
(setq *INTEXP-MAXIMUM-EXPONENT* (max *intexp-maximum-exponent* *i-num-digits*))

(defvar IA0 (expt 10 *i-num-digits*)
  "Initial value of ia(n).")

(defvar IB0 (round (/ (* ia0 ia0) (isqrt (* 2 ia0 ia0))))
  "Initial value of ib(n).")

(defun AGM (a0 b0 n)
  "Compute the Nth iteration of the AGM starting from A0 and B0."
  (do ((i 0 (1+ i))
       (an a0 (/ (+ an bn) 2d0))
       (bn b0 (sqrt (* an bn)))
       (cn (/ (- a0 b0) 2d0) (/ (- an bn) 2d0))
       (c nil (append c (list cn))))
      ((> i n) (list an bn (- an bn) c))))

(defun IAGM (a0 b0 n)
  "Compute the Nth iteration of the AGM starting from A0 and B0."
  (do ((i 0 (1+ i))
       (an a0 (round (/ (+ an bn) 2)))
       (bn b0 (isqrt (* an bn)))
       (cn (round (/ (- a0 b0) 2)) (round (/ (- an bn) 2)))
       (c nil (append c (list cn))))
      ((> i n) (list an bn (- an bn) c))))

(defun API (&optional (n 3))
  "Compute Pi using the AGM."
  (let* ((res (agm a0 b0 n))
         (agm (car res))
         (c (cdr (cadddr res))))
    (/ (* 4d0 (* agm agm))
       (- 1d0
          (do ((i 1 (1+ i))
               (ci (car c) (car c))
               (c (cdr c) (cdr c))
               (sum 0d0 (+ sum (* (expt 2d0 (+ i 1))
                                  (expt ci 2)))))
              ((> i n) sum))))))

(defun IPI (&optional (n 3))
  "Compute Pi using the AGM."
  (let* ((res (iagm ia0 ib0 n))
         (agm (car res))
         (c (cdr (cadddr res))))
    (values 
     (round (/ (* (* 4 ia0) (* agm agm))
               (- (* ia0 ia0)
                  (do ((i 1 (1+ i))
                       (ci (car c) (car c))
                       (c (cdr c) (cdr c))
                       (sum 0 (+ sum (* (expt 2 (+ i 1))
                                        (expt ci 2)))))
                      ((> i n) sum))))))))

(defun n-digits-of-pi (digits &optional (n 20))
  "Compute DIGITS decimal places of Pi with optional number of AGM iterations N."
  (setq *intexp-maximum-exponent* (max *intexp-maximum-exponent* digits))
  (setq ia0 (expt 10 digits))
  (setq ib0 (round (/ (* ia0 ia0) (isqrt (* 2 ia0 ia0)))))
  (ipi n))
