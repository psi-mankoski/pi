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
;;;    The number of iterations required for computing n decimal digits of pi
;;;    is the ceiling of log2(n), so 20 iterations is sufficient for accurately
;;;    computing 1 million decimal digits of pi.
;;;
;;;    This program uses Lisp bignums to represent (scaled) real numbers, so to
;;;    get the actual numerical value of pi, simply shift the decimal point back
;;;    to the left, so that it is just to the right of the first digit, i.e., 3.
;;;
;;;    References:
;;;
;;;     [1] Eugene Salamin, "Computation of pi Using Arithmetic-Geometric Mean," _Mathematics of Computation_,
;;;            volume 30, number 135, (July 1976), pages 565-570.
;;;
;;;     [2] Richard P. Brent, "Fast Multiple-Precision Evaluation of Elementary Functions," _Journal of the ACM_,
;;;            volume 23, issue 2, (April 1976), pages 242-251.
;;;
;;;     [3] Jonathan M. Borwein and Peter B. Borwein, _Pi and the AGM: A Study in Analytic Number Theory
;;;            and Computational Complexity_, (Wiley, New York, NY, 1987).
;;;
;;;     [4] Dario Castellanos, "The Ubiquitous pi" (Part II), _Mathematics Magazine_,
;;;            volume 61, number 3, (June 1988), pages 148-163.
;;;
;;;     [5] Jonathan M. Borwein, Peter B. Borwein, David H. Bailey, "Ramanujan, Modular Equations, and Approximations to Pi
;;;            or How to Compute One Billion Digits of Pi", _The American Mathematical Monthly_,
;;;            volume 96, number 3, (March 1989), pages 201-219.
;;;
;;; Copyright (c) 2012-2018 Joseph J. Mankoski ***PSI***
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
;;; Some examples using SBCL or CMUCL:
;;;
;;;   * (compile-file "AGM")
;;;
;;;   * (load "AGM")
;;;
;;;   * (n-digits-of-pi 100)
;;;
;;;   31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170669
;;;
;;;   * (n-digits-of-pi n (ceiling (log n 2)) t)
;;;
;;;     ...
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

#+SBCL
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :sb-gmp))
#+SBCL
(eval-when (:compile-toplevel :execute :load-toplevel)
  (format t "Installing GMP functions.~&")
  (require :sb-gmp)
  (sb-gmp:install-gmp-funs))

(defvar A0 1d0
  "Initial value of a(n).")

(defvar B0 (/ 1d0 (sqrt 2d0))
  "Initial value of b(n).")

(defvar *I-NUM-DIGITS* 100)

(defvar *LAST-N-DIGITS* 1000)

;;; Allow bigger bignums, if necessary.  [Note:  This is specific to CMUCL.]
#+CMU (setq *INTEXP-MAXIMUM-EXPONENT* (max *intexp-maximum-exponent* *i-num-digits*))

(defvar *LOG-GC-ROOM* nil
  "Log verbose information about Garbage Collection.")

(defvar IA0 (expt 10 *i-num-digits*)
  "Initial value of ia(n).")

(defvar IA0^2 (expt 10 (* 2 *i-num-digits*))
  "Initial value of ia(n).")

(defvar IB0 (round (/ ia0^2 (isqrt (* 2 ia0^2))))
  "Initial value of ib(n).")

(defun FORMAT-TIME (timestamp)
  "Render and return unversal time TIMESTAMP as a string."
  #+SBCL (sb-int:format-universal-time nil timestamp)
  #+CMU (format-universal-time nil timestamp))

(defun LOG-ITERATION (n)
  "Log the Nth iteration."
  (let ((timestamp (get-universal-time)))
    (format t "~&Iteration: ~D -- ~A (~D)~&" n (format-time timestamp) timestamp)))

(defun DO-GC (&optional (full nil))
  "Clean up time."
  (when *Log-GC-Room*
    (format t "~2&Room before GC:~2&")
    (room t))
  #+CMU (setq *GC-Verbose* *Log-GC-Room*)
  (gc :full full)
  (when *Log-GC-Room*
    (format t "~2&Room after GC:~2&")
    (room t)))

(defun AGM (a0 b0 n)
  "Compute the Nth iteration of the AGM starting from A0 and B0."
  (do ((i 0 (1+ i))
       (an a0 (/ (+ an bn) 2d0))
       (bn b0 (sqrt (* an bn)))
       (cn (/ (- a0 b0) 2d0) (/ (- an bn) 2d0))
       (c nil (append c (list cn))))
      ((> i n) (list an bn (- an bn) c))))

(defun IAGM (a0 b0 n &optional (verbose nil))
  "Compute the Nth iteration of the AGM starting from A0 and B0."
  (do ((i 0 (1+ i))
       (an a0 (round (/ (+ an bn) 2)))
       (bn b0 (isqrt (* an bn)))
       (cn (round (/ (- a0 b0) 2)) (round (/ (- an bn) 2)))
       (2^i+1 2 (* 2^i+1 2))
       (sum ia0^2))
      ((>= i n) (list an bn (- an bn) sum))
      (if (> i 0)
	(decf sum (* 2^i+1 (expt cn 2))))
      (if verbose
	  (log-iteration i))
      (do-gc t)))

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

(defun IPI (&optional (n 3) (verbose nil))
  "Compute Pi using the AGM with optional number of AGM iterations N and Boolean verbosity."
  (let* ((res (iagm ia0 ib0 n verbose))
         (agm (first res))
         (sum (fourth res)))
    (values
     (round (/ (* (* 4 ia0) (* agm agm)) sum)))))

(defun n-digits-of-pi (digits &optional (n 20) (verbose nil))
  "Compute DIGITS decimal places of Pi with optional number of AGM iterations N and Boolean verbosity."
  (if verbose
      (let ((timestamp (get-universal-time)))
        (format t "~&Starting up -- ~A (~D)~&" (format-time timestamp) timestamp)))
#+CMU (setq *intexp-maximum-exponent* (max *intexp-maximum-exponent* digits))
  (setq ia0 (expt 10 digits))
  (setq ia0^2 (expt 10 (* 2 digits))) ; (* ia0 ia0)
  (setq ib0 (round (/ ia0^2 (isqrt (* 2 ia0^2)))))
  (let ((api (ipi n verbose)))
    (if verbose
        (let ((timestamp (get-universal-time)))
          (format t "~&Done! -- ~A (~D)~&" (format-time timestamp) timestamp)))
    api))

(defun write-n-digits-of-pi (digits &optional (n 20) (verbose nil) (filename "api"))
  "Compute and write DIGITS decimal places of Pi with an optional number of AGM iterations N and Boolean verbosity."
  (let ((api (time (n-digits-of-pi digits n verbose))))
    (format t "~&Last ~D digits: ~D~&" *Last-N-Digits* (mod api (expt 10 *Last-N-Digits*)))
    (time
     (with-open-file (outfile filename :direction :output)
       (format outfile "~D" api)))
    api))

(defun write-number (n filename &optional (base 10.))
  "Write a number N to FILENAME in ASCII format in the given BASE (defaulting to decimal.)."
  (format t "~&Last ~D hex digits: ~X~&" *Last-N-Digits* (mod n (expt 16 *Last-N-Digits*)))
  (time
   (with-open-file (outfile filename :direction :output)
     (write n :base base :stream outfile)))
  t)
