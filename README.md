# Calculate pi via Gauss' Arithmetic-Geometric Mean (AGM) Iteration

This repository contains Common Lisp code for calculating the value of
the mathematical constant "pi" using the
[Arithmetic-Geometric Mean (AGM)](http://en.wikipedia.org/wiki/Arithmetic-geometric_mean "Wikipedia AGM article")
iteration of Carl Friedrich Gauss and Adrien-Marie Legendre. It is also
known as the
[Gauss-Legendre Algorithm](http://en.wikipedia.org/wiki/Gauss-Legendre_algorithm "Wikipedia Gauss-Legendre Algorithm article").

Salamin [[1]](#ref1 "reference 1") and Brent [[2]](#ref2 "reference 2")
both independently published articles proposing to use this method to
calculate pi in 1976. This algorithm (and its descendants) [[3, p. 341]](#ref3 "reference 3")
has been used to calculate pi to
[trillions of decimal places](http://en.wikipedia.org/wiki/Chronology_of_computation_of_%CF%80 "Wikipedia pi records article"). 

This particular Common Lisp implementation of the AGM algorithm has been
used to calculate pi to over 1 million decimal places, and the results
are in perfect agreement with references for the values for those digits
of pi available on the Internet.

## Mathematical Background

The AGM is an extremely efficient method for calculating pi. It
converges quadratically, which results in each successive iteration
effectively doubling the number of significant digits in agreement with
the limiting value.

(Note that this technique can also be applied to efficiently computing
many other functions and constant values. Calculating pi is simply one
of the most interesting and accessible uses of the AGM.)

### Initial Conditions

The initial conditions for the AGM iteration are:

`a(0) >= b(0) > 0`

A good choice is:

`a(0) = 1`

and

`b(0) = 1 / sqrt(2)`

In the program, I actually use starting values of:

`a(0) = 10 ^ <NumDesiredDigitsOfPi>`

and

`b(0) = (a(0) ^ 2) / sqrt(2 * (a(0) ^ 2))`

The reason for this is that Lisp bignums (i.e., infinite-precision
integers) are used to represent the entire sequence of decimal digits of
pi, rather than a floating point representation. This way the program
is quite short, since it builds upon the bignum algorithms already
provided by Lisp.

### AGM Iteration

The iteration is:

`a(n+1) = (a(n) + b(n)) / 2`

`b(n+1) = sqrt(a(n) * b(n))`

`c(n+1) = (a(n) - b(n)) / 2`

The Arithmetic-Geometric Mean is defined as:

`AGM(a(0), b(0)) = lim (n ==> inf) a(n) = lim (n ==> inf) b(n)`

In other words, in the limit as `n` tends toward infinity, `a(n)` will
equal `b(n)`, and that value is the AGM of the initial conditions,
`a(0)` and `b(0)`.

The number of iterations required for convergence (i.e, the maximum
value of `n`) depends upon the number of desired digits. 20 iterations
is sufficient for computing 1 million digits of pi.

### Computing the Result - pi

Finally, the value of pi is calculated as follows:

`pi = (4 * AGM(1, 1 / sqrt(2)) ^ 2) / (1 - Sum(i=1 to inf) (2 ^ (i+1)) * (c(i)) ^ 2)`

and approximated in the program by:

`pi = (4 * AGM(a(0), b(0)) ^ 2) / (1 - Sum(i=1 to n) (2 ^ (i+1)) * (c(i)) ^ 2)`

As mentioned above, `10 ^ <NumDesiredDigitsOfPi>` is used in the place
of `1` (i.e., `a(0)`), so consider that the output of this program is
actually pi with the decimal point shifted all the way to the right. To
get the actual numerical value of pi, simply shift the decimal point back
to the left, so that it is just to the right of the first digit, i.e., `3`.

## Requirements

Running this code requires the Common Lisp language.

Steel Bank Common Lisp (SBCL) <http://sbcl.org> is an excellent,
high-performance, multi-platform, open-source Common Lisp implementation
that will execute this code correctly. (This code will also run on CMU
Common Lisp (CMUCL) <http://www.cons.org/cmucl/>. SBCL is in fact a
derivative of CMUCL ("Steel" <==> Carnegie, "Bank" <==> Mellon.)

## Calculating pi

To calculate pi to the desired number of decimal digits, first enter
your Common Lisp environment. The code to calculate pi is in the file
`AGM.lisp`. Ideally it should first be compiled and then loaded into
your Common Lisp environment. The file provides various functions for
performing the AGM iteration and calculating pi.

The easiest way to invoke the AGM iteration to compute pi is to calling
the function `n-digits-of-pi` with the desired number of digits:

	(n-digits-of-pi (digits &optional (n 20) (verbose nil)))

For simplicity of implementation, this program uses Lisp's bignums for
unlimited precision integer arithmetic instead of floating point
numbers. To reach billions of decimal places within reasonable CPU time
and memory space constraints, it is advisable to use FFT-based
multiplication.  The GNU Multiple Precision (GMP) Arithmetic Library
<http://gmplib.org/> transparently plugs into SBCL's bignum operations.

### 2 Giga Decimal Digits of pi

Here is an example of calculating pi to greater than 2 billion decimal
places using SBCL and GMP. The computer is an HP Omen X 900-140VT
desktop with an Intel Core i7-7700K CPU @ 4.2 GHz and 32 GB RAM running
Fedora 27 Linux.

	[v4.valis.com:/home/psi/sw/github.com/pi]
	% sbcl --dynamic-space-size 64Gb
	This is SBCL 1.4.2-1.fc27, an implementation of ANSI Common Lisp.
	More information about SBCL is available at <http://www.sbcl.org/>.

	SBCL is free software, provided as is, with absolutely no warranty.
	It is mostly in the public domain; some portions are provided under
	BSD-style licenses.  See the CREDITS and COPYING files in the
	distribution for more information.
	* (compile-file "AGM")

	; compiling file "/home/psi/sw/github.com/pi/AGM.lisp" (written 13 MAR 2018 04:51:15 AM):
	; compiling (REQUIRE :SB-GMP)
	; compiling (FORMAT T ...)Installing GMP functions.
	; compiling (REQUIRE :SB-GMP)
	; compiling (SB-GMP:INSTALL-GMP-FUNS)
	; compiling (DEFVAR A0 ...)
	; compiling (DEFVAR B0 ...)
	; compiling (DEFVAR *I-NUM-DIGITS* ...)
	; compiling (DEFVAR *LAST-N-DIGITS* ...)
	; compiling (DEFVAR *LOG-GC-ROOM* ...)
	; compiling (DEFVAR IA0 ...)
	; compiling (DEFVAR IA0^2 ...)
	; compiling (DEFVAR IB0 ...)
	; compiling (DEFUN FORMAT-TIME ...)
	; compiling (DEFUN LOG-ITERATION ...)
	; compiling (DEFUN DO-GC ...)
	; compiling (DEFUN AGM ...)
	; compiling (DEFUN IAGM ...)
	; compiling (DEFUN API ...)
	; compiling (DEFUN IPI ...)
	; compiling (DEFUN N-DIGITS-OF-PI ...)
	; compiling (DEFUN WRITE-N-DIGITS-OF-PI ...)

	; /home/psi/sw/github.com/pi/AGM.fasl written
	; compilation finished in 0:00:00.027
	#P"/home/psi/sw/github.com/pi/AGM.fasl"
	NIL
	NIL
	* (load "AGM")
	Installing GMP functions.
	T
	* (defvar n (expt 2 31))

	N
	* (defvar i (ceiling (log n 2)))

	I
	* n

	2147483648
	* i

	31
	* (defvar api 3)

	API
	* (time (progn (setq api (write-n-digits-of-pi n i t "Pi.2G.GMP.v4.1.4.2-1.fc27.13Mar18")) t))

	Starting up -- Tuesday, March 13, 2018 05:14:23 AM PDT (3729932063)
	Iteration: 0 -- Tuesday, March 13, 2018 06:28:41 AM PDT (3729936521)
	Iteration: 1 -- Tuesday, March 13, 2018 06:33:31 AM PDT (3729936811)
	Iteration: 2 -- Tuesday, March 13, 2018 06:38:20 AM PDT (3729937100)
	Iteration: 3 -- Tuesday, March 13, 2018 06:43:09 AM PDT (3729937389)
	Iteration: 4 -- Tuesday, March 13, 2018 06:47:58 AM PDT (3729937678)
	Iteration: 5 -- Tuesday, March 13, 2018 06:52:47 AM PDT (3729937967)
	Iteration: 6 -- Tuesday, March 13, 2018 06:57:36 AM PDT (3729938256)
	Iteration: 7 -- Tuesday, March 13, 2018 07:02:25 AM PDT (3729938545)
	Iteration: 8 -- Tuesday, March 13, 2018 07:07:14 AM PDT (3729938834)
	Iteration: 9 -- Tuesday, March 13, 2018 07:12:03 AM PDT (3729939123)
	Iteration: 10 -- Tuesday, March 13, 2018 07:16:53 AM PDT (3729939413)
	Iteration: 11 -- Tuesday, March 13, 2018 07:21:42 AM PDT (3729939702)
	Iteration: 12 -- Tuesday, March 13, 2018 07:26:31 AM PDT (3729939991)
	Iteration: 13 -- Tuesday, March 13, 2018 07:31:21 AM PDT (3729940281)
	Iteration: 14 -- Tuesday, March 13, 2018 07:36:10 AM PDT (3729940570)
	Iteration: 15 -- Tuesday, March 13, 2018 07:40:59 AM PDT (3729940859)
	Iteration: 16 -- Tuesday, March 13, 2018 07:45:48 AM PDT (3729941148)
	Iteration: 17 -- Tuesday, March 13, 2018 07:50:37 AM PDT (3729941437)
	Iteration: 18 -- Tuesday, March 13, 2018 07:55:26 AM PDT (3729941726)
	Iteration: 19 -- Tuesday, March 13, 2018 08:00:16 AM PDT (3729942016)
	Iteration: 20 -- Tuesday, March 13, 2018 08:05:04 AM PDT (3729942304)
	Iteration: 21 -- Tuesday, March 13, 2018 08:09:53 AM PDT (3729942593)
	Iteration: 22 -- Tuesday, March 13, 2018 08:14:42 AM PDT (3729942882)
	Iteration: 23 -- Tuesday, March 13, 2018 08:19:30 AM PDT (3729943170)
	Iteration: 24 -- Tuesday, March 13, 2018 08:24:19 AM PDT (3729943459)
	Iteration: 25 -- Tuesday, March 13, 2018 08:29:09 AM PDT (3729943749)
	Iteration: 26 -- Tuesday, March 13, 2018 08:33:58 AM PDT (3729944038)
	Iteration: 27 -- Tuesday, March 13, 2018 08:38:45 AM PDT (3729944325)
	Iteration: 28 -- Tuesday, March 13, 2018 08:43:32 AM PDT (3729944612)
	Iteration: 29 -- Tuesday, March 13, 2018 08:48:14 AM PDT (3729944894)
	Iteration: 30 -- Tuesday, March 13, 2018 08:52:47 AM PDT (3729945167)
	Done! -- Tuesday, March 13, 2018 11:36:04 AM PDT (3729954964)
	Evaluation took:
	  22900.452 seconds of real time
	  22576.415954 seconds of total run time (22074.843200 user, 501.572754 system)
	  [ Run times consist of 8.507 seconds GC time, and 22567.909 seconds non-GC time. ]
	  98.59% CPU
	  96,182,040,596,429 processor cycles
	  5,613 page faults
	  492,957,260,048 bytes consed

	Last 1000 digits: 6128707026157903694208731094423927151484727659291015323586535496516098827178407885688643568025181090091530458419892577486892186560755618696108787719283846369922146190927112971885922784316457070195492971052705843648446073166071523302817989435029562098080856244140348601980681522403014411225939628325397723846695577390411526185829432696933580512778382430216594959784787209780298790435175641407731492576964230577791151385265884938913307130848637305380640159175861923453943406911863006823994986834301187525392831225877735117707188467394301950440652082600221127090443290885861597716695218025967387969247149572303646478452345633177858789671444424956331771536675271541326122415912451639126443695034189180852090439726109561340993744270575843386797985880803984065390547399268509824206264882331104212317866073185185616880547848252456756952818795517001764796226381868552208049914757611820073576843021064567048710589617237926447442382673655600511704783223650544693156176377410057169385546980184129084150913685047
	Evaluation took:
	  1042.057 seconds of real time
	  1030.160749 seconds of total run time (1012.204770 user, 17.955979 system)
	  [ Run times consist of 0.387 seconds GC time, and 1029.774 seconds non-GC time. ]
	  98.86% CPU
	  4,376,646,281,829 processor cycles
	  18 page faults
	  60,577,879,792 bytes consed

	Evaluation took:
	  23948.350 seconds of real time
	  23612.072933 seconds of total run time (23091.541596 user, 520.531337 system)
	  [ Run times consist of 9.464 seconds GC time, and 23602.609 seconds non-GC time. ]
	  98.60% CPU
	  100,583,218,195,302 processor cycles
	  5,704 page faults
	  555,318,619,200 bytes consed

	T
	* (quit)
	[v4.valis.com:/home/psi/sw/github.com/pi]
	%

Here is an example run computing 1,024 decimal places of pi using CMUCL
on a MacBook Pro laptop computer (Intel Core i7 @ 2.3 GHz with 8 GB
RAM): [Note that the values computed below differ from the reference in
the last 2 decimal places as they are, of course, only approximations to
an unending transcendental number.]

	[silverowl:/Users/psi/sw/github.com/pi]
	% cmucl
	CMU Common Lisp Snapshot 2011-03 (20B Unicode), running on SilverOwl.local
	With core: /usr/local/lib/cmucl.2011.03/lib/cmucl/lib/lisp-sse2.core
	Dumped on: Tue, 2011-03-01 06:41:16-08:00 on gondor.local
	Send questions and bug reports to your local CMUCL maintainer, 
	or see <http://www.cons.org/cmucl/support.html>.
	Loaded subsystems:
	    Unicode 1.24 with Unicode version 5.2.0
	    Python 1.1, target Intel x86/sse2
	    CLOS based on Gerd's PCL 2010-03-19 15:19:03
	* (compile-file "AGM")

	; Python version 1.1, VM version Intel x86/sse2 on 2018-03-13 04:55:00.
	; Compiling: /Users/psi/sw/github.com/pi/AGM.lisp 2018-03-13 04:51:15

	; Byte Compiling Top-Level Form:
	; Byte Compiling Top-Level Form:
	; Byte Compiling Top-Level Form:
	; Byte Compiling Top-Level Form:
	; Converted FORMAT-TIME.
	; Compiling DEFUN FORMAT-TIME:
	; Converted LOG-ITERATION.
	; Compiling DEFUN LOG-ITERATION:
	; Byte Compiling Top-Level Form:
	; Converted DO-GC.
	; Compiling DEFUN DO-GC:
	; Converted AGM.
	; Compiling DEFUN AGM:
	; Converted IAGM.
	; Compiling DEFUN IAGM:
	; Converted API.
	; Compiling DEFUN API:
	; Converted IPI.
	; Compiling DEFUN IPI:
	; Converted N-DIGITS-OF-PI.
	; Compiling DEFUN N-DIGITS-OF-PI:
	; Converted WRITE-N-DIGITS-OF-PI.
	; Compiling DEFUN WRITE-N-DIGITS-OF-PI:
	; Byte Compiling Top-Level Form:

	; AGM.sse2f written.
	; Compilation finished in 0:00:00.

	#P"/Users/psi/sw/github.com/pi/AGM.sse2f"
	NIL
	NIL
	* (load "AGM")

	; Loading #P"/Users/psi/sw/github.com/pi/AGM.sse2f".
	T
	* (n-digits-of-pi 1024)

	31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989380952572010654858632775

	* (quit)
	[SilverOwl:/Users/psi/sw/github.com/pi]
	%

## License

This code is published under the MIT License (i.e., the "Expat License".) Please see the file `LICENSE`.

## Have Fun!!!

## References

1. <a name="ref1"></a>Eugene Salamin, "Computation of pi Using Arithmetic-Geometric Mean," _Mathematics of Computation_, volume 30, number 135, (July 1976), pages 565-570.

2. <a name="ref2"></a>Richard P. Brent, "Fast Multiple-Precision Evaluation of Elementary Functions," _Journal of the ACM_, volume 23, issue 2, (April 1976), pages 242-251.

3. <a name="ref3"></a>Jonathan M. Borwein and Peter B. Borwein, _Pi and the AGM: A Study in Analytic Number Theory and Computational Complexity_, (Wiley, New York, NY, 1987).

4. <a name="ref4"></a>Dario Castellanos, "The Ubiquitous pi" (Part II), _Mathematics Magazine_, volume 61, number 3, (June 1988), pages 148-163.

5. <a name="ref5"></a>Jonathan M. Borwein, Peter B. Borwein, David H. Bailey, "Ramanujan, Modular Equations, and Approximations to Pi or How to Compute One Billion Digits of Pi", _The American Mathematical Monthly_, volume 96, number 3, (March 1989), pages 201-219.
