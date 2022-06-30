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
used to calculate pi to over 4 billion decimal places, and the results
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

### 4 Giga Decimal Digits of pi

Here is an example of calculating pi to greater than 4 billion decimal
places using SBCL and GMP. The computer is an HP Omen Obelisk 875-0014
desktop with an Intel Core i7-8700 CPU @ 3.20 GHz processor (max. turbo
boost frequency: 4.60 GHz; 6 cores / 12 threads) and 32 GB RAM running
Fedora 29 Linux:

	[v5.valis.com:/home/psi/sw/github.com/pi]
	% sbcl --dynamic-space-size 200Gb
	This is SBCL 1.4.6-2.fc29, an implementation of ANSI Common Lisp.
	More information about SBCL is available at <http://www.sbcl.org/>.

	SBCL is free software, provided as is, with absolutely no warranty.
	It is mostly in the public domain; some portions are provided under
	BSD-style licenses.  See the CREDITS and COPYING files in the
	distribution for more information.
	* (compile-file "AGM")

	; compiling file "/home/psi/sw/github.com/pi/AGM.lisp" (written 13 MAR 2019 12:03:32 PM):
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
	; compiling (DEFUN WRITE-NUMBER ...)

	; /home/psi/sw/github.com/pi/AGM.fasl written
	; compilation finished in 0:00:00.078
	#P"/home/psi/sw/github.com/pi/AGM.fasl"
	NIL
	NIL
	* (load "AGM")
	Installing GMP functions.
	T
	* (defvar n (expt 2 32))

	N
	* (defvar i (ceiling (log n 2)))

	I
	* i

	32
	* n

	4294967296
	* (defvar api 3)

	API
	* (time (progn (setq api (write-n-digits-of-pi n i t "Pi.4G.GMP.v5.1.4.6-2.fc29.13Mar19")) t))

	Starting up -- Wednesday, March 13, 2019 12:09:31 PM PDT (3761492971)
	Iteration: 0 -- Wednesday, March 13, 2019 02:38:20 PM PDT (3761501900)
	Iteration: 1 -- Wednesday, March 13, 2019 02:47:39 PM PDT (3761502459)
	Iteration: 2 -- Wednesday, March 13, 2019 02:57:44 PM PDT (3761503064)
	Iteration: 3 -- Wednesday, March 13, 2019 03:07:07 PM PDT (3761503627)
	Iteration: 4 -- Wednesday, March 13, 2019 03:16:10 PM PDT (3761504170)
	Iteration: 5 -- Wednesday, March 13, 2019 03:25:16 PM PDT (3761504716)
	Iteration: 6 -- Wednesday, March 13, 2019 03:34:22 PM PDT (3761505262)
	Iteration: 7 -- Wednesday, March 13, 2019 03:43:44 PM PDT (3761505824)
	Iteration: 8 -- Wednesday, March 13, 2019 03:53:25 PM PDT (3761506405)
	Iteration: 9 -- Wednesday, March 13, 2019 04:02:29 PM PDT (3761506949)
	Iteration: 10 -- Wednesday, March 13, 2019 04:11:46 PM PDT (3761507506)
	Iteration: 11 -- Wednesday, March 13, 2019 04:20:51 PM PDT (3761508051)
	Iteration: 12 -- Wednesday, March 13, 2019 04:30:06 PM PDT (3761508606)
	Iteration: 13 -- Wednesday, March 13, 2019 04:39:12 PM PDT (3761509152)
	Iteration: 14 -- Wednesday, March 13, 2019 04:48:18 PM PDT (3761509698)
	Iteration: 15 -- Wednesday, March 13, 2019 04:57:25 PM PDT (3761510245)
	Iteration: 16 -- Wednesday, March 13, 2019 05:06:35 PM PDT (3761510795)
	Iteration: 17 -- Wednesday, March 13, 2019 05:15:41 PM PDT (3761511341)
	Iteration: 18 -- Wednesday, March 13, 2019 05:24:47 PM PDT (3761511887)
	Iteration: 19 -- Wednesday, March 13, 2019 05:34:05 PM PDT (3761512445)
	Iteration: 20 -- Wednesday, March 13, 2019 05:43:08 PM PDT (3761512988)
	Iteration: 21 -- Wednesday, March 13, 2019 05:52:13 PM PDT (3761513533)
	Iteration: 22 -- Wednesday, March 13, 2019 06:01:18 PM PDT (3761514078)
	Iteration: 23 -- Wednesday, March 13, 2019 06:10:22 PM PDT (3761514622)
	Iteration: 24 -- Wednesday, March 13, 2019 06:19:28 PM PDT (3761515168)
	Iteration: 25 -- Wednesday, March 13, 2019 06:28:34 PM PDT (3761515714)
	Iteration: 26 -- Wednesday, March 13, 2019 06:37:43 PM PDT (3761516263)
	Iteration: 27 -- Wednesday, March 13, 2019 06:46:52 PM PDT (3761516812)
	Iteration: 28 -- Wednesday, March 13, 2019 06:55:53 PM PDT (3761517353)
	Iteration: 29 -- Wednesday, March 13, 2019 07:04:55 PM PDT (3761517895)
	Iteration: 30 -- Wednesday, March 13, 2019 07:13:47 PM PDT (3761518427)
	Iteration: 31 -- Wednesday, March 13, 2019 07:22:24 PM PDT (3761518944)
	Done! -- Thursday, March 14, 2019 12:41:04 AM PDT (3761538064)
	Evaluation took:
	  45093.154 seconds of real time
	  44731.558009 seconds of total run time (43570.747162 user, 1160.810847 system)
	  [ Run times consist of 20.055 seconds GC time, and 44711.504 seconds non-GC time. ]
	  99.20% CPU
	  143,937,723,655,703 processor cycles
	  1,391,460 page faults
	  991,777,058,272 bytes consed

	Last 1000 digits: 3413657859666896267821877044446158661445456612159941518254445963559201522341365127456887060882957489968358233131890658207279642485508525070184914608491211125036480205284190848991702134930428895203012950823781460486606178596751430718031721969184922573793749862252280816955064385594066652888017199497528117993669892856213609441085590936023678406836646108636259565319453421722926623323119208760323213437985636317261585046370726456255506553357866774498614095208650951958054986235582473898875741245401010901630657861825676608660450022044628298621138269606178787747110896696785254404959874260776266872524370672125477742380670740543693781591839873600478582959954778848457767260510380338430307408756194119972754253482862025991572003117846657075674120822415937560343046459136328647689611078014979653744733588651771897986360657267444446749976063496863897229086373751472665965036432501622069780721382670293029726472848638424612562001105967875069167292166652527817388293343234635525725113327148905270472422262233
	Evaluation took:
	  2067.497 seconds of real time
	  2061.938434 seconds of total run time (2020.426410 user, 41.512024 system)
	  [ Run times consist of 0.778 seconds GC time, and 2061.161 seconds non-GC time. ]
	  99.73% CPU
	  6,599,466,043,592 processor cycles
	  945 page faults
	  124,723,175,776 bytes consed

	Evaluation took:
	  47171.840 seconds of real time
	  46804.602278 seconds of total run time (45599.442467 user, 1205.159811 system)
	  [ Run times consist of 22.547 seconds GC time, and 46782.056 seconds non-GC time. ]
	  99.22% CPU
	  150,572,910,935,924 processor cycles
	  1,392,586 page faults
	  1,120,067,159,968 bytes consed

	T
	* (quit)
	[v5.valis.com:/home/psi/sw/github.com/pi]
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
