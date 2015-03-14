# Calculate pi via Gauss' Arithmetic-Geometric Mean (AGM) Iteration

This repository contains Common Lisp code for calculating the value of
the mathematical constant "pi" using the Arithmetic-Geometric Mean (AGM)
iteration of Carl Friedrich Gauss.  This code has been used to calculate
pi to over 1 million decimal places, and the results are in perfect
agreement with references for the value of pi available on the Internet.

## Mathematical Background

The AGM is an extremely efficient method for calculating pi.  It
converges quadratically, which results in each successive iteration
effectively doubling the number of significant digits in agreement with
the limiting value.

(Note that this technique can be applied to efficiently computing many
other values. Calculating pi is simply one of the most interesting and
accessible uses of the AGM.)

## Requirements

Running this code requires the Common Lisp language.

CMU Common Lisp (CMUCL) <http://www.cons.org/cmucl/> is an excellent,
portable, open-source Common Lisp implementation that will execute this
code correctly.  (It should also work on derivatives of CMUCL.)

## Calculating pi

To calculate pi to the desired number of decimal digits, first enter
your Common Lisp environment. The code to calculate pi is in the file
`AGM.lisp`.  Ideally it should be compiled and loaded into your Common
Lisp environment.  The file provides various functions for performing
the AGM iteration and calculating pi.

The easiest way to invoke the AGM iteration to compute pi is to calling
the function `n-digits-of-pi` with the desired number of digits:

	(n-digits-of-pi (digits &optional (n 20)))

Here is example output using CMUCL on a Mac Pro computer: [Note that the
values computed below differ from the reference in the last 2 decimal
places as they are, of course, only approximations to an unending
trancendental number!]

	[spiritowl:/Users/psi/Lisp/AGM/14Mar15]
	% cmucl
	CMU Common Lisp Snapshot 2011-03 (20B Unicode), running on spiritowl.talksoft.com
	With core: /usr/local/lib/cmucl.2011.03/lib/cmucl/lib/lisp-sse2.core
	Dumped on: Tue, 2011-03-01 06:41:16-08:00 on gondor.local
	Send questions and bug reports to your local CMUCL maintainer, 
	or see <http://www.cons.org/cmucl/support.html>.
	Loaded subsystems:
	    Unicode 1.24 with Unicode version 5.2.0
	    Python 1.1, target Intel x86/sse2
	    CLOS based on Gerd's PCL 2010-03-19 15:19:03
	* (compile-file "AGM")
	
	; Python version 1.1, VM version Intel x86/sse2 on 2015-03-14 12:25:22.
	; Compiling: /Users/psi/Lisp/AGM/14Mar15/AGM.lisp 2012-10-10 15:26:46
	
	; Byte Compiling Top-Level Form: 
	; Byte Compiling Top-Level Form: 
	; Byte Compiling Top-Level Form: 
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
	; Byte Compiling Top-Level Form: 
	 
	; AGM.sse2f written.
	; Compilation finished in 0:00:00.
	
	#P"/Users/psi/Lisp/AGM/14Mar15/AGM.sse2f"
	NIL
	NIL
	* (load "AGM")
	
	; Loading #P"/Users/psi/Lisp/AGM/14Mar15/AGM.sse2f".
	T
	* (n-digits-of-pi 256)
	
	31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456478
	* (n-digits-of-pi 1024)
	
	31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989380952572010654858632775
	* 

## License

This code is published under the MIT License (i.e., the "Expat License".) Pleaes see the file `LICENSE`.

## Have fun!!!

## References

1. Eugene Salamin, "Computation of pi Using Arithmetic-Geometric Mean," _Mathematics of Computation_, volume 30, number 135, (July 1976), pages 565-570.

2. Jonathan M. Borwein and Peter B. Borwein, _Pi and the AGM: A Study in Analytic Number Theory and Computational Complexity_, (Wiley, New York, NY, 1987).

3. Dario Castellanos, "The Ubiquitous pi" (Part II), _Mathematics Magazine_, volume 61, number 3, (June 1988), pages 148-163.

4. Jonathan M. Borwein, Peter B. Borwein, David H. Bailey, "Ramanujan, Modular Equations, and Approximations to Pi or How to Compute One Billion Digits of Pi", _The American Mathematical Monthly_, volume 96, number 3, (March 1989), pages 201-219.
