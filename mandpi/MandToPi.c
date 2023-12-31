/*
	MandToPi.c - Copyright ? 1993 CygnusSoftware.

	Cygnus Software
	33 University Square, #199
	Madison, WI, 53715
	USA
	CygnusSoft@cup.portal.com

	This  program  is  supplied  along  with  Mand2000, the tryware fractal
exploration  program  for the Amiga.  Mand2000 features multi-pass drawing,
animated  zooming,  joystick driving through the Mandelbrot set, full ARexx
support,  1000+  bit  accuracy,  multiple  fractal  equations,  etc.  68000
required.   MandToPi  and  Mand2000  may  not  be  distributed,  but may be
purchased  from  Cygnus Software for $34.95.  Demo versions of Mand2000 are
available on many networks.

	MandToPi is based on a discovery made by Dave Boll in 1991.

	Boll's rather surprising discovery was that that ubiquitous number, PI,
could  even  be  found in the Mandelbrot set.  Where?  Well, if you look at
the  two  largest bulbs of the Mandelbrot set (the head and body) you might
notice  that  these bulbs are seperated by two spikes or arrows.  One spike
on  either  side.   And you may wonder how close do those two spikes get to
each  other?   How  thin  is  the  `neck'  of  the  Mandelbrot?   Since the
Mandelbrot  set  is  known  to be connected (all those black areas are tied
together  by  microscopic black filaments) the spikes can't actually touch.
But  if  they don't touch, how close do they get?  As it turns out they get
infinitely  close  -  that  is,  there  is just a single mathematical point
separating the two spikes.

	The  `real' coordinate of those spikes (also known as the x coordinate)
is -0.75, a nice round number.  What this program does is it tries a series
of  points,  all  with  real  coordinates of -0.75, but each one a tiny bit
closer  to  the  real  axis.   For  each  point it calculates the number of
iterations  it  takes  for  that point to escape.  All of these points will
escape,  because  all  of the points are on the spikes and therefore not in
the Mandelbrot set.

	What  Boll's  discovery  was is that there is a very strange pattern to
the  number  of iterations.  Each point we try is one tenth the distance to
the  real  axis,  and each number of iterations that the program prints out
looks more and more like PI!  In fact, the general rule is that if you take
the  point (-.75, E), where E is any small number (less than one), then the
number  of  iterations until that point escapes,, times E, is approximately
equal  to  PI.  The smaller E gets, the closer the approximation.  In fact,
the  error  is  always less than E, so in theory you can get an arbitrarily
accurate  value  for  PI,  although  to get more than seven or eight digits
takes a phenomenal amount of computing time.

	Here's  the results from a ten pass run of this program.  This run took
five  hours  on  an  '040  - not surprising as over thirty billion floating
point operations were involved.

    E is 1.00000000000, num iterations is           3, PI is ~3.00000000000
    E is 0.10000000000, num iterations is          33, PI is ~3.30000000000
    E is 0.01000000000, num iterations is         315, PI is ~3.15000000000
    E is 0.00100000000, num iterations is        3143, PI is ~3.14300000000
    E is 0.00010000000, num iterations is       31417, PI is ~3.14170000000
    E is 0.00001000000, num iterations is      314160, PI is ~3.14160000000
    E is 0.00000100000, num iterations is     3141593, PI is ~3.14159300000
    E is 0.00000010000, num iterations is    31415927, PI is ~3.14159270000
    E is 0.00000001000, num iterations is   314159266, PI is ~3.14159266000
    E is 0.00000000100, num iterations is  3141592656, PI is ~3.14159265600

	The  final  digit  is  sometimes off by a bit, but the other digits are
always  correct.   Strange things these fractals.  Strange stuff math.  You
just can't keep a good number down.

	The actual value of PI, to more digits that you'll ever need is:

	3.1415926535 8979323846 2643383279 5028841971 6939937510 5820974944...

	This was NOT calculated with this program.  Don't even try.

	For the more graphically oriented, you may want to explore these spikes
with  Mand2000.  However if you aren't careful in your exploration, you may
be  disappointed.   The  first  important  thing to do is to make sure that
Sneaky  pixels  (in  the Setup/Set misc menu) is turned off.  Next, because
the  spikes  are  very  thin  and  can  easily sneak between two columns of
pixels,  you must have the pixels on your monitor lined up so that there is
a  column of pixels at exactly -0.75.  The best way to deal with this is to
zoom  in a bit and then scroll the picture so that the two spikes are quite
close  to  the  left  edge of the screen.  Then bring up the `Set location'
requester  and  type  in -0.75 for the left coordinate.  Now, even when you
scroll  the spikes back to the centre of the screen, or zoom in, there will
still  be  a column of pixels precisely lined up on the spikes.  This makes
it  much  easier to see how close to each other the spikes really get.  The
next step is to increase the number of iterations as high as possible.  The
final  thing  to  do  is to crank up the accuracy a bit - go into the Setup
in  the  Misc menu and set extra accuracy to about four.  Normally Mand2000
tries  to  get away with as little accuracy as possible, for speed reasons,
but  for  calculations  like  this, precise results need a few more digits.
All this should insure that your results will not be severely degraded.

	The  source to this program was provided so that mathematical explorers
can  search  around  and  find  other places in the Mandelbrot set where PI
occurs.   It  can  also  be  found,  for  instance,  at  the  `butt' of the
Mandelbrot  set,  but  I'll  leave  the exact method as an exercise for the
reader.


 	To  find  out  more  about  Boll's  discovery,  read  "Fractals for the
Classroom - Part Two", an excellent book for anybody interested in learning
wierd and wonderful facts about these beatiful monsters.

	When  compiling  this  program  be sure to compile with a very accurate
floating  point  option.   The `Fast Floating Point' (FFP) routines are not
accurate  enough and will cause the program to run forever.  If you have an
FPU, compile and link with the FPU options, because they are both very fast
and very accurate.  And be sure to link with the appropriate floating point
library, needed to print out the results.  */



#include <stdio.h>
#include <stdbool.h>

/* This may be an Aztec specific function - replace it with your own or */
/* just comment it out.  It just checks for control C and exits when it */
/* detects it. */
long Chk_Abort(void);




/*
	Warning.   Set  this  to higher numbers at your own risk.  Even at just
seven,  the  final  pass  will  do 3,141,593 iterations - which takes a few
seconds  even  on  an  '040.   Each  time  you increment NUMPASSES, it does
ten  times  as  many  iterations - if you set it to ten you can expect your
computer to be busy for several hours at least.
	*/

#define	MAXPASSES	10

/*
	For those hard core experimenters who want to try multi-week runs, I've
made  counter  into  a long double instead of a mere long.  This will allow
maximum  counts of somewhere around 16 billion billion instead of only four
billion.   (Half  of  you  will probably complain that a long double can go
much  higher  than  that  -  but  it  can't if you still want to be able to
accurately  add  one  to  it).   However, after that many iterations, it is
unlikely  that  enough  accuracy  will  be left to continue giving you more
digits of PI.  That's chaos for you.
	*/



int main(int argc, char *argv[])
	{
	register long double	zr, zi, cr, ci;
	register long double	rsquared, isquared;
	register long double	counter;
	long double starti;
	short	pass;
	FILE	*mystdout;

    mystdout = stdout;
    fprintf(mystdout, "MandToPi: Copyright ? 1993 Cygnus Software.\n");
	fprintf(mystdout, "Calculating PI based on escape times from the `neck' at (-0.75, 0.0) of\n");
	fprintf(mystdout, "the Mandelbrot set.  Read the file 'MandToPI.c' for more information.\n");
	fprintf(mystdout, "\n");

	/* The initial value for starti and the amount to reduce it by are quite */
	/* arbitrary.  You could just as easily start at 0.683 and multiply by */
	/* 0.5 each time instead.  PI is unavoidable. */
	for (starti = 1.0, pass = 0;  pass < MAXPASSES;  starti *= 0.1, pass++)
		{
		zr = 0.0;
		zi = 0.0;
		cr = -.75;
		ci = starti;
		rsquared = zr * zr;
		isquared = zi * zi;

		/* The guts of the program - standard Mandelbrot iteration formula. */
		for (counter = 0; rsquared + isquared <= 4.0; counter++)
			{
			zi = zr * zi * 2;
			zi += ci;

			zr = rsquared - isquared;
			zr += cr;

			rsquared = zr * zr;
			isquared = zi * zi;

		/* To make the results look pretty, be sure to put .N, where is N is MAXPASSES or greater */
		/* after each percent sign in the following printf control string.  Otherwise you may get */
		/* misaligned printouts or, worse, missing digits. */
		fprintf(mystdout, "E is %.11Lf, num iterations is %11.Lf, PI is ~%.11Lf\n", starti, counter, starti * counter);
		}

	fprintf(mystdout, "\nDone.\n");
		}

	return 0;
	}

