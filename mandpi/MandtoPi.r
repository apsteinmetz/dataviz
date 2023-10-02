MAXPASSES<-10

# /*
# For those hard core experimenters who want to try multi-week runs, I've
# made  counter  into  a long double instead of a mere long.  This will allow
# maximum  counts of somewhere around 16 billion billion instead of only four
# billion.   (Half  of  you  will probably complain that a long double can go
# much  higher  than  that  -  but  it  can't if you still want to be able to
# accurately  add  one  to  it).   However, after that many iterations, it is
# unlikely  that  enough  accuracy  will  be left to continue giving you more
# digits of PI.  That's chaos for you.
#             */


#   int main(int argc, char *argv[])
# {
#   register long double	zr, zi, cr, ci;
#   register long double	rsquared, isquared;
#   register long double	counter;
#   long double starti;
#   register short	abortcounter;
#   short	pass;
#   unsigned short	breakable;


print("Calculating PI based on escape times from the `neck' at (-0.75, 0.0) of")
print("the Mandelbrot set.")
# /* The initial value for starti and the amount to reduce it by are quite */
# /* arbitrary.  You could just as easily start at 0.683 and multiply by */
# /* 0.5 each time instead.  PI is unavoidable. */

starti <- 1
for (pass in 1:MAXPASSES)
{
#  z <-  complex(real=0.0,imaginary = 0.0)
#  c <- complex(real = -0.75,imaginary = starti)
    z= 0.0
    c = 0.25 + starti #  on the real axis so no imaginary part required.
  #/* The guts of the program - standard Mandelbrot iteration formula. */
  #    for (counter = 0; rsquared + isquared <= 4.0; counter++)
  counter = 0
  while (z <=2.0)
  {
    z <- z^2+c
    counter = counter + 1
  }
  starti<-starti*0.01
  
  # /* To make the results look pretty, be sure to put .N, 
  # where is N is MAXPASSES or # # greater after each percent sign 
  # in the following printf control string.  Otherwise # you may get misaligned printouts or, worse, missing digits. */
  print(sprintf("E is %1.10f, num iterations is %20.0f, PI is ~%f", starti,counter, starti^.5*10* counter))
}  

print("Done")
