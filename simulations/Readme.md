This README file describes how to replicate the estimation and simulations
results. Most of the subroutines have trivial names, so I will just include the
information that is necessary to run the codes or that is not obvious to a
non-fortran user. Details on the specific subroutines can be found inside the f90 files.

All these codes are adapted from Dearden et al. (2008)'s original codes to
accommodate specificities of the Spanish labor market (as well as the data) that we discuss 
in the paper. We thank them for sharing them with us.
    

MODULES - TO BE CALLED BY ALL SUBROUTINES:

    - Paramaters.f90 sets the value for paths, model of interest (e.g. males
      with higher education, females with higher education...), and parametric
      assumptions.

    - Globals.f90 contains the declaration of global variables and vectors that
      will be used in a few subroutines. 


ROUTINES - MAIN CODE:

    - Main.f90 is the master file. It calls the subroutines in the right order.

    - GetData.f90 imports empirical autocovariances and work probits obtained in
      STATA.

    - Estimation.f90 constructs the initial vector of parameters based on the
      gueses (MakeGuess.f90) and parametric choices (Parameters.f90). Then it
      calls a non-linear minimization algorithm in minpack.f90 to minimize the
      quadratic distance between the simulated and the empirical moments. 

    - ObjFunction.f90 calculates the vector of moment distances * sqrt(weight).
      The algortihm then minimizes the sum of the squares of the M nonlinear
      functions in N variables by a modification of the Levenberg-Marquardt
      algorithm.

