# 
#                            COPYRIGHT (c) 1990
#                     Kapteyn Astronomical Institute
#       University of Groningen  -  9700 AV Groningen,  The Netherlands
# 
# #>             combin.dc1
# am:       COMBIN
# se:       Combine sets to create new sets using mathematical
#                expressions.
# ory:      ANALYSIS, COMBINATION, MANIPULATION, CALCULATION
#           combin.shl
# r:        M. Vogelaar
# rds:
# ESULT01=  Combination code for first output set.
#                The code must contain at least one parameter ($n).
#                If you want a set with for example only noise, try
#                something like:  ($1 - $1) + expression for noise.
# ESULT02=  Combination code for the second output set.
#                If carriage return is given, COMBIN assumes that
#                no other output combinations are wanted.
# ESULT03=  etc.
# ET01=     Input set (and subsets) for first combination.
#                Maximum number of subsets is 2048. The output
#                set(s) will get the same sizes as this first
#                input set.
# OX01=     Frame for first input parameter.       [entire subset]
# ET02=     Input set (and subsets) for second parameter.
#                Input is accepted if the number of subsets is
#                equal to the previous number of subsets.
#                If you want to use COMBIN outside a given box
#                (see OPTION= keyword), all the input sets need
#                to have the same axis sizes.
# OX02=     Frame for second parameter.             [previous box]
#                The axis lengths of this box have to be equal to the
#                axis lengths of the previous box. If using COMBIN
#                outside a box, the relative position of this box
#                has to be the same as the position of the first box.
# ET03=     etc.
# OX03=     etc.
# ETOUT01=  Give set and subset(s) where the first combination is
#                to be stored.
# ETOUT02=  Give set and subset(s) where the second combination is
#                to be stored.
# ETOUT03=  etc.
# PTION=                                                       [0]
#                0. COMBIN operates inside given box and blanks values
#                          outside this box
#                1. COMBIN operates inside given box and transfers
#                         'outside' values of SET01
#                2. COMBIN operates outside given box and blanks values
#                          inside this box
#                3. COMBIN operates outside given box and transfers
#                         'inside' values of SET01
# 
# iption:   This program allows the user to create sets which are
#                a (complicated) combination of other sets. The
#                combination description language is best illustrated
#                by the following example: suppose that one
#                has two sets which are the result of a Fourier
#                transform, one containing the real part and the other
#                the imaginary part of the result.  The code needed to
#                convert these two sets to amplitude and phase is the
#                following:
# 
#                RESULT01= SQRT( $1*$1 + $2*$2 )            (amplitude)
#                RESULT02= DEG( ATAN2 ( $2, $1 ) )   (phase in degrees)
# 
#                Here $1 and $2 denote the first and second input sets.
#                In this example two input sets result in two output
#                sets. But just as easy we could produce a third output
#                set which contains the scaled real part. Then the code
#                would be:
# 
#                RESULT03= 2 * $1 + 1                (scaled real part)
# 
#                So with COMBIN we can combine M input sets into N
#                output sets.
#                The example above can be achieved with the following
#                keywords:
# 
#                COMBIN,RESULT01=sqrt($1*$1+$2*$2)
#                COMBIN,RESULT02=deg(atan2($2,$1))
#                COMBIN,RESULT03=<carriage return>
# 
#                Note: Two different types of output sets will be
#                      produced
# 
#                Input sets:
# 
#                COMBIN,SET01=AURORA_RE  FREQ 1:10
#                COMBIN,SET02=AURORA_IM  FREQ 1:10
# 
#                Output sets:
# 
#                COMBIN,SETOUT01=AURORA_AM  FREQ 1:10
#                COMBIN,SETOUT02=AURORA_PH  FREQ 1:10
# 
#                Note: We used (the default) option 0
# 
#                Available mathematics:
# 
#               1) functions; syntax ff(..); where ff is one of the
#                  following available functions:
#                  abs(x)         absolute value of x
#                  sqrt(x)        square root of x
#                  sin(x)         sine of x
#                  asin(x)        inverse sine of x
#                  cos(x)         cosine of x
#                  acos(x)        inverse cosine of x
#                  tan(x)         tangent of x
#                  atan(x)        inverse tan of x
#                  atan2(x,y)     inverse tan (mod 2pi) x = sin, y = cos
#                  exp(x)         exponential of x
#                  ln(x)          natural log of x
#                  log(x)         log (base 10) of x
#                  sinh(x)        hyperbolic sine of x
#                  cosh(x)        hyperbolic cosine of x
#                  tanh(x)        hyperbolic tangent of x
#                  rad(x)         convert x to radians
#                  deg(x)         convert x to degrees
#                  erf(x)         error function of x
#                  erfc(x)        1-error function
#                  max(x,y)       maximum of x and y
#                  min(x,y)       minimum of x and y
#                  sinc(x)        sin(x)/x (sinc function)
#                  sign(x)        sign of x (-1,0,1)
#                  mod(x,y)       gives remainder of x/y
#                  int(x)         truncates to integer
#                  nint(x)        nearest integer
#                  ranu(x,y)      generates uniform noise between x and y
#                  rang(x,y)      generates gaussian noise with mean x
#                                 and dispersion y
#                  ranp(x)        generates poisson noise with mean x
#                  ifeq(x,y,a,b)  returns a if x == y, else b
#                  ifne(x,y,a,b)  returns a if x != y, else b
#                  ifgt(x,y,a,b)  returns a if x > y, else b
#                  ifge(x,y,a,b)  returns a if x >= y, else b
#                  iflt(x,y,a,b)  returns a if x < y, else b
#                  ifle(x,y,a,b)  returns a if x <= y, else b
#                  ifblank(x,a,b) returns a if x == BLANK, else b
# 
#                  Some (statistical) functions have a variable number
#                  of arguments:
# 
#                  sum(x1,..,xn)     sum of elements x1 .. xn
#                  mean(x1,..,xn)    mean
#                  var(x1,..,xn)     variance
#                  sdev(x1,..,xn)    standard deviation
#                  adev(x1,..,xn)    absolute deviation
#                  skew(x1,..,xn)    skewness
#                  kurt(x1,..,xn)    kurtosis
#                  median(x1,..,xn)  median
#                  nblanks(x1,..,xn) number of blanks
# 
#                  max(x1,..,xn)     maximum of elements x1 .. xn
#                  min(x1,..,xn)     minimum
# 
#                  Note that n <= 128
# 
#               2) constants; syntax cc; where cc is one of the following
#                  available constants:
#                  PI             3.14159....
#                  C              speed of light (SI)
#                  H              Planck (SI)
#                  K              Boltzmann (SI)
#                  G              gravitation (SI)
#                  S              Stefan-Boltzman (SI)
#                  M              mass of sun (SI)
#                  P              parsec (SI)
#                  BLANK          Universal undefined value
#                  Note: the Hubble constant is not included.
#               3) operators; syntax op; where op is one of the following
#                  available operators:
#                  +              addition
#                  -              subtraction
#                  *              multiplication
#                  /              division
#                  **             power
#               4) parameters; syntax $n; where 1 <= n <= 128.
# ks:      A) The calculations are all done in double precision.
# 
#               B) BLANK values are recognized. Any operation on a
#                  BLANK causes the result to be set to BLANK (except the
#                  function IFBLANK). Also all illegal operations, like
#                  dividing by zero or the logarithm of a negative value,
#                  will cause the result to be set to BLANK.
# 
#               C) If you cannot find your favorite constant or function
#                  in the list, please contact Kor Begeman.  He might be
#                  persuaded to put it in.
# les:     1) Add three maps
#                  RESULT01=$1+$2+$3
#                  RESULT02=<carriage return>
#                  SET01= set and subset(s) of first parameter
#                  SET02= set and subset(s) of second parameter
#                  SET03= set and subset(s) of third parameter
#                  SETOUT01= set and subset(s) of sum
# 
#               2) Add gaussian noise to a map with zero mean and a
#                  rms of 2
#                  RESULT01=$1+rang(0,2)
#                  RESULT02=<carriage return>
#                  SET01= input set and subset(s)
#                  SETOUT01= output set and subset(s) with noise added
# 
#               3) Clip values in a set between -4 and 5 and replace with
#                 'BLANK'
#                  RESULT01=ifgt($1,5,BLANK,iflt($1,-4,BLANK,$1))
#                  RESULT02=<carriage return>
#                  SET01= input set and subset(s)
#                  SETOUT01= clipped output set and subset(s) with values
#                            between -4 and 5
# 
#               4) Conditional transfer: if value in second set is
#                  greater than 2 then result is value from first set,
#                  else set this value to BLANK
#                  RESULT01=ifgt($2,2,$1,BLANK)
#                  RESULT02=<carriage return>
#                  SET01= input set and subset(s)
#                  SET02= test set and subset(s)
#                  SETOUT01= conditional transfered output set and
#                            subset(s)
# 
#               5) Calculate median of three sets and put result in outpu
#                  set MEDIAN
#                  RESULT01=median($1,$2,$3)
#                  RESULT02=<carriage return>
#                  SET01=OPTC1
#                  SET01=OPTC2
#                  SET01=OPTC3
#                  SETOUT01=MEDIAN
# :
# #<
# 
# EJECT: ***** Program COMBIN ***** (SHELTRAN EJECT directive)
# UNTRANSLATED (L236): PROGRAM COMBIN
#      Declaration of parameters:
# HARACTER*(*)  ident
# UNTRANSLATED (L239): PARAMETER    ( ident = '  Version 1.0  Mar 14, 1990 ' )
# INCLUDE NTEGER        maxsubs (SHELTRAN I directive - manual include/translation needed if it's Python code)
# UNTRANSLATED (L241): PARAMETER    ( maxsubs = 2048 )
# INCLUDE NTEGER        maxaxes (SHELTRAN I directive - manual include/translation needed if it's Python code)
# UNTRANSLATED (L243): PARAMETER    ( maxaxes = 10 )
# NOTE: Default options for use in USERxxx() routines
# INCLUDE NTEGER        none, request, hidden (SHELTRAN I directive - manual include/translation needed if it's Python code)
# UNTRANSLATED (L246): PARAMETER    ( none    = 0 )
# UNTRANSLATED (L247): PARAMETER    ( request = 1 )
# UNTRANSLATED (L248): PARAMETER    ( hidden  = 2 )
# NOTE: Buffer size for I/O
# INCLUDE NTEGER        maxIObuf (SHELTRAN I directive - manual include/translation needed if it's Python code)
# UNTRANSLATED (L251): PARAMETER    ( maxIObuf = 4096 )
# NOTE: Remove with 'WMINMAX' old minmax descriptors
# INCLUDE NTEGER        remove (SHELTRAN I directive - manual include/translation needed if it's Python code)
# UNTRANSLATED (L254): PARAMETER    ( remove = 1 )
# NOTE: Max. number of characters in 1 comb. description
# INCLUDE NTEGER        maxchars (SHELTRAN I directive - manual include/translation needed if it's Python code)
# UNTRANSLATED (L257): PARAMETER    ( maxchars = 256 )FIE' has 16 different buffers for results
# INCLUDE NTEGER        maxrslt (SHELTRAN I directive - manual include/translation needed if it's Python code)
# UNTRANSLATED (L259): PARAMETER    ( maxrslt = 16 )
# NOTE: Syntax parameters: $n with 1 <= n <= 32
# INCLUDE NTEGER        maxpars (SHELTRAN I directive - manual include/translation needed if it's Python code)
# UNTRANSLATED (L262): PARAMETER    ( maxpars = 32 )
# NOTE: Coordinate word for whole set
# INCLUDE NTEGER        wholeset (SHELTRAN I directive - manual include/translation needed if it's Python code)
# UNTRANSLATED (L265): PARAMETER    ( wholeset = 0 )
#      Declarations for GDSINP:
# INCLUDE NTEGER        GDSINP (SHELTRAN I directive - manual include/translation needed if it's Python code)
# HARACTER*80   setname( maxpars )
# NOTE: Array containing subset coordinate words
# INCLUDE NTEGER        subsets( maxsubs, maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Axis permutation array
# INCLUDE NTEGER        axperm(  maxaxes, maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        axcount( maxaxes, maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Number of subsets
# INCLUDE NTEGER        nsubs (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Determined at first input of sets
# INCLUDE NTEGER        nsubsmax (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        dfault (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Number of output device [0..16]
# INCLUDE NTEGER        devicenum (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Keywords for numerous inputs
# HARACTER*10   keyword
# HARACTER*10   setkwrd
# HARACTER*10   boxkwrd
# HARACTER*40   message
# NOTE: Algorithm with repeat axes ==> class = 1
# INCLUDE NTEGER        class (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Dimension of the subsets
# INCLUDE NTEGER        subdim (SHELTRAN I directive - manual include/translation needed if it's Python code)
#      Declarations for GDSBOX:
# NOTE: Grid vectors of sub frames of input sets
# INCLUDE NTEGER        BgridLO(  maxaxes, maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        BgridHI(  maxaxes, maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Grid vectors of entire frames of inp. sets
# INCLUDE NTEGER        FgridLO(  maxaxes, maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        FgridHI(  maxaxes, maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Same for output sets
# INCLUDE NTEGER        FgridLOO( maxaxes, maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        FgridHIO( maxaxes, maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: What's the default in GDSBOX?
# INCLUDE NTEGER        boxopt (SHELTRAN I directive - manual include/translation needed if it's Python code)
#      Declarations for GDSOUT:
# INCLUDE NTEGER        GDSOUT (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Number of output sets
# INCLUDE NTEGER        nsubsO (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        subsetsO( maxsubs, maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        axpermO(  maxaxes, maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        axcountO( maxaxes, maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# HARACTER*80   setnameO( maxrslt )
#      Functions:
# NOTE: Extracts grid coordinate from coord. word
# INCLUDE NTEGER        GDSC_GRID (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Returns coordinate word
# INCLUDE NTEGER        GDSC_FILL (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Returns .TRUE. if inside defined sub frame
# UNTRANSLATED (L316): LOGICAL        OUTSIDEPTR
# NOTE: Returns .TRUE. if outside defined sub frame
# UNTRANSLATED (L318): LOGICAL        INSIDEPTR
# NOTE: Determine length of string
# INCLUDE NTEGER        NELC (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Obtain the name under which a GIPSY task runs
# HARACTER*9    MYNAME
# NOTE: Get length of input string
# INCLUDE NTEGER        USERTEXT (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Decode a string with math expression for FIEDO
# INCLUDE NTEGER        FIEINI (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Evaluate the code generated by FIEINI
# INCLUDE NTEGER        FIEDO (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: User integer input
# INCLUDE NTEGER        USERINT (SHELTRAN I directive - manual include/translation needed if it's Python code)
#      Variables for the algorithms:
# NOTE: Various counters
# INCLUDE NTEGER        m, i, q (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: There are totpixels in one subset
# INCLUDE NTEGER        totpixels (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Array version of transfer id's
# INCLUDE NTEGER        tids(   maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: For output data
# INCLUDE NTEGER        tidsO(  maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Special tid for outside operations
# INCLUDE NTEGER        tidT (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Counter for subroutine MINMAX3
# INCLUDE NTEGER        mcount( maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Number of pointers in INITPTR buffer
# INCLUDE NTEGER        ptrcount (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        stilltowrite (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Coordinate words for total & sub frame
# INCLUDE NTEGER        Fcwlo(  maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        Fcwhi(  maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Array version of Bcwlo, Bcwhi
# INCLUDE NTEGER        Bcwlo(  maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        Bcwhi(  maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Array version of FcwloO, FcwhiO
# INCLUDE NTEGER        FcwloO( maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        FcwhiO( maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: For use in INSIDEPTR/OUTSIDEPTR
# INCLUDE NTEGER        bufptr (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Dynamical I/O buffer length
# INCLUDE NTEGER        numinreadbuf (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Number of elements inside/outside sub frame
# INCLUDE NTEGER        numpixin (SHELTRAN I directive - manual include/translation needed if it's Python code)
# INCLUDE NTEGER        numpixout (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Number of pixels actually in read buffer
# INCLUDE NTEGER        numpixread (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: How far we proceeded, used in STABAR
# UNTRANSLATED (L366): REAL           curval
# NOTE: Help variable in STABAR
# UNTRANSLATED (L368): REAL           ratio
# NOTE: Offset for 1 dimensional dataI array
# INCLUDE NTEGER        offset (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: For updating number of blanks
# INCLUDE NTEGER        nblanks( maxsubs,   maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: For updating minimum and maximum of subset
# UNTRANSLATED (L374): REAL           minval(  maxsubs,   maxrslt )
# UNTRANSLATED (L375): REAL           maxval(  maxsubs,   maxrslt )
# NOTE: Arrays holding the data
# UNTRANSLATED (L377): REAL           dataO(   maxIObuf,  maxrslt )
# UNTRANSLATED (L378): REAL           dataI(   maxIObuf * maxpars )
# NOTE: Data array only for transfer option
# UNTRANSLATED (L380): REAL           dataT(   maxIObuf )
# NOTE: Status of function FIEDO
# INCLUDE NTEGER        fieresult (SHELTRAN I directive - manual include/translation needed if it's Python code)
#      Miscellaneous:
# NOTE: Error codes for Range and Grid routines
# INCLUDE NTEGER        Rerror, Gerror (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Coordinate words to determine total range
# INCLUDE NTEGER        cwlo, cwhi (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Option 0..3, for different operations
# INCLUDE NTEGER        option (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Choose COMBIN operating in- or outside box
# UNTRANSLATED (L391): LOGICAL        workinside
# NOTE: Choose between transfer or blanking
# UNTRANSLATED (L393): LOGICAL        transfer
# NOTE: True when grids of input edges are equal
# UNTRANSLATED (L395): LOGICAL        equalgrids
# NOTE: Loop guard for valid set input
# UNTRANSLATED (L397): LOGICAL        agreed
# NOTE: Loop guard for valid box input
# UNTRANSLATED (L399): LOGICAL        equalbox
# NOTE: Loop guard for 'work outside' option
# UNTRANSLATED (L401): LOGICAL        equalframe
# NOTE: Another guard for 'work outside' option
# UNTRANSLATED (L403): LOGICAL        equalpos
# NOTE: First input of a valid set
# UNTRANSLATED (L405): LOGICAL        first
# NOTE: Length of result string
# INCLUDE NTEGER        len (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Number of results (=output sets)
# INCLUDE NTEGER        numouts (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Counter for 'numouts'
# INCLUDE NTEGER        out (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Total number of output sets
# INCLUDE NTEGER        totouts (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Number of parameters = num. of input sets
# INCLUDE NTEGER        numpars (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Maximum number of counted parameters
# INCLUDE NTEGER        totpars (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Id. for each result string
# INCLUDE NTEGER        fid( maxrslt ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Position of error in result string
# INCLUDE NTEGER        errpos (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Current parameter
# INCLUDE NTEGER        par (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Length of axes of first box
# INCLUDE NTEGER        Blen1( maxpars ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Length of one axis of other boxes
# INCLUDE NTEGER        Blen2 (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Display name of this program
# HARACTER*9    task
# NOTE: Storage for 'maxrslt' descriptions
# HARACTER*( maxchars )   resultstr( maxrslt )
# EJECT: ***** COMBIN: Main ***** (SHELTRAN EJECT directive)
# NOTE: Enter HERMES
# ALL INIT
# NOTE: Get task name
# UNTRANSLATED (L436): task = MYNAME()
# NOTE: Show task and version number
# ALL ANYOUT( 8, ( task(:NELC(task) ) // ident) )
#      Options for work area, asked before(!) GDSBOX:
d = 'OPTION='
e = 'Give option:   [0]'
# UNTRANSLATED (L442): dfault  = hidden
#      Choose one of the four available options:
# Label only: REPEAT
# INCLUDE F ( USERINT( option, 1, dfault, keyword, message ) .EQ. 0 ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# UNTRANSLATED (L447): option = 0
# IF
# UNTRANSLATED (L449): agreed = ( (option .GE. 0) .AND. (option .LE. 3) )
# INCLUDE F (.NOT. agreed) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# UNTRANSLATED (L452): dfault  = request
e = 'COMBIN: Illegal opt. try again: [0]'
# ALL CANCEL( keyword )
# IF
# UNTRANSLATED (L456): UNTIL agreed
#      Translate options into logicals to work with:
# INCLUDE F (option .EQ. 0) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
side = .TRUE.
er = .FALSE.
# ALL ANYOUT( 3, 'Option: operate inside box, blank outside' )
# IF
# INCLUDE F (option .EQ. 1) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
side = .TRUE.
er = .TRUE.
# ALL ANYOUT( 3, 'Option: operate inside box, transfer outside' )
# IF
# INCLUDE F (option .EQ. 2) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
side = .FALSE.
er = .FALSE.
# ALL ANYOUT(3, 'Option: operate outside box, blank inside' )
# ALL ANYOUT(3, 'Note: axis sizes of frames need to be equal!')
# IF
# INCLUDE F (option .EQ. 3) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
side = .FALSE.
er = .TRUE.
# ALL ANYOUT(3, 'Option: operate outside box, transfer inside' )
# ALL ANYOUT(3, 'Note: axis sizes of frames need to be equal!')
# IF
#      Input of combination descriptions:
s = 1
s = 0
d = 'RESULT01='
e = 'Give expression: '
# UNTRANSLATED (L489): dfault  = none
# Label only: REPEAT
# UNTRANSLATED (L491): fid(numouts) = 0
str[numouts - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: numouts - 1 = ' '
# UNTRANSLATED (L493): len = USERTEXT( resultstr(numouts), dfault, keyword, message )
# NOTE: Check string & prepare for next input
# INCLUDE F (len .NE. 0) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# NOTE: Check whether input string is ok
s = FIEINI(resultstr[numouts - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: numouts - 1, fid[numouts - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: numouts - 1, errpos)
# NOTE: Store the maximum value of numpars in totpars
s = MAX[numpars - 1][totpars - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: numpars - 1 # REVIEW INDEX: Verify subtraction for 0-based: totpars - 1
# INCLUDE F (numpars .GT. 0) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
s = numouts + 1
# UNTRANSLATED (L504): dfault = request
# EJECT: LSE (SHELTRAN EJECT directive)
# ALL CANCEL( keyword )
# INCLUDE F (numpars .EQ. 0) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# ALL ANYOUT( 3, 'COMBIN: No parameters specified' )
# IF
# INCLUDE F (numpars .LT. 0) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# UNTRANSLATED (L513): WRITE( message,           '(''COMBIN: Error in string at position '', I3 )' )           errpos
# ALL ANYOUT( 3, message )
# IF
# IF
# UNTRANSLATED (L517): WRITE( keyword, '(''RESULT'', I2.2, ''='')' ) numouts
# UNTRANSLATED (L518): WRITE( message, '(''Give expression: '', I2, '' [stop]'' )' )           numouts
# IF
# UNTRANSLATED (L520): UNTIL ( ( (numouts-1) .EQ. maxrslt) .OR. (len .EQ. 0) )
# NOTE: Compensate for previous loop
s = numouts - 1
#      Get sets corresponding to the parameters:
# NOTE: Prepare GDSINP for first SET
# UNTRANSLATED (L525): dfault = none
# NOTE: Application repeats operation for each subset
# UNTRANSLATED (L527): class  = 1
# NOTE: First time free choice, later not!
# UNTRANSLATED (L529): subdim = 0
num = 11
# NOTE: Do it for all input parameters
# FORTRAN DIRECTIVE: OR par = 1, totpars (SHELTRAN F directive)
# UNTRANSLATED (L533): first = (par .EQ. 1)
# UNTRANSLATED (L534): WRITE( setkwrd, '(''SET'', I2.2, ''='' )' ) par
# UNTRANSLATED (L535): WRITE( message, '(''Set (and subsets) of parameter '', I2 )' )         par
# NOTE: Until nsubs eq. to first time number of subs.
# Label only: REPEAT
# UNTRANSLATED (L538): nsubs = GDSINP( setname(par), subsets(1, par), maxsubs,                    dfault, setkwrd, message, devicenum,                    axperm(1, par), axcount(1, par),                    maxaxes, class, subdim )
# INCLUDE F first (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Determine axis lengths & first # of subsets
# Label only: THEN
ax = nsubs
els = 1
# FORTRAN DIRECTIVE: OR m = 1, subdim (SHELTRAN F directive)
els = totpixels * axcount[m - 1][0] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: m - 1
# FOR
# IF
# UNTRANSLATED (L548): agreed = (nsubs .EQ. nsubsmax)
# INCLUDE F (.NOT. agreed) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# ALL ANYOUT( 3, 'COMBIN: Wrong number of subsets' )
# ALL CANCEL( setkwrd )
# NOTE: Else if agreed get grids
# EJECT: LSE (SHELTRAN EJECT directive)
# UNTRANSLATED (L555): Rerror = 0
# UNTRANSLATED (L556): Gerror = 0
# ALL GDSC_RANGE( setname(par), 0, cwlo, cwhi, Rerror )
# FORTRAN DIRECTIVE: OR m = 1, subdim (SHELTRAN F directive)
# FORTRAN DIRECTIVE: gridLO(m, par) = GDSC_GRID( setname(par),                            ax (SHELTRAN F directive)
# FORTRAN DIRECTIVE: gridHI(m, par) = GDSC_GRID( setname(par),                            ax (SHELTRAN F directive)
# FOR
rids = .TRUE.
# INCLUDE F (.NOT. first) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# NOTE: Are frst and scnd sets comparable on sset level?
# FORTRAN DIRECTIVE: OR q = 1, subdim (SHELTRAN F directive)
rids = ( equalgrids .AND.                       ( FgridLO[q - 1][(par-1) - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: (par-1) - 1 .EQ.                         FgridLO[q - 1][par - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: par - 1                       ) .AND.                       ( FgridHI[q - 1][(par-1) - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: (par-1) - 1 .EQ.                         FgridHI[q - 1][par - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: par - 1                       ) )
# FOR
# NOTE: For outside operations eq. Fsizes are needed
# INCLUDE F ( (.NOT. workinside) .AND. (.NOT. equalgrids) ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# NOTE: Compare sizes of frames
rame = .TRUE.
# FORTRAN DIRECTIVE: OR q = 1, subdim (SHELTRAN F directive)
rame = (equalframe .AND.                         ( axcount[q - 1][par - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: par - 1 .EQ. axcount[q - 1][0] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 ) )
# FOR
# INCLUDE F (.NOT. equalframe) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# UNTRANSLATED (L579): agreed = .FALSE.
# ALL ANYOUT( 3, 'COMBIN: For this option you need' //                            ' same axis lengths as ' )
# ALL ANYOUT( 3, '        previous set frame!' )
# ALL CANCEL( setkwrd )
# IF
# NOTE: End if work outside option
# IF
# NOTE: End of not first
# IF
# NOTE: End of agreed / not agreed
# IF
# UNTRANSLATED (L590): UNTIL agreed
#        Input of sub frames with GDSBOX:
# NOTE: Prepare for a frame for first set
# UNTRANSLATED (L593): dfault    = request
# UNTRANSLATED (L594): WRITE( boxkwrd, '(''BOX'', I2.2, ''='' )' ) par
e = ' '
# NOTE: Default is entire subset
# INCLUDE F first (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# UNTRANSLATED (L599): boxopt = 0
# IF
num = 11
# Label only: REPEAT
# NOTE: Set default sizes for GDSBOX
# INCLUDE F (.NOT. first) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# NOTE: Fill arrays with GRIDS for GDSBOX
# FORTRAN DIRECTIVE: OR q = 1, subdim (SHELTRAN F directive)
I[q - 1][par - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: par - 1 = BgridHI[q - 1][(par - 1) - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: (par - 1) - 1
O[q - 1][par - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: par - 1 = BgridLO[q - 1][(par - 1) - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: (par - 1) - 1
# FOR
# NOTE: Default in B..LO & B..HI
# UNTRANSLATED (L612): boxopt = 6
# INCLUDE F (.NOT. equalgrids) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Fill arrays with SIZES for GDSBOX
# Label only: THEN
# FORTRAN DIRECTIVE: OR q = 1, subdim (SHELTRAN F directive)
I[q - 1][par - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: par - 1 = BgridHI[q - 1][(par-1) - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: (par-1) - 1-BgridLO[q - 1][(par-1) - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: (par-1) - 1 + 1
# FOR
# NOTE: Box restricted to size in B..HI
# UNTRANSLATED (L620): boxopt = ( 8 + 4 )
# NOTE: End of not equalgrids
# IF
# NOTE: End if not first
# IF
# ALL GDSBOX( BgridLO(1, par), BgridHI(1, par),                 setname(par), subsets(1, par),                 dfault, boxkwrd, message, devicenum, boxopt )
os = .TRUE.
# INCLUDE F first (SHELTRAN I directive - manual include/translation needed if it's Python code)
# NOTE: Determine length of box axes of first box
# Label only: THEN
ox = .TRUE.
# FORTRAN DIRECTIVE: OR q = 1, subdim (SHELTRAN F directive)
# UNTRANSLATED (L632): Blen1(q) = BgridHI(q, 1) - BgridLO(q, 1) + 1
# FOR
# EJECT: LSE (SHELTRAN EJECT directive)
# NOTE: Not the first ==> compare box sizes
ox = .TRUE.
# FORTRAN DIRECTIVE: OR q = 1, subdim (SHELTRAN F directive)
# UNTRANSLATED (L638): Blen2 = BgridHI(q, par) - BgridLO(q, par) + 1
ox = ( equalbox .AND. (Blen1[q - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 .EQ. Blen2) )
# FOR
# NOTE: For outside operations, box must have same
os = .TRUE.
# NOTE: relative position as previous box
# INCLUDE F (equalbox .AND. (.NOT. workinside) ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# FORTRAN DIRECTIVE: OR q = 1, subdim (SHELTRAN F directive)
os = ( equalpos .AND. (                       (BgridLO[q - 1][par - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: par - 1 - FgridLO[q - 1][par - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 # REVIEW INDEX: Verify subtraction for 0-based: par - 1) .EQ.                       (BgridLO[q - 1][0] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1 - FgridLO[q - 1][0] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: q - 1) ) )
# FOR
# INCLUDE F (.NOT. equalpos) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# ALL ANYOUT( 3, 'COMBIN: Wrong position of box' )
# ALL CANCEL( boxkwrd )
# IF
# IF
# NOTE: End first or other set
# IF
# INCLUDE F (.NOT. equalbox) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# ALL ANYOUT( 3, 'COMBIN: Wrong size of box!' )
# ALL CANCEL( boxkwrd )
# IF
ox = (equalbox .AND. equalpos)
# NOTE: Now its ok with the boxes also
# UNTRANSLATED (L664): UNTIL equalbox
# NOTE: Close after 'totpars'
# FOR
#      Create output sets:
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
# UNTRANSLATED (L669): WRITE( keyword, '(''SETOUT'', I2.2, ''='' )' ) out
# NOTE: Assign GDSINP buffer of SET01 to GDSOUT
# ALL GDSASN( 'SET01=', keyword, class )
# UNTRANSLATED (L672): dfault    = none
e = 'Give output set[and subsets - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: and subsets - 1:'
num = 11
# NOTE: Until the correct number of SETOUT subsets
# Label only: REPEAT
# UNTRANSLATED (L677): nsubsO = GDSOUT( setnameO(out), subsetsO(1, out),                     nsubsmax, dfault, keyword, message,                     devicenum, axpermO(1, out),                     axcountO(1, out), maxaxes )
# UNTRANSLATED (L678): agreed = (nsubsO .EQ. nsubsmax)
# INCLUDE F (.NOT. agreed) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# ALL ANYOUT( 3, 'COMBIN: Wrong number of subsets' )
# ALL CANCEL( keyword )
# IF
# UNTRANSLATED (L684): UNTIL agreed
# UNTRANSLATED (L685): Rerror = 0
# UNTRANSLATED (L686): Gerror = 0
# ALL GDSC_RANGE( setnameO(out), wholeset, cwlo, cwhi, Rerror )
# FORTRAN DIRECTIVE: OR q = 1, subdim (SHELTRAN F directive)
# FORTRAN DIRECTIVE: gridLOO(q, out) = GDSC_GRID( setnameO, axpermO(q, out), (SHELTRAN F directive)
# FORTRAN DIRECTIVE: gridHIO(q, out) = GDSC_GRID( setnameO, axpermO(q, out), (SHELTRAN F directive)
# FOR
# NOTE: End for, for all setouts
# FOR
#      Fill buffers and calculate:
# NOTE: cw=coordinate word, lo=lower, hi=upper
# NOTE: F=Frame, B=Box
# FORTRAN DIRECTIVE: OR m = 1, nsubsmax (SHELTRAN F directive)
# NOTE: Determine coordinate words for input sets
# FORTRAN DIRECTIVE: OR par = 1, totpars (SHELTRAN F directive)
# FORTRAN DIRECTIVE: cwlo(par) = GDSC_FILL( setname(par), subsets(m, par), (SHELTRAN F directive)
# FORTRAN DIRECTIVE: cwhi(par) = GDSC_FILL( setname(par), subsets(m, par), (SHELTRAN F directive)
# UNTRANSLATED (L702): Bcwlo(par) = GDSC_FILL( setname(par), subsets(m, par),                            BgridLO(1, par) )
# UNTRANSLATED (L703): Bcwhi(par) = GDSC_FILL( setname(par), subsets(m, par),                            BgridHI(1, par) )
# UNTRANSLATED (L704): tids(par) = 0
# FOR
# NOTE: Determine coordinate words for output sets
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
# FORTRAN DIRECTIVE: cwloO(out) = GDSC_FILL( setnameO(out), subsetsO(m, out), (SHELTRAN F directive)
# FORTRAN DIRECTIVE: cwhiO(out) = GDSC_FILL( setnameO(out), subsetsO(m, out), (SHELTRAN F directive)
# NOTE: Reset transfer id for output data
# UNTRANSLATED (L711): tidsO(out)  = 0
# NOTE: Reset counter for subroutine MINMAX3
(out) = 0
# FOR
nt = 0
read = 0
# UNTRANSLATED (L717): tidT       = 0
owrite = totpixels
# Label only: REPEAT
# NOTE: # per iteration cannot exceed maxIObuf
eadbuf = MIN[maxIObuf - 1][stilltowrite - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: maxIObuf - 1 # REVIEW INDEX: Verify subtraction for 0-based: stilltowrite - 1
# NOTE: On exit ptrcount contains # ptrs in buf.
# ALL INITPTR( FgridLO(1, 1), FgridHI(1, 1),                  BgridLO(1, 1), BgridHI(1, 1),                  subdim, numinreadbuf, ptrcount )
# NOTE: Read full buff., all first set data is needed
# INCLUDE F transfer (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# ALL GDSI_READ( setname(1), Fcwlo(1), Fcwhi(1),                      dataT, numinreadbuf, numpixread, tidT )
# IF
# INCLUDE F workinside (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# UNTRANSLATED (L731): WHILE ( OUTSIDEPTR( bufptr, numpixout) )
# NOTE: Then set to blank
# INCLUDE F (.NOT. transfer) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
# ALL SETNFBLANK( dataO(bufptr+1, out), numpixout )
# FOR
# NOTE: Else if transfer is needed ...
# EJECT: LSE (SHELTRAN EJECT directive)
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
# ALL MOVER( dataT(bufptr+1), dataO(bufptr+1, out),                        numpixout )
# FOR
# IF
# WHILE
read = 0
# UNTRANSLATED (L746): WHILE ( INSIDEPTR( bufptr, numpixin ) )
# NOTE: Read data of all parameters in a 1 dim. array
# UNTRANSLATED (L748): offset = 1
# FORTRAN DIRECTIVE: OR par = 1, totpars (SHELTRAN F directive)
# ALL GDSI_READ( setname(par),                          Bcwlo(par), Bcwhi(par),                          dataI(offset), numpixin, numpixread,                          tids(par) )
# UNTRANSLATED (L751): offset = offset + numpixread
# FOR
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
# NOTE: Do the calculations
ult = FIEDO(dataI, numpixin, dataO[(bufptr+1) - 1][out - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: (bufptr+1) - 1 # REVIEW INDEX: Verify subtraction for 0-based: out - 1, fid[out - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: out - 1 )
# INCLUDE F ( fieresult .LT. 0 ) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# ALL ERROR( 4, 'COMBIN: No evaluation possible' )
# IF
# FOR
# WHILE
# NOTE: If NOT workinside ==> operate outside box
# EJECT: LSE (SHELTRAN EJECT directive)
# UNTRANSLATED (L764): offset = 1
# FORTRAN DIRECTIVE: OR par = 1, totpars (SHELTRAN F directive)
# NOTE: Combin on all(!) data, blank later
# ALL GDSI_READ( setname(par),                        Fcwlo(par), Fcwhi(par),                        dataI(offset), numinreadbuf, numpixread,                        tids(par) )
# UNTRANSLATED (L768): offset = offset + numpixread
# FOR
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
ult = FIEDO(dataI, numpixread, dataO[0][out - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: out - 1, fid[out - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: out - 1)
# FOR
# NOTE: Blank the inside data
# UNTRANSLATED (L774): WHILE ( INSIDEPTR( bufptr, numpixin ) )
# NOTE: Then set to blank
# INCLUDE F (.NOT. transfer) (SHELTRAN I directive - manual include/translation needed if it's Python code)
# Label only: THEN
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
# ALL SETNFBLANK( dataO(bufptr+1, out), numpixin )
# FOR
# NOTE: Else if transfer is needed ...
# EJECT: LSE (SHELTRAN EJECT directive)
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
# ALL MOVER( dataT(bufptr+1), dataO(bufptr+1, out),                        numpixin )
# FOR
# IF
# WHILE
# IF
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
# NOTE: Write output data
# ALL GDSI_WRITE( setnameO(out), FcwloO(out), FcwhiO(out),                       dataO(1, out), numinreadbuf, numinreadbuf,                       tidsO(out) )
# NOTE: Find running min, max etc for this output
# ALL MINMAX3( dataO(1, out), numinreadbuf, minval(m, out),
# NOTE: set and subset and store it in arrays                    max
# FOR
owrite = stilltowrite - numinreadbuf
# UNTRANSLATED (L797): ratio  =   FLOAT( totpixels-stilltowrite ) / FLOAT(totpixels)
# UNTRANSLATED (L798): curval = ( FLOAT( m-1 ) + ratio ) / FLOAT( nsubsmax )
# ALL STABAR( 0.0, 1.0, curval )
# UNTRANSLATED (L800): UNTIL (stilltowrite .EQ. 0)
# FOR
# NOTE: min max probably changed ==> update min, max
# FORTRAN DIRECTIVE: OR out = 1, totouts (SHELTRAN F directive)
# ALL WMINMAX( setnameO(out), subsetsO(1, out),                minval(1, out), maxval(1, out), nblanks(1, out),                nsubsmax, remove )
# FOR
# NOTE: Exit HERMES
# ALL FINIS
# Label only: STOP
# EJECT: ND (SHELTRAN EJECT directive)
# EJECT: ***** COMBIN Technicalities ***** (SHELTRAN EJECT directive)
#      Options in GDSBOX:
# 
#      1 box may exceed subset size
#      2 default is in B..LO
#      4 default is in B..HI
#      8 box restricted to size defined in B..HI
#      These codes work additive.
#      The size of the input set will be copied to the output set.
#      It is possible to work on a smaller area. Therefore we need
#      the grid coordinates of the frame ('F') and
#      the coordinates of the user defined box ('B').
#      All output sets will get same coordinate system as first input set
#      The order of the indices of the two dim. arrays is important,
#      that is, if they are used in a subroutine call. The most rapidly
#      changing index is always the first.
