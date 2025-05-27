#                            COPYRIGHT (c) 1990
#                      Kapteyn Astronomical Institute
#                 University of Groningen, The Netherlands
#                            All Rights Reserved.
# 
# te 'synonym' files from contents of aid.syn:
# d.syn
# sub
# mul
# div
# add
# #<
# 
# #>            aid.dc1
# am:      AID
# se:      Selects one of the tasks SUB, MUL, ADD, or DIV
# ory:     COMBINATION
#          aid.shl
# r:       M. Vogelaar
# rds:
# ASK=     Select task: SUB/MUL/ADD/DIV:                       [SUB]
# iption:  The programs ADD, SUB, MUL, and DIV perform the respectiv
#               arithmetic operations of addition, subtraction, multipli-
#               cation, and division. The first operand is a series of
#               subsets and the second operand is one subset or another
#               series of subsets. The programs are similar in structure.
#               They start with requesting an input set and a series of
#               subsets (INSET=). Then, a frame can be given for which
#               the operation applies to the input subsets (BOX=). Next,
#               the second operand is asked for (SETX=). If needed, the
#               specification of a frame is requested (BOXX=). Furthermor
#               an output set is requested (OUTSET=).
# 
#               To start one of these programs, give the appropriate
#               program name.
# ks:      To get corresponding help info in Xhermes, select one
#               of the tasks SUB, MUL, ADD, or DIV instead of AID.
# 
# es:      Feb 22, 1990: MV, Document created.
# 
# #<
# 
# #>            sub.dc1
# am:      SUB
# se:      Subtract one or a series of subsets from another
#               series of subsets.
# ory:     COMBINATION
#          aid.shl
# r:       M. Vogelaar
# rds:
# NSET=    Input set (and subsets). Maximum number of subsets
#               is 2048.
# OX=      Frame for input subsets:                   [entire subset
# 
# ETX=     Set name and subset(s) which operate on the input set.
#               SUB subtracts subsets in two different modes:
#               A: One 'SETX' subset works one all 'SET' subsets
#               B: All given 'SETX' subsets work pairwise on all
#                  subsets of 'SET'
# 
#               Depending on the number of user given 'SETX' subsets,
#               the program selects the mode of operation.
# OXX=     The axis sizes of the subset box of 'SET' have to be
#               equal to the axis sizes of the subset box of 'SETX'.
#               If using comparable subsets (i.e. same axis names,
#               same grids and origins), this keyword is hidden.
# UTSET=   Output set and subset(s) for the result.
#               The number of output subsets is the same as the
#               number of input subsets.
# es:      Feb 22, 1990: MV, Document created.
# 
# #<
# 
# #>            add.dc1
# am:      ADD
# se:      Add a series of input subsets and one or another
#               series of subsets.
# ory:     COMBINATION
#          aid.shl
# r:       M. Vogelaar
# rds:
# NSET=    Input set (and subsets). Maximum number of subsets
#               is 2048.
# OX=      Frame for input subsets.            [entire subset]
# ETX=     Set name and subset(s) which operate on the input set.
#               ADD adds subsets in two different modes:
#               A: One 'SETX' subset works on all 'SET' subsets
#               B: All given 'SETX' subsets work pairwise on all
#                  subsets of 'SET'
# 
#               Depending on the number of user given 'SETX' subsets,
#               the program selects the mode of operation.
# OXX=     The axis sizes of the subset box of 'SET' have to be
#               equal to the axis sizes of the subset box of 'SETX'.
#               If using comparable subsets (i.e. same axis names,
#               same grids and origins), this keyword is hidden.
# UTSET=   Output set and subset(s) for the result.
#               The number of output subsets is the same as the
#               number of input subsets.
# es:      Feb 22, 1990: MV, Document created.
# 
# #<
# 
# #>            mul.dc1
# am:      MUL
# se:      Multiplies a series of input subsets with one or
#               another series of subsets.
# ory:     COMBINATION
#          aid.shl
# r:       M. Vogelaar
# rds:
# NSET=    Input set (and subsets). Maximum number of subsets
#               is 2048.
# OX=      Frame for input subsets.            [entire subset]
# ETX=     Set name and subset(s) which operate on the input set.
#               MUL multiplies subsets in two different modes:
#               A: One 'SETX' subset works one all 'SET' subsets
#               B: All given 'SETX' subsets work pairwise on all
#                  subsets of 'SET'
# 
#               Depending on the number of user given 'SETX' subsets,
#               the program selects the mode of operation.
# OXX=     The axis sizes of the subset box of 'SET' have to be
#               equal to the axis sizes of the subset box of 'SETX'.
#               If using comparable subsets (i.e. same axis names,
#               same grids and origins), this keyword is hidden.
# UTSET=   Output set and subset(s) for the result.
#               The number of output subsets is the same as the
#               number of input subsets.
# es:      Feb 22, 1990: MV, Document created.
# 
# #<
# 
# #>            div.dc1
# am:      DIV
# se:      Divides a series of input subsets by one or another
#               series of subsets.
# ory:     COMBINATION
#          aid.shl
# r:       M. Vogelaar
# rds:
# NSET=    Input set (and subsets). Maximum number of subsets
#               is 2048.
# OX=      Frame for input subsets.            [entire subset]
# ETX=     Set name and subset(s) which operate on the input set.
#               DIV divides subsets in two different modes:
#               A: One 'SETX' subset works one all 'SET' subsets
#               B: All given 'SETX' subsets work pairwise on all
#                  subsets of 'SET'
# 
#               Depending on the number of user given 'SETX' subsets,
#               the program selects the mode of operation.
# OXX=     The axis sizes of the subset box of 'SET' have to be
#               equal to the axis sizes of the subset box of 'SETX'.
#               If using comparable subsets (i.e. same axis names,
#               same grids and origins), this keyword is hidden.
# UTSET=   Output set and subset(s) for the result.
#               The number of output subsets is the same as the
#               number of input subsets.
# es:      Feb 22, 1990: MV, Document created.
# 
# #<
# es:      Feb 28, 1990: MV, Document created.
#               Jun 30, 1993: MV, TID's in arrays, usertext replaced
#                                 by usercharu
# 
# #<
# 
# EJECT: ***** Program SUB *****
# UNTRANSLATED (L170): PROGRAM SUB
#      Declaration of parameters:
# HARACTER*(*)  ident
# UNTRANSLATED (L173): PARAMETER    ( ident = ' Version 1.1  Oct 31, 1994 ' )
# INCLUDE: NTEGER        maxsubsets
# UNTRANSLATED (L175): PARAMETER    ( maxsubsets = 2048 )
# INCLUDE: NTEGER        maxaxes
# UNTRANSLATED (L177): PARAMETER    ( maxaxes = 10 )
# NOTE: Default options for use in USERxxx() routines
# INCLUDE: NTEGER        none, request, hidden
# UNTRANSLATED (L180): PARAMETER    ( none    = 0 )
# UNTRANSLATED (L181): PARAMETER    ( request = 1 )
# UNTRANSLATED (L182): PARAMETER    ( hidden  = 2 )
# NOTE: Buffer size for I/O
# INCLUDE: NTEGER        maxIObuf
# UNTRANSLATED (L185): PARAMETER    ( maxIObuf = 4096 )
# NOTE: Remove with 'WMINMAX' old minmax descriptors
# INCLUDE: NTEGER        remove
# UNTRANSLATED (L188): PARAMETER    ( remove = 1 )
#      Declarations for GDSINP:
# INCLUDE: NTEGER        GDSINP
# NOTE: Array containing subset coordinate words
# INCLUDE: NTEGER        subsets1( maxsubsets ), subsetsX( maxsubsets )
# NOTE: Number of subsets in first and second set
# INCLUDE: NTEGER        nsubs1, nsubsX
# INCLUDE: NTEGER        dfault
# NOTE: Number of output device [0..16]
# INCLUDE: NTEGER        devicenum
# HARACTER*80   setname1, setnameX
# HARACTER*10   keyword
# HARACTER*40   message
# INCLUDE: NTEGER        axperm1(  maxaxes ), axpermX(  maxaxes )
# INCLUDE: NTEGER        axcount1( maxaxes ), axcountX( maxaxes )
# INCLUDE: NTEGER        class
# INCLUDE: NTEGER        dimofsubsets
#      Declarations for GDSBOX:
# NOTE: Grid vectors of sub frames
# INCLUDE: NTEGER        BedgeLO1( maxaxes ), BedgeHI1( maxaxes )
# INCLUDE: NTEGER        BedgeLOX( maxaxes ), BedgeHIX( maxaxes )
# NOTE: Grid vectors of entire frames
# INCLUDE: NTEGER        FedgeLO1( maxaxes ), FedgeHI1( maxaxes )
# INCLUDE: NTEGER        FedgeLOX( maxaxes ), FedgeHIX( maxaxes )
# INCLUDE: NTEGER        FedgeLOO( maxaxes ), FedgeHIO( maxaxes )
# NOTE: What's the default in GDSBOX
# INCLUDE: NTEGER        option
#      Declarations for GDSOUT:
# INCLUDE: NTEGER        GDSOUT
# INCLUDE: NTEGER        nsubsO
# INCLUDE: NTEGER        subsetsO( maxsubsets )
# INCLUDE: NTEGER        axpermO(  maxaxes )
# INCLUDE: NTEGER        axcountO( maxaxes )
# HARACTER*80   setnameO
#      Functions:
# NOTE: Returns dimension of a set
# INCLUDE: NTEGER        GDSC_NDIMS
# NOTE: Extracts grid coordinate from coord. word
# INCLUDE: NTEGER        GDSC_GRID
# NOTE: Returns coordinate word
# INCLUDE: NTEGER        GDSC_FILL
# NOTE: Returns .TRUE. if inside defined sub frame
# UNTRANSLATED (L230): LOGICAL        OUTSIDEPTR
# NOTE: Returns .TRUE. if outside defined sub frame
# UNTRANSLATED (L232): LOGICAL        INSIDEPTR
# NOTE: Determine length of string
# INCLUDE: NTEGER        NELC
# NOTE: Obtain the name under which a GIPSY task runs
# HARACTER*9    MYNAME
#      Variables for the algorithms:
# INCLUDE: NTEGER        m
# NOTE: There are totpixels in one subset
# INCLUDE: NTEGER        totpixels
# NOTE: Transfer identifications for read/write
# INCLUDE: NTEGER        tidX( maxsubsets )
# NOTE: Array version of transfer id's
# INCLUDE: NTEGER        tids1(  maxsubsets )
# INCLUDE: NTEGER        tidsO(  maxsubsets )
# INCLUDE: NTEGER        mcount( maxsubsets )
# INCLUDE: NTEGER        ptrcount
# INCLUDE: NTEGER        stilltowrite
# NOTE: Coordinate words for total & sub frame
# INCLUDE: NTEGER        Fcwlo1, Fcwhi1
# INCLUDE: NTEGER        BcwloX( maxsubsets ), BcwhiX( maxsubsets )
# NOTE: Array version of Bcwlo1, Bcwhi1
# INCLUDE: NTEGER        Bcwlo1( maxsubsets ), Bcwhi1( maxsubsets )
# NOTE: Array version of FcwloO, FcwhiO
# INCLUDE: NTEGER        FcwloO( maxsubsets ), FcwhiO( maxsubsets )
# NOTE: For use in INSIDEPTR/OUTSIDEPTR
# INCLUDE: NTEGER        bufptr
# NOTE: Length of the pointer buffer
# INCLUDE: NTEGER        ptrbuflen
# INCLUDE: NTEGER        numinreadbuf
# NOTE: # of pixels inside ptr buf. & sub frame
# INCLUDE: NTEGER        totinside
# NOTE: Number of elements inside/outside sub frame
# INCLUDE: NTEGER        numin
# INCLUDE: NTEGER        numout
# INCLUDE: NTEGER        pixelsdone
# INCLUDE: NTEGER        start
# NOTE: Number of elements already calculated
# INCLUDE: NTEGER        done
# INCLUDE: NTEGER        numpixels1, numpixelsX
# NOTE: How far we proceeded, used in mode A
# INCLUDE: NTEGER        curval
# NOTE: For updating number of blanks
# INCLUDE: NTEGER        nblanks( maxsubsets )
# NOTE: For updating minimum and maximum of subset
# UNTRANSLATED (L276): REAL           minval(  maxsubsets  ), maxval( maxsubsets )
# UNTRANSLATED (L277): REAL           data1(   maxIObuf  ), dataX(  maxIObuf )
# UNTRANSLATED (L278): REAL           dataO(   maxIObuf )
# INCLUDE: NTEGER        R1
# INCLUDE: NTEGER        USERREAL
#      Miscellaneous:
# INCLUDE: NTEGER        dimofset
# NOTE: Error codes for Range and Grid routines
# INCLUDE: NTEGER        Rerror, Gerror
# NOTE: Coordinate words to determine total range
# INCLUDE: NTEGER        cwlo, cwhi
# NOTE: One of the names SUB, ADD, MUL or DIV
# HARACTER*9    task
# NOTE: Arithmetic operator (+,-,* or /)
# HARACTER*1    operator
# NOTE: Is .TRUE. if one 'SETX' subset was given
# UNTRANSLATED (L292): LOGICAL        onesubsx
# NOTE: True when grids of input edges are equal
# UNTRANSLATED (L294): LOGICAL        equalgrids
# UNTRANSLATED (L295): LOGICAL        agreed, ok
# UNTRANSLATED (L296): ce the five programs SUB, ADD MUL and DIV use essentially same code this program was created to avoid overlap.
#   It is supplied by four other entry points: MUL, DIV, ADD and SUB. entry (=taskname) determines which operationl be executed on the inputs. ANYOUT, device number 8 is selected. It is possible to disableput with HERMES' switch /MODE 0.
#   +1 is output to terminal.
#   +2 is output to HERMES' logfile
# EJECT: Main
# ALL INIT
# NOTE: Get task name
# UNTRANSLATED (L303): task = MYNAME()
# INCLUDE: F (task(1:3) .EQ. 'AID')
# Label only: THEN
# UNTRANSLATED (L306): dfault = request
# UNTRANSLATED (L307): task = 'SUB'
# EJECT: LSE
# UNTRANSLATED (L309): dfault = hidden
# IF
# Label only: REPEAT
# ALL USERCHARU( task, 1, dfault, 'TASK=',                'SUB/MUL/ADD/DIV:     [SUB]' )
# UNTRANSLATED (L313): ok = ( (task .EQ. 'SUB') .OR.         (INDEX( task, 'SUB_') .EQ. 1) .OR.         (task .EQ. 'MUL') .OR.         (INDEX( task, 'MUL_') .EQ. 1) .OR.         (task .EQ. 'DIV') .OR.         (INDEX( task, 'DIV_') .EQ. 1) .OR.         (task .EQ. 'ADD') .OR.         (INDEX( task, 'ADD_') .EQ. 1) )
# INCLUDE: F .NOT. ok
# Label only: THEN
# ALL REJECT( 'TASK=', 'Unknown task!' )
# UNTRANSLATED (L317): dfault = request
# UNTRANSLATED (L318): task = 'SUB'
# IF
# UNTRANSLATED (L320): UNTIL ok
# NOTE: Show task and version number
# ALL ANYOUT( 8, ( task( :NELC(task) ) // ident) )
# UNTRANSLATED (L323): operator = '-'
# NOTE: Task is ADD: addition
# INCLUDE: F task .EQ. 'ADD'
# Label only: THEN
# UNTRANSLATED (L327): operator = '+'
# NOTE: Task is MUL: multiplication
# EJECT: LSEIF task .EQ. 'MUL'
# Label only: THEN
# UNTRANSLATED (L331): operator = '*'
# NOTE: Task is DIV: division
# EJECT: LSEIF task .EQ. 'DIV'
# Label only: THEN
# UNTRANSLATED (L335): operator = '/'
# IF
# NOTE: Prepare for GDSINP for first set
# UNTRANSLATED (L338): dfault       = none
# UNTRANSLATED (L339): keyword      ='INSET='
# UNTRANSLATED (L340): message      ='Give input set(subsets) to work on:'
# NOTE: Application repeats operation for each subset
# UNTRANSLATED (L342): class        = 1
# UNTRANSLATED (L343): dimofsubsets = 0
# UNTRANSLATED (L344): devicenum    = 3
# UNTRANSLATED (L345): nsubs1 = GDSINP( setname1, subsets1, maxsubsets, dfault,                 keyword, message, devicenum, axperm1,                 axcount1, maxaxes, class,                 dimofsubsets )
# NOTE: Determine the edges of this set
#      The size of the input set will be copied to the output set.
#      It is possible to work on a smaller area. Therefore we need
#      the grid coordinates of the frame ('Fedge') and
#      the coordinates of the user defined box ('Bedge').
# UNTRANSLATED (L351): Rerror = 0
# UNTRANSLATED (L352): Gerror = 0
# UNTRANSLATED (L353): totpixels = 1
# ALL GDSC_RANGE( setname1, 0, cwlo, cwhi, Rerror )
# FORTRAN DIRECTIVE: OR m = 1, dimofsubsets
# FORTRAN DIRECTIVE: edgeLO1(m) = GDSC_GRID( setname1, axperm1(m), cwlo, Gerror )
# FORTRAN DIRECTIVE: edgeHI1(m) = GDSC_GRID( setname1, axperm1(m), cwhi, Gerror )
# NOTE: Calculate number of pixels per subset
# UNTRANSLATED (L359): totpixels   = totpixels * ( FedgeHI1(m) - FedgeLO1(m) + 1 )
# FOR
#      Options in GDSBOX:
#      1 box may exceed subset size
#      2 default is in BLO
#      4 default is in BHI
#      8 box restricted to size defined in BHI
#      These codes work additive.
# NOTE: Prepare for a frame for first set
# UNTRANSLATED (L368): dfault    = request
# UNTRANSLATED (L369): keyword   = 'BOX='
# UNTRANSLATED (L370): message   = ' '
# NOTE: Default is entire subset
# UNTRANSLATED (L372): option    = 0
# UNTRANSLATED (L373): devicenum = 3
# ALL GDSBOX( BedgeLO1, BedgeHI1, setname1, subsets1,             dfault, keyword, message, devicenum, option )
# NOTE: Prepare for GDSINP for second set
# UNTRANSLATED (L376): dfault = request
# NOTE: Application repeats operation for each subset
# UNTRANSLATED (L378): class  = 1dimofsubsets' has value of previous GDSINP
# UNTRANSLATED (L379): message = 'Enter mask set(subsets):'
# Label only: REPEAT
# UNTRANSLATED (L381): onesubsx  =.TRUE.
# UNTRANSLATED (L382): keyword   ='SETX='
# NOTE: nsubsX <= nsubsX
# UNTRANSLATED (L384): devicenum = 3
# UNTRANSLATED (L385): nsubsX     = GDSINP( setnameX, subsetsX, nsubs1,                       dfault, keyword,                       message, devicenum, axpermX,                       axcountX, maxaxes, class,                       dimofsubsets )
# INCLUDE: F ( nsubsX .EQ. nsubs1 ) .OR.     ( nsubsX .EQ. 1 )
# Label only: THEN
# UNTRANSLATED (L388): agreed = .TRUE.
# INCLUDE: F ( nsubsX .NE. 1 )
# Label only: THEN
# NOTE: Then operate in mode B
# UNTRANSLATED (L392): onesubsx = .FALSE.
# IF
# EJECT: LSE
# NOTE: Output to terminal only
# UNTRANSLATED (L396): devicenum = 1
# UNTRANSLATED (L397): agreed  = .FALSE.
# UNTRANSLATED (L398): message =' Number of subsets has to be equal to 1'
# ALL ANYOUT( devicenum, message )
# UNTRANSLATED (L400): message = ' or eq. to first number of input subsets'
# ALL ANYOUT( devicenum, message )
# ALL CANCEL( keyword )
# UNTRANSLATED (L403): message = ' Try again; set, subsets: '
# IF
# UNTRANSLATED (L405): UNTIL agreed
# UNTRANSLATED (L406): Rerror = 0
# UNTRANSLATED (L407): Gerror = 0
# ALL GDSC_RANGE( setnameX, 0, cwlo, cwhi, Rerror )
# FORTRAN DIRECTIVE: OR m = 1, dimofsubsets
# FORTRAN DIRECTIVE: edgeLOX(m) = GDSC_GRID( setnameX, axpermX(m), cwlo, Gerror )
# FORTRAN DIRECTIVE: edgeHIX(m) = GDSC_GRID( setnameX, axpermX(m), cwhi, Gerror )
# FOR
# NOTE: Are first and second subsets comparable?
# UNTRANSLATED (L414): equalgrids = .TRUE.
# FORTRAN DIRECTIVE: OR m = 1, dimofsubsets
# UNTRANSLATED (L416): equalgrids = ( equalgrids .AND.                 ( FedgeLOX(m) .EQ. FedgeLO1(m) ) .AND.                 ( FedgeHIX(m) .EQ. FedgeHI1(m) )               )
# FOR
# NOTE: Prepare for a frame for second set
# INCLUDE: F equalgrids
# Label only: THEN
# NOTE: Fill arrays with GRIDS for GDSBOX
# FORTRAN DIRECTIVE: OR m = 1, dimofsubsets
# UNTRANSLATED (L423): BedgeHIX(m) = BedgeHI1(m)
# UNTRANSLATED (L424): BedgeLOX(m) = BedgeLO1(m)
# FOR
# UNTRANSLATED (L426): dfault = hidden
# NOTE: Defaults in BedgeHIX/LOX
# UNTRANSLATED (L428): option = 6
# EJECT: LSE
# NOTE: Fill arrays with SIZES for GDSBOX
# FORTRAN DIRECTIVE: OR m = 1, dimofsubsets
# UNTRANSLATED (L432): BedgeHIX(m) = BedgeHI1(m) - BedgeLO1(m) + 1
# FOR
# UNTRANSLATED (L434): dfault = request
# NOTE: GDSBOX has a special option for sizes
# UNTRANSLATED (L436): option = ( 8 + 4 )
# IF
# UNTRANSLATED (L438): keyword   ='BOXX='
# UNTRANSLATED (L439): message   =' '
# UNTRANSLATED (L440): devicenum = 3
# NOTE: Box restricted to size defined in BedgeHI
# ALL GDSBOX( BedgeLOX, BedgeHIX, setnameX, subsetsX,             dfault, keyword, message, devicenum, option )
# NOTE: Assign GDSINP buffer to GDSOUT
# NOTE: Output set will get same coordinate-
# NOTE: system as first input set
# UNTRANSLATED (L446): keyword  ='OUTSET='
# UNTRANSLATED (L447): dimofset = GDSC_NDIMS( setname1, 0 )
# ALL GDSASN( 'INSET=', keyword, class )
# UNTRANSLATED (L449): dfault    = none
# UNTRANSLATED (L450): message   ='Give output set (and subsets):'
# UNTRANSLATED (L451): devicenum = 3
# UNTRANSLATED (L452): nsubsO    = GDSOUT( setnameO, subsetsO, nsubs1,                    dfault, keyword, message, devicenum,                    axpermO, axcountO, maxaxes )
# UNTRANSLATED (L453): Rerror = 0
# UNTRANSLATED (L454): Gerror = 0
# ALL GDSC_RANGE( setnameO, 0, cwlo, cwhi, Rerror )
# FORTRAN DIRECTIVE: OR m = 1, dimofsubsets
# FORTRAN DIRECTIVE: edgeLOO(m) = GDSC_GRID( setnameO, axpermO(m), cwlo, Gerror )
# FORTRAN DIRECTIVE: edgeHIO(m) = GDSC_GRID( setnameO, axpermO(m), cwhi, Gerror )
# FOR
# NOTE: cw=coordinate word, lo=lower, hi=upper
# NOTE: F=Frame, B=Box
# FORTRAN DIRECTIVE: OR m = 1, nsubsO
# UNTRANSLATED (L463): tids1( m) = 0
# UNTRANSLATED (L464): tidsO( m) = 0
# UNTRANSLATED (L465): tidX(  m) = 0
# UNTRANSLATED (L466): mcount(m) = 0
# UNTRANSLATED (L467): Bcwlo1(m) = GDSC_FILL( setname1, subsets1(m), BedgeLO1 )
# UNTRANSLATED (L468): Bcwhi1(m) = GDSC_FILL( setname1, subsets1(m), BedgeHI1 )
# FORTRAN DIRECTIVE: cwloO(m) = GDSC_FILL( setnameO, subsetsO(m), FedgeLOO )
# FORTRAN DIRECTIVE: cwhiO(m) = GDSC_FILL( setnameO, subsetsO(m), FedgeHIO )
# INCLUDE: F onesubsx
# Label only: THEN
# UNTRANSLATED (L473): BcwloX(m) = GDSC_FILL( setnameX, subsetsX(1), BedgeLOX )
# UNTRANSLATED (L474): BcwhiX(m) = GDSC_FILL( setnameX, subsetsX(1), BedgeHIX )
# EJECT: LSE
# UNTRANSLATED (L476): BcwloX(m) = GDSC_FILL( setnameX, subsetsX(m), BedgeLOX )
# UNTRANSLATED (L477): BcwhiX(m) = GDSC_FILL( setnameX, subsetsX(m), BedgeHIX )
# IF
# FOR
# UNTRANSLATED (L480): ptrcount  = 0
# UNTRANSLATED (L481): stilltowrite = totpixels
# Label only: REPEAT
# NOTE: Update needed for STABAR
# UNTRANSLATED (L484): curval = totpixels - stilltowrite
# NOTE: Bufferlength never exceeds len. of write buf.
# UNTRANSLATED (L486): ptrbuflen = MIN( maxIObuf, stilltowrite )
# UNTRANSLATED (L487): stilltowrite  = stilltowrite - ptrbuflen
# NOTE: Only scan in the first subset of SET
# ALL INITPTR( FedgeLO1, FedgeHI1, BedgeLO1, BedgeHI1,                dimofsubsets, ptrbuflen, ptrcount )
# UNTRANSLATED (L490): totinside = 0
# UNTRANSLATED (L491): WHILE ( INSIDEPTR( bufptr, numin ) )
# NOTE: Find # in pointer buffer & inside subframe
# UNTRANSLATED (L493): totinside = totinside + numin
# WHILE
# FORTRAN DIRECTIVE: OR m = 1, nsubsO
# NOTE: Prepare a message about how far we proceeded
# ALL STABAR( 0.0, FLOAT( totpixels ),                 FLOAT( curval + ( m/nsubsO ) * ptrbuflen ) )
# UNTRANSLATED (L498): WHILE ( OUTSIDEPTR( bufptr, numout ) )
# NOTE: Fill array with blanks
# ALL SETNFBLANK( dataO( bufptr+1 ), numout )
# WHILE
# INCLUDE: F ( totinside .GT. 0 )
# Label only: THEN
# ALL GDSI_READ( setname1, Bcwlo1(m), Bcwhi1(m),                      data1, totinside, totinside, tids1(m) )
# ALL GDSI_READ( setnameX, BcwloX(m), BcwhiX(m),                      dataX, totinside, totinside, tidX(m) )
# UNTRANSLATED (L506): start = 1
# UNTRANSLATED (L507): WHILE ( INSIDEPTR( bufptr, numin ) )
# NOTE: bufptr is an offset, start is an index!
# ALL ARITH( data1( start ), operator,                    dataX( start ), dataO( bufptr+1 ), numin )
# UNTRANSLATED (L510): start = start + numin
# WHILE
# IF
# UNTRANSLATED (L513): pixelsdone = ptrbuflen
# ALL GDSI_WRITE( setnameO, FcwloO(m), FcwhiO(m),                     dataO, ptrbuflen, pixelsdone, tidsO(m) )
# NOTE: For all ssets, you have to store the counter
# ALL MINMAX3( dataO, ptrbuflen, minval(m), maxval(m),                  nblanks(m), mcount(m) )
# FOR
# UNTRANSLATED (L518): UNTIL ( stilltowrite .EQ. 0 )
# NOTE: min max always change, ==> update min, max
# ALL WMINMAX( setnameO, subsetsO, minval, maxval, nblanks,              nsubsO, remove )
# ALL FINIS
# Label only: STOP
# EJECT: ND
