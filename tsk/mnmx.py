# EJECT: PROGRAM MNMX (copyright notice).shl (SHELTRAN EJECT directive)
#                            COPYRIGHT (c) 1990
#          Kapteyn Astronomical Institute - University of Groningen
#             P.O. box 800,  9700 AV Groningen, The Netherlands
# 
# EJECT: PROGRAM MNMX (mnmx.dc1) (SHELTRAN EJECT directive)
# #>            mnmx.dc1
# am:      MNMX
# se:      This program finds the minimum and maximum value in a
#               subset or part of a subset. It also counts the number
#               of blanks.
# ory:     CALCULATION, HEADER, MANIPULATION
#          mnmx.shl
# r:       K.G. Begeman
# rds:
# SET=     Set (and subset(s)) for which to determine the minimum
#               and maximum. Maximum number of subsets is 2048.
# X=       Area to be searched for minimum and maximum [entire subse
# FILE=    Write result table to file?  Y/[N]
#               Hidden keyword. Is set to Y the result table is written
#               to a file on disk in the directory where GIPSY is
#               started. The file has a fixed name. That name is _mnmxtmp
# :        1. MNMX updates the descriptor items DATAMIN, DATAMAX and
#               NBLANK only when working on whole subsets.
#               2. If subsets have zero dimension, i.e. if they are
#               pixels, then MNMX will display the pixel values.
# le:      <USER  >MNMX
#               <USER  >MNMX     ,INSET=AURORA RA -3:4 DEC 0 FREQ 32
#               Set AURORA has 3 axes
#               RA-NCP             from    -7 to     8
#               DEC-NCP            from    -7 to     8
#               FREQ-OHEL          from     1 to    59
#               (   RA,  DEC, FREQ)  DATA VALUE
#               (   -3,    0,   32)  -4.0723352
#               (   -2,    0,   32)  -2.8402100
#               (   -1,    0,   32)  -3.8706512
#               (    0,    0,   32)   5.0316072
#               (    1,    0,   32)  -11.610437
#               (    2,    0,   32)  -6.4864664
#               (    3,    0,   32)   4.0308046
#               (    4,    0,   32)   3.9248881
#               <STATUS>  MNMX    +++ FINISHED +++
# es:      Jan 29, 1990: KGB, Document created.
#               Jun 25, 1997: VOG, Hidden keyword CHANGE= added
#                                  Keyword is undocumented and for
#                                  special purposes only.
#               Feb 25, 2013: VOG, Increased filename length to 1024
#               Nov 10, 2014: VOG, Added keyword TOFILE= to facilitate
#                                  output in GUIPSY
# 
# #<
# EJECT: PROGRAM MNMX (code) (SHELTRAN EJECT directive)
# UNTRANSLATED (L53): program mnmx
# 
#      Parameters:
# 
# UNTRANSLATED (L57): character*(*) ident
# NOTE: Change version number on this line
# UNTRANSLATED (L59): parameter (ident = ' MNMX  Version 1.0  Jan 29, 1990 ')
# UNTRANSLATED (L60): integer       maxaxes
# NOTE: Maximum number of axes in set
# UNTRANSLATED (L62): parameter (maxaxes = 10)
# UNTRANSLATED (L63): integer       maxsubs
# NOTE: Maximum number of subsets
# UNTRANSLATED (L65): parameter (maxsubs = 2048)
# UNTRANSLATED (L66): integer       maxbuf
# NOTE: Size of data array
# UNTRANSLATED (L68): parameter (maxbuf = 4096)
# UNTRANSLATED (L69): integer       outdev
# NOTE: Device on which to display info
# UNTRANSLATED (L71): parameter (outdev = 11)
# 
#      Declarations for input set:
# 
# NOTE: Array with axis names
# UNTRANSLATED (L76): character*18  axname(maxaxes)
# NOTE: Name of input set
# UNTRANSLATED (L78): character*1024  set
# NOTE: Permutation array of axes numbers
# UNTRANSLATED (L80): integer       axperm(maxaxes)
# NOTE: Array with sizes of subset axis
# UNTRANSLATED (L82): integer       axsize(maxaxes)
# NOTE: Coordinate Words of choosen frame
# UNTRANSLATED (L84): integer       cwlo, cwhi
# NOTE: Number of input subsets
# UNTRANSLATED (L86): integer       nsub
# NOTE: Array with subset coordinate words
# UNTRANSLATED (L88): integer       subset(maxsubs)
# NOTE: Dimension of input set
# UNTRANSLATED (L90): integer       setdim
# NOTE: Dimension of input subsets
# UNTRANSLATED (L92): integer       subdim
# NOTE: Transfer id input data
# UNTRANSLATED (L94): integer       tid
# 
#      Declaration of local variables:
# 
# NOTE: Labels for axes
# UNTRANSLATED (L99): character*5   axlab(maxaxes)
# NOTE: Arrays containing choosen frame
# UNTRANSLATED (L101): integer       blo(maxaxes), bhi(maxaxes)
# NOTE: Grid counter
# UNTRANSLATED (L103): integer       g
# NOTE: Various GDS error returns
# UNTRANSLATED (L105): integer       gerror
# NOTE: Arrays containing min and max position
# UNTRANSLATED (L107): integer       gmin(maxaxes), gmax(maxaxes)
# NOTE: Position of minimum and maximum
# UNTRANSLATED (L109): integer       imin, imax
# NOTE: Length of axis name
# UNTRANSLATED (L111): integer       l
# NOTE: Loop counter
# UNTRANSLATED (L113): integer       m
# NOTE: Counter  for minmax4
# UNTRANSLATED (L115): integer       mcount
# NOTE: Loop counter
# UNTRANSLATED (L117): integer       n
# NOTE: Number of blanks in subsets
# UNTRANSLATED (L119): integer       nblank(maxsubs)
# NOTE: Number of points actually read from subset
# UNTRANSLATED (L121): integer       nd
# NOTE: Array containing number of pixels (dimension)
# UNTRANSLATED (L123): integer       nf(maxaxes)
# NOTE: Number of points on axis inside frame
# UNTRANSLATED (L125): integer       np
# NOTE: Subset counter
# UNTRANSLATED (L127): integer       ns
# NOTE: Total number of pixels in subset
# UNTRANSLATED (L129): integer       ntotal
# NOTE: For output formatting
# UNTRANSLATED (L131): integer       tabs(6)
# NOTE: Logical indicating whether header update is possible
# UNTRANSLATED (L133): logical       change
# NOTE: Logical for writing to file _mnmxtmp_
# UNTRANSLATED (L135): logical       tofile
# NOTE: Array which recieves the data
# UNTRANSLATED (L137): real          buf(maxbuf)
# NOTE: Minimum and maximum value in subsets
# UNTRANSLATED (L139): real          datamin(maxsubs), datamax(maxsubs)
# 
#      Functions:
# 
# NOTE: Returns name of axis
# UNTRANSLATED (L144): character*18  gdsc_name
# NOTE: Returns number of input subsets
# UNTRANSLATED (L146): integer       gdsinp
# NOTE: Returns coordinate word
# UNTRANSLATED (L148): integer       gdsc_fill
# NOTE: Returns grid coordinate from coordinate word
# UNTRANSLATED (L150): integer       gdsc_grid
# NOTE: Returns number of dimensions in coordinate word
# UNTRANSLATED (L152): integer       gdsc_ndims
# NOTE: Returns number of characters in string
# UNTRANSLATED (L154): integer       nelc
# NOTE: integer       userlog
# 
#      Data statements:
# 
# NOTE: Default change descriptors
# UNTRANSLATED (L160): data change / .true. /
# NOTE: Write to file or not
# UNTRANSLATED (L162): data tofile / .false. /
# NOTE: Various GDS error returns
# UNTRANSLATED (L164): data gerror / 0 /
# NOTE: Total number of pixels
# UNTRANSLATED (L166): data ntotal / 1 /
# NOTE: Dimension of subset
# UNTRANSLATED (L168): data subdim / 0 /
# 
#      Executable statements:
# 
# NOTE: contact HERMES
# UNTRANSLATED (L173): call init
# NOTE: tell user who we are
# UNTRANSLATED (L175): call anyout( 8, ident )
# NOTE: get input database
# UNTRANSLATED (L177): nsub = gdsinp( set,               subset,               maxsubs,               0,               'INSET=',               'Set (and subset(s)) to work on',               outdev,               axperm,               axsize,               maxaxes,               1,               subdim )
# NOTE: Get number of axis
# UNTRANSLATED (L179): setdim = gdsc_ndims( set, 0 )
# NOTE: Get axis names and prepare axis lables
# UNTRANSLATED (L181): for m = 1, setdim
# NOTE: Get name of axis
(m) = GDSC_NAME(set, axperm[m - 1] # REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based. # REVIEW INDEX: Verify subtraction for 0-based: m - 1, gerror)
# NOTE: Reset label
# UNTRANSLATED (L185): axlab(m) = ' '
# NOTE: Length of axis name
# UNTRANSLATED (L187): l = nelc( axname(m) )
# NOTE: Position of hyphen in axis name
# UNTRANSLATED (L189): n = index( axname(m), '-' )
# NOTE: Hyphen found, so ...
# UNTRANSLATED (L191): if (n .gt. 0)
# Label only: then
# NOTE: ... set number of characters to display
# UNTRANSLATED (L194): l = n - 1
# Label only: cif
# NOTE: When too large, truncate axis name
# UNTRANSLATED (L197): l = min( 5, l )
# NOTE: Now do some right adjusting
# UNTRANSLATED (L199): axlab(m)(5-l+1:) = axname(m)(:l)
# Label only: cfor
# NOTE: get area of subset to work on (hidden keyword)
# UNTRANSLATED (L202): call gdsbox( blo,             bhi,             set,             subset,             2,             ' ',             ' ',             outdev,             0 )
# NOTE: get size of subset
# UNTRANSLATED (L204): for n = 1, subdim
# NOTE: Store it here for later use
# UNTRANSLATED (L206): nf(n) = ntotal
# NOTE: Number of pixels along nth axis
# UNTRANSLATED (L208): np = bhi(n) - blo(n) + 1
# NOTE: Add to total, that is, multiply
# UNTRANSLATED (L210): ntotal = ntotal * np
# NOTE: Determine whether header update is possible
# UNTRANSLATED (L212): change = ((change) .and. (np .eq. axsize(n)))
# Label only: cfor
# UNTRANSLATED (L214): if (.not. change)
# Label only: then
# UNTRANSLATED (L216): call userlog( change, 1, 2, 'CHANGE=',                'Update header anyhow?  Y/[N]' )
# Label only: cif
# UNTRANSLATED (L218): call userlog(tofile, 1, 2, 'TOFILE=',                'Write result table to file?   Y/[N]')
# UNTRANSLATED (L219): if (tofile)
# Label only: then
nit = 7, file='_mnmxtmp_')
# Label only: cif
# NOTE: print column head and determine format
# UNTRANSLATED (L224): call heading( outdev,              setdim,              subdim,              axlab,              tabs,              tofile)
# NOTE: loop through subsets
# UNTRANSLATED (L226): for ns = 1, nsub
# NOTE: show user what we are working on
# UNTRANSLATED (L228): call showsub1( set, subset(ns), axperm )
# NOTE: Get lower coordinate word of frame
# UNTRANSLATED (L230): cwlo = gdsc_fill( set, subset(ns), blo )
# NOTE: Get upper coordinate word of frame
# UNTRANSLATED (L232): cwhi = gdsc_fill( set, subset(ns), bhi )
# NOTE: Reset transfer id
# UNTRANSLATED (L234): tid = 0
# NOTE: initialize count for minmax4
# UNTRANSLATED (L236): mcount = 0
# NOTE: loop trough this subset
# Label only: repeat
# NOTE: Read chunk of data into buffer
# UNTRANSLATED (L240): call gdsi_read( set,                      cwlo,                      cwhi,                      buf,                      maxbuf,                      nd,                      tid )
# NOTE: Determine running minimum and maximum etc.
# UNTRANSLATED (L242): call minmax4( buf,                    nd,                    datamin(ns),                    datamax(ns),                    imin,                    imax,                    nblank(ns),                    mcount )
# NOTE: Ready when all data read
# UNTRANSLATED (L244): until (mcount .eq. ntotal)
# NOTE: get coordinates of min and max outside subset
# UNTRANSLATED (L246): for n = subdim + 1, setdim
# NOTE: Get grid
# UNTRANSLATED (L248): gmin(n) = gdsc_grid( set, axperm(n), subset(ns), gerror )
# NOTE: Is the same for a subset
# UNTRANSLATED (L250): gmax(n) = gmin(n)
# Label only: cfor
# NOTE: not all values BLANK?
# UNTRANSLATED (L253): if (ntotal .gt. nblank(ns))
# Label only: then
# NOTE: get coordinates of min and max inside subset
# UNTRANSLATED (L256): for n = subdim, 1, -1
# NOTE: divide by naxis1 * naxis 2 * ... * naxis(n-1)
# UNTRANSLATED (L258): g = imin / nf(n)
# NOTE: subtract nth axis part from position
# UNTRANSLATED (L260): imin = imin - g * nf(n)
# NOTE: this is the nth grid
# UNTRANSLATED (L262): gmin(n) = blo(n) + g
# NOTE: divide by naxis1 * naxis 2 * ... * naxis(n-1)
# UNTRANSLATED (L264): g = imax / nf(n)
# NOTE: subtract nth axis part from position
# UNTRANSLATED (L266): imax = imax - g * nf(n)
# NOTE: this is the nth grid
# UNTRANSLATED (L268): gmax(n) = blo(n) + g
# Label only: cfor
# Label only: cif
# NOTE: put out results
# UNTRANSLATED (L272): call output( outdev,                setdim,                subdim,                gmin,                gmax,                datamin(ns),                datamax(ns),                nblank(ns),                tabs,                tofile)
# Label only: cfor
# UNTRANSLATED (L274): if (tofile)
# Label only: then
# UNTRANSLATED (L276): close(7)
# Label only: cif
# NOTE: update descriptors ?
# UNTRANSLATED (L279): if (change)
# Label only: then
# NOTE: Update
# UNTRANSLATED (L282): call wminmax( set,                 subset,                 datamin,                 datamax,                 nblank,                 nsub,                 0 )
# Label only: cif
# NOTE: Quit communication with user
# UNTRANSLATED (L285): call finis
# 
#      End of program
# 
# Label only: stop
# Label only: end
# EJECT: PROGRAM MNMX (subroutine heading) (SHELTRAN EJECT directive)
# UNTRANSLATED (L292): subroutine heading( outdev, setdim, subdim, axlab, tabs, tofile )
# 
#      This procedure prints the heading of the columns which will be
#      printed by OUTPUT. At the same time the column positions are
#      determined. The tabs are set in the following way:
#      ........t1........t2........t3........t4........t5........t6
#      subset  DATAMIN   position  DATAMAX   position  NBLANK    END
#       grids            minimum             maximum
# 
#      Arguments:
# 
# NOTE: Output device
# UNTRANSLATED (L304): integer     outdev
# NOTE: Number of labels (set dimension)
# UNTRANSLATED (L306): integer     setdim
# NOTE: Dimension of subset
# UNTRANSLATED (L308): integer     subdim
# NOTE: Axis labels:
# UNTRANSLATED (L310): character*5 axlab(*)
# NOTE: Tabulator stops
# UNTRANSLATED (L312): integer     tabs(7)
# UNTRANSLATED (L313): logical     tofile
# 
#      Local variables:
# 
# NOTE: Character string for output
# UNTRANSLATED (L318): character*132 mess
# NOTE: Loop counter
# UNTRANSLATED (L320): integer       m
# NOTE: Tab counter
# UNTRANSLATED (L322): integer       t
# 
#      Executable code:
# 
# NOTE: Reset this mess
# UNTRANSLATED (L327): mess = ' '
# NOTE: the first position
# Label only: t = 1
# NOTE: are the grids assigned to subset ?
# UNTRANSLATED (L331): if (subdim .lt. setdim)
# Label only: then
# NOTE: show them between brackets
# UNTRANSLATED (L334): mess(t:) = '('
# NOTE: next writing position
# Label only: t = 3
# NOTE: show grids defined in subset coordinate word
# UNTRANSLATED (L338): for m = subdim + 1, setdim
# NOTE: put stripped axis name in place
# UNTRANSLATED (L340): write( mess(t:), '(A,'','')' ) axlab(m)
# NOTE: next writing position
# UNTRANSLATED (L342): t = t + 6
# Label only: cfor
# NOTE: closing bracket
# UNTRANSLATED (L345): write( mess(t-1:), '('') '')' )
# NOTE: next writing position
# UNTRANSLATED (L347): t = t + 1
# Label only: cif
# NOTE: save first tabulator stop
# UNTRANSLATED (L350): tabs(1) = t
# NOTE: Is subset a pixel?
# UNTRANSLATED (L352): if (subdim .gt. 0)
# Label only: then
# NOTE: show column head
# UNTRANSLATED (L355): write( mess(t:), '(''DATAMIN at '')' )
# NOTE: next writing position
# UNTRANSLATED (L357): t = t + 11
# NOTE: save second tabulator stop
# UNTRANSLATED (L359): tabs(2) = t
# NOTE: show position of minimum between brackets
# UNTRANSLATED (L361): write( mess(t:), '(''('')' )
# NOTE: next writing position
# UNTRANSLATED (L363): t = t + 1
# NOTE: loop through subset axes
# UNTRANSLATED (L365): for m = 1, subdim
# NOTE: put stripped axisname in place
# UNTRANSLATED (L367): write( mess(t:), '(A,'','')' ) axlab(m)
# NOTE: next writing position
# UNTRANSLATED (L369): t = t + 6
# Label only: cfor
# NOTE: closing bracket
# UNTRANSLATED (L372): write( mess(t-1:), '('') '')' )
# NOTE: next writing position
# UNTRANSLATED (L374): t = t + 1
# NOTE: save third tabulator stop
# UNTRANSLATED (L376): tabs(3) = t
# NOTE: show column head
# UNTRANSLATED (L378): write( mess(t:), '(''DATAMAX at '')' )
# NOTE: next writing position
# UNTRANSLATED (L380): t = t + 11
# NOTE: save fourth tabulator stop
# UNTRANSLATED (L382): tabs(4) = t
# NOTE: put position of maximum between brackets
# UNTRANSLATED (L384): write( mess(t:), '(''('')' )
# NOTE: next writing position
# UNTRANSLATED (L386): t = t + 1
# NOTE: loop trough dimensions of subset
# UNTRANSLATED (L388): for m = 1, subdim
# NOTE: put stripped axis name in place
# UNTRANSLATED (L390): write( mess(t:), '(A,'','')' ) axlab(m)
# NOTE: next writing position
# UNTRANSLATED (L392): t = t + 6
# Label only: cfor
# NOTE: write closing bracket
# UNTRANSLATED (L395): write( mess(t-1:), '('') '')' )
# NOTE: next writing position
# UNTRANSLATED (L397): t = t + 1
# NOTE: save fifth tabulator stop
# UNTRANSLATED (L399): tabs(5) = t
# NOTE: write column head
# UNTRANSLATED (L401): write( mess(t:), '(''  NBLANK '')' )
# NOTE: next writing position
# UNTRANSLATED (L403): t = t + 9
# NOTE: save sixth tabulator stop
# UNTRANSLATED (L405): tabs(6) = t
# NOTE: subset is a pixel
# Label only: else
# NOTE: show column head
# UNTRANSLATED (L409): write( mess(t:), '('' DATA VALUE    '')' )
# NOTE: last writing position
# UNTRANSLATED (L411): t = t + 16
# NOTE: save second tabulator stop
# UNTRANSLATED (L413): tabs(2) = t
# Label only: cif
# NOTE: output to output device
# UNTRANSLATED (L416): call anyout( outdev, mess(:t) )
# UNTRANSLATED (L417): if (tofile)
# Label only: then
# UNTRANSLATED (L419): write(7,*) mess(:t)
# Label only: cif
# 
#      Return to caller
# 
# Label only: return
# Label only: end
# EJECT: PROGRAM MNMX (subroutine output) (SHELTRAN EJECT directive)
# UNTRANSLATED (L427): subroutine output( outdev, setdim, subdim, gmin, gmax,                   amin, amax, nblank, tabs, tofile )
# 
#      This procedure prints the grids defined in the subset coordinate
#      word, the minimum, position of minimum, maximum, position of
#      maximum and the number of blanks. It uses the tabulator stops
#      determined by subroutine heading.
#      ........t1........t2........t3........t4........t5........t6
#      subset  DATAMIN   position  DATAMAX   position  NBLANK    END
#       grids            minimum             maximum
# 
#      Arguments:
# 
# NOTE: Output device
# UNTRANSLATED (L440): integer outdev
# NOTE: Set dimension
# UNTRANSLATED (L442): integer setdim
# NOTE: Subset dimension
# UNTRANSLATED (L444): integer subdim
# NOTE: Grid positions of minimum
# UNTRANSLATED (L446): integer gmin(*)
# NOTE: Grid positions of maximum
# UNTRANSLATED (L448): integer gmax(*)
# NOTE: Minimum value
# UNTRANSLATED (L450): real    amin
# NOTE: Maximum value
# UNTRANSLATED (L452): real    amax
# NOTE: Number of blanks
# UNTRANSLATED (L454): integer nblank
# NOTE: Tabulator stops
# UNTRANSLATED (L456): integer tabs(6)
# UNTRANSLATED (L457): logical tofile
# 
#      Locals:
# 
# NOTE: Character string
# UNTRANSLATED (L462): character*132 mess
# NOTE: Loop counter
# UNTRANSLATED (L464): integer       m
# NOTE: Tabulator counter
# UNTRANSLATED (L466): integer       t
# 
#      Functions:
# 
# NOTE: Returns true if argument blank value
# UNTRANSLATED (L471): logical fblank
# 
#      Executable statements:
# 
# NOTE: Reset this mess
# UNTRANSLATED (L476): mess = ' '
# NOTE: The first position
# Label only: t = 1
# NOTE: any grids defined in subset coordinate word ?
# UNTRANSLATED (L480): if (subdim .lt. setdim)
# Label only: then
# NOTE: show them between brackets
# UNTRANSLATED (L483): mess(t:) = '('
# NOTE: next writing position
# Label only: t = 3
# NOTE: loop through dimensions outside subset
# UNTRANSLATED (L487): for m = subdim + 1, setdim
# NOTE: put grid coordinate in place
# UNTRANSLATED (L489): write( mess(t:), '(i5,'','')' ) gmin(m)
# NOTE: next writing position
# UNTRANSLATED (L491): t = t + 6
# Label only: cfor
# NOTE: closing bracket
# UNTRANSLATED (L494): write(mess(t-1:),'('') '')')
# Label only: cif
# NOTE: now at fist tabulator stop
# UNTRANSLATED (L497): t = tabs(1)
# NOTE: is subset a pixel ?
# UNTRANSLATED (L499): if (subdim .gt. 0)
# Label only: then
# NOTE: is minimum defined ?
# UNTRANSLATED (L502): if (.not. fblank( amin ))
# Label only: then
# NOTE: show minimum
# UNTRANSLATED (L505): write( mess(t:), '(g10.3,1x)' ) amin
# Label only: else
# NOTE: minimum not defined
# UNTRANSLATED (L508): write( mess(t:), '(''  BLANK    '')' )
# Label only: cif
# NOTE: now at second tabulator stop
# UNTRANSLATED (L511): t = tabs(2)
# NOTE: show position between brackets
# UNTRANSLATED (L513): write( mess(t:), '(''('')' )
# NOTE: next writing position
# UNTRANSLATED (L515): t = t + 1
# NOTE: loop through subset dimensions
# UNTRANSLATED (L517): for m = 1, subdim
# NOTE: minimum defined ?
# UNTRANSLATED (L519): if (.not. fblank( amin ))
# Label only: then
# NOTE: show grid coordinate of minimum
# UNTRANSLATED (L522): write( mess(t:), '(i5,'','')' ) gmin(m)
# Label only: else
# NOTE: minimum not defined
# UNTRANSLATED (L525): write( mess(t:), '(''     ,'')' )
# Label only: cif
# NOTE: next writing position
# UNTRANSLATED (L528): t = t + 6
# Label only: cfor
# NOTE: closing bracket
# UNTRANSLATED (L531): write( mess(t-1:), '('') '')' )
# NOTE: now at third tabulator stop
# UNTRANSLATED (L533): t = tabs(3)
# NOTE: maximum defined ?
# UNTRANSLATED (L535): if (.not. fblank( amax ))
# Label only: then
# NOTE: show maximum
# UNTRANSLATED (L538): write( mess(t:), '(g10.3,1x)' ) amax
# Label only: else
# NOTE: maximum not defined
# UNTRANSLATED (L541): write( mess(t:), '(''  BLANK    '')' )
# Label only: cif
# NOTE: now at fourth tabulator stop
# UNTRANSLATED (L544): t = tabs(4)
# NOTE: show position of maximum between brackets
# UNTRANSLATED (L546): write( mess(t:), '(''('')' )
# NOTE: next writing position
# UNTRANSLATED (L548): t = t + 1
# NOTE: loop through subset dimensions
# UNTRANSLATED (L550): for m = 1, subdim
# NOTE: maximum defined ?
# UNTRANSLATED (L552): if (.not. fblank( amax ))
# Label only: then
# NOTE: show grid coordinate of maximum
# UNTRANSLATED (L555): write( mess(t:), '(i5,'','')' ) gmax(m)
# Label only: else
# NOTE: maximum not defined
# UNTRANSLATED (L558): write( mess(t:), '(''     ,'')' )
# Label only: cif
# NOTE: next writing position
# UNTRANSLATED (L561): t = t + 6
# Label only: cfor
# NOTE: closing bracket
# UNTRANSLATED (L564): write( mess(t-1:), '('') '')' )
# NOTE: now at fifth tabulator stop
# UNTRANSLATED (L566): t = tabs(5)
# NOTE: show number of blanks
# UNTRANSLATED (L568): write( mess(t:), '(i8,1x)' ) nblank
# NOTE: now at last tabulator stop
# UNTRANSLATED (L570): t = tabs(6)
# NOTE: subset is a pixel
# Label only: else
# NOTE: data value defined
# UNTRANSLATED (L574): if (.not. fblank( amin ))
# Label only: then
# NOTE: show data value
# UNTRANSLATED (L577): write( mess(t:), '(g15.8,1x)' ) amin
# Label only: else
# NOTE: show BLANK
# UNTRANSLATED (L580): write( mess(t:), '(''      BLANK     '')')
# Label only: cif
# NOTE: Now at second tabulator stop
# UNTRANSLATED (L583): t = tabs(2)
# Label only: cif
# NOTE: send text to output device
# UNTRANSLATED (L586): call anyout( outdev, mess(:t) )
# UNTRANSLATED (L587): if (tofile)
# Label only: then
# UNTRANSLATED (L589): write(7,*) mess(:t)
# Label only: cif
# 
#      Return to caller
# 
# Label only: return
# Label only: end
