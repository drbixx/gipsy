# EJECT: PROGRAM CBLANK (copyright notice)nk.shl
#                            COPYRIGHT (c) 1990
#          Kapteyn Astronomical Institute - University of Groningen
#             P.O. box 800,  9700 AV Groningen, The Netherlands
# 
# 
# EJECT: PROGRAM CBLANK (cblank.dc1)
# #>            cblank.dc1
# am:      CBLANK
# se:      Converts datavalues to BLANK and vice versa. CBLANK
#               also checks whether the data are legal (i.e. it checks
#               for NaN's etc. and replaces them by BLANKS).
# ory:     MANIPULATION, UTILITY
#          cblank.shl
# r:       K.G. Begeman
# rds:
# SET=     Set (and subsets) to work on. Maximum number of
#               subsets is 2048.
# X=       Frame of subset to work on [whole subset].
# ANK=     Value to be replaced by BLANK [BLANK].
# LUE=     Value to replace BLANK [BLANK].
# TION=    Option where to replace. 0 means inside frame,
#               1 means outside frame [0].
# le:      <USER  >CBLANK
#               <USER  >CBLANK   ,INSET=AURORA FREQ
#               Set AURORA has 3 axes
#               RA-NCP             from    -7 to     8
#               DEC-NCP            from    -7 to     8
#               FREQ-OHEL          from     1 to    59
#               BOX range for set AURORA :
#               RA-NCP             from    -7 to     8
#               DEC-NCP            from    -7 to     8
#               <USER  >CBLANK   ,BLANK=
#               <USER  >CBLANK   ,VALUE=0.0
#               <STATUS>  CBLANK  +++ FINISHED +++
# es:      Oct 26, 1988: KGB, Document created.
# 
# #<
# EJECT: PROGRAM CBLANK (code)
# UNTRANSLATED (L40): program cblank
# 
#      Parameters:
# 
# NOTE: Change version number on next line
# UNTRANSLATED (L45): character*(*) id
# UNTRANSLATED (L46): parameter (id = ' CBLANK  Version 1.1  Oct 26, 1993 ')
# NOTE: Maximum number of subsets
# UNTRANSLATED (L48): integer maxsubs
# UNTRANSLATED (L49): parameter (maxsubs = 2048)
# NOTE: Size of data buffer
# UNTRANSLATED (L51): integer maxdata
# UNTRANSLATED (L52): parameter (maxdata = 4096)
# NOTE: Maximum number of axes
# UNTRANSLATED (L54): integer maxaxes
# UNTRANSLATED (L55): parameter (maxaxes = 10)
# NOTE: Device for set info
# UNTRANSLATED (L57): integer showdev
# UNTRANSLATED (L58): parameter (showdev = 11)
# 
#      Variables for GDSINP:
# 
# NOTE: Name of GDS set
# UNTRANSLATED (L63): character*80 set
# NOTE: Array with the numbers of grids along an axis
# UNTRANSLATED (L65): integer      axcount(maxaxes)
# NOTE: Permutation array with axis number
# UNTRANSLATED (L67): integer      axperm(maxaxes)
# NOTE: Number of subsets entered by user
# UNTRANSLATED (L69): integer      nsubs
# NOTE: Dimension of input set
# UNTRANSLATED (L71): integer      setdim
# NOTE: Dimension of subsets
# UNTRANSLATED (L73): integer      subdim
# NOTE: Array for subset coordinate words
# UNTRANSLATED (L75): integer      subset(maxsubs)
# 
#      Variables for GDSBOX:
# 
# NOTE: Arrays for frame defined by user
# UNTRANSLATED (L80): integer      blo(maxaxes), bhi(maxaxes)
# 
#      Variables for BLANK and replacement VALUE:
# 
# NOTE: System defined BLANK
# UNTRANSLATED (L85): real         blank
# NOTE: Replacement value
# UNTRANSLATED (L87): real         value
# 
#      Variables for INITPTR:
# 
# NOTE: Counter for initptr
# UNTRANSLATED (L92): integer      icount
# NOTE: Pointer in/outside sub frame
# UNTRANSLATED (L94): integer      ip
# NOTE: Counter in/outside sub frame
# UNTRANSLATED (L96): integer      np
# 
#      Variables for MINMAX3:
# 
# NOTE: Counter for minmax3
# UNTRANSLATED (L101): integer      mcount
# NOTE: Array for number of blanks in subsets
# UNTRANSLATED (L103): integer      nblank(maxsubs)
# NOTE: Array for maximum in subsets
# UNTRANSLATED (L105): real         datamax(maxsubs)
# NOTE: Array for minimum in subsets
# UNTRANSLATED (L107): real         datamin(maxsubs)
# 
#      Variables for GDS:
# 
# NOTE: Coordinate words
# UNTRANSLATED (L112): integer      cwlo, cwhi
# NOTE: GDS error returns
# UNTRANSLATED (L114): integer      gerror
# NOTE: Transfer id input and output data
# UNTRANSLATED (L116): integer      tid1, tid2
# 
#      Other variables:
# 
# UNTRANSLATED (L120): character*80 text
# NOTE: Arrays for edge grids of subset
# UNTRANSLATED (L122): integer      flo(maxaxes), fhi(maxaxes)
# NOTE: Loop counter
# UNTRANSLATED (L124): integer      n
# NOTE: Number of blanks added
# UNTRANSLATED (L126): integer      nbt
# NOTE: Subset counter
# UNTRANSLATED (L128): integer      ns
# NOTE: Option
# UNTRANSLATED (L130): integer      option
# NOTE: Number of pixels in data buffer
# UNTRANSLATED (L132): integer      pixels_done
# NOTE: Array for data
# UNTRANSLATED (L134): real         datar(maxdata)
# 
#      Functions:
# 
# NOTE: Fills coordinate word with array of grids
# UNTRANSLATED (L139): integer      gdsc_fill
# NOTE: Returns grid value
# UNTRANSLATED (L141): integer      gdsc_grid
# NOTE: Gets dimension of substructure
# UNTRANSLATED (L143): integer      gdsc_ndims
# NOTE: Gets input set and subset from user
# UNTRANSLATED (L145): integer      gdsinp
# NOTE: Gets input (integers) from user
# UNTRANSLATED (L147): integer      userint
# NOTE: Get input (reals) from user
# UNTRANSLATED (L149): integer      userreal
# NOTE: True when inside sub frame
# UNTRANSLATED (L151): logical      insideptr
# NOTE: True when outside sub frame
# UNTRANSLATED (L153): logical      outsideptr
# UNTRANSLATED (L154): integer      clspfp
# 
#      Data statements:
# 
# NOTE: Reset GDS error return
# UNTRANSLATED (L159): data gerror / 0 /
# NOTE: No restriction on subset dimensions
# UNTRANSLATED (L161): data subdim / 0 /
# 
#      Executable code:
# 
# NOTE: Link with HERMES
# UNTRANSLATED (L166): call init
# NOTE: Show user who we are
# UNTRANSLATED (L168): call anyout( 8, id )
# NOTE: Get input set from user
# UNTRANSLATED (L170): nsubs = gdsinp( set,                subset,                maxsubs,                0,                'INSET=',                'Set (and subsets) to work on' ,                showdev,                axperm,                axcount,                maxaxes,                1,                subdim )
# NOTE: Get dimension of input set
# UNTRANSLATED (L172): setdim = gdsc_ndims( set, 0 )
# NOTE: Get edge coordinates words
# UNTRANSLATED (L174): call gdsc_range( set, subset, cwlo, cwhi, gerror )
# NOTE: Get edges of subset
# UNTRANSLATED (L176): for n = 1, subdim
# NOTE: Get lower left grid
# UNTRANSLATED (L178): flo(n) = gdsc_grid( set, axperm(n), cwlo, gerror )
# NOTE: Get upper left grid
# UNTRANSLATED (L180): fhi(n) = gdsc_grid( set, axperm(n), cwhi, gerror )
# Label only: cfor
# NOTE: Get frame of subset to work on
# UNTRANSLATED (L183): call gdsbox( blo,             bhi,             set,             subset,             2,             'BOX=',             ' ',             showdev,             0 )
# NOTE: Get BLANK
# UNTRANSLATED (L185): if (userreal( blank,              1,              1,              'BLANK=',              'Value in set to be replaced [BLANK]' ) .eq. 0)
# Label only: then
# NOTE: Default is BLANK
# UNTRANSLATED (L188): call setfblank( blank )
# Label only: cif
# NOTE: Get replacement value
# UNTRANSLATED (L191): if (userreal( value,              1,              1,              'VALUE=',              'Value to replace [BLANK]' ) .eq. 0)
# Label only: then
# NOTE: Default is BLANK
# UNTRANSLATED (L194): call setfblank( value )
# Label only: cif
# NOTE: Get option from user
# UNTRANSLATED (L197): if (userint( option,             1,             2,             'OPTION=',             'Give option where to replace [0]' ) .eq. 0)
# Label only: then
# NOTE: Default is inside frame
# UNTRANSLATED (L200): option = 0
# Label only: cif
# NOTE: Now check whether we should do anything
# UNTRANSLATED (L203): if (value .eq. blank)
# Label only: then
# NOTE: Simple solution
# UNTRANSLATED (L206): call anyout( 1, 'Only checking for legal data!' )
# Label only: cif
# NOTE: Subset loop
# UNTRANSLATED (L209): for ns = 1, nsubs
# NOTE: Show user what we are working on
# UNTRANSLATED (L211): call showsub1( set, subset(ns), axperm )
# NOTE: Reset transfer id input data
# UNTRANSLATED (L213): tid1 = 0
# NOTE: Reset transfer id output data
# UNTRANSLATED (L215): tid2 = 0
# NOTE: Reset counter for initptr
# UNTRANSLATED (L217): icount = 0
# NOTE: Reset counter for minmax3
# UNTRANSLATED (L219): mcount = 0
# NOTE: Get lower coordinate words of frame
# UNTRANSLATED (L221): cwlo = gdsc_fill( set, subset(ns), flo )
# NOTE: Get upper coordinate words of frame
# UNTRANSLATED (L223): cwhi = gdsc_fill( set, subset(ns), fhi )
# NOTE: Read/write loop
# UNTRANSLATED (L225): nbt = 0
# Label only: repeat
# NOTE: Read data
# UNTRANSLATED (L228): call gdsi_read( set,                      cwlo,                      cwhi,                      datar,                      maxdata,                      pixels_done,                      tid1 )
# NOTE: Initialize initptr
# UNTRANSLATED (L230): call initptr( flo,                    fhi,                    blo,                    bhi,                    subdim,                    pixels_done,                    icount )
# NOTE: Check data
# UNTRANSLATED (L232): nbt = nbt + clspfp( datar, pixels_done )
# NOTE: Do we have to do anything?
# UNTRANSLATED (L234): if (value .ne. blank )
# Label only: then
# NOTE: Replace inside frame
# UNTRANSLATED (L237): if (option .eq. 0)
# Label only: then
# NOTE: Inside frame
# UNTRANSLATED (L240): while (insideptr( ip, np ))
# NOTE: Loop though data in buffer
# UNTRANSLATED (L242): for n = 1, np
# NOTE: Does it match
# UNTRANSLATED (L244): if (datar(n+ip) .eq. blank)
# Label only: then
# NOTE: Replace it
# UNTRANSLATED (L247): datar(n+ip) = value
# Label only: cif
# Label only: cfor
# Label only: cwhile
# NOTE: Replace outside frame
# Label only: else
# NOTE: Outside frame
# UNTRANSLATED (L254): while (outsideptr( ip, np ))
# NOTE: Loop though data in buffer
# UNTRANSLATED (L256): for n = 1, np
# NOTE: Does it match
# UNTRANSLATED (L258): if (datar(n+ip) .eq. blank)
# Label only: then
# NOTE: Replace it
# UNTRANSLATED (L261): datar(n+ip) = value
# Label only: cif
# Label only: cfor
# Label only: cwhile
# Label only: cif
# Label only: cif
# NOTE: Determine new minimum and maximum
# UNTRANSLATED (L268): call minmax3( datar,                    pixels_done,                    datamin(ns),                    datamax(ns),                    nblank(ns),                    mcount )
# NOTE: Write (modified) data back to disk
# UNTRANSLATED (L270): call gdsi_write( set,                       cwlo,                       cwhi,                       datar,                       pixels_done,                       pixels_done,                       tid2 )
# UNTRANSLATED (L271): until (tid1 .eq. 0)
# UNTRANSLATED (L272): if (nbt .ne. 0)
# Label only: then
# UNTRANSLATED (L274): write( text, '(''Found '',I5,'' Weirdos'')') NBT
# UNTRANSLATED (L275): call anyout( 0, text )
# Label only: endif
# Label only: cfor
# NOTE: Update new minimum and maximum
# UNTRANSLATED (L279): call wminmax( set,              subset,              datamin,              datamax,              nblank,              nsubs,              1 )
# NOTE: Tell HERMES we're done
# UNTRANSLATED (L281): call finis
# 
#      That's All Folks
# 
# Label only: stop
# Label only: end
