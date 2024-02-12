C...  Venus-GRAM 2005 Dummy Trajectory Program (Version 1)              DUMT  1
C                          March, 2005                                  DUMT  2
C                                                                       DUMT  3
C     A program to illustrate how Venus-GRAM subroutines can be in-     DUMT  4
C     corporated into a (double precision) trajectory-calculation code. DUMT  5
C                                                                       DUMT  6
C     SUBSTITUTE YOUR OWN TRAJECORY CODE FOR THIS MAIN DRIVER AND FOR   DUMT  7
C     SUBROUTINES trajest_V05 AND initcode_V05                          DUMT  8
C                                                                       DUMT  9
C     RETAIN THE SUBROUTINE Venustraj_V05.                              DUMT 10
C                                                                       DUMT 11
C     All interface between the main driver trajectory program and      DUMT 12
C     Venus-GRAM is via the three calls to Venustraj_V05 (One with      DUMT 13
C     isetup=1, the others with isetup = 0).  Input to Venus-GRAM       DUMT 14
C     routine is via the NAMELIST FORMAT file INPUT, called in the      DUMT 15
C     setup mode.  A number of Monte-Carlo trajectories (NMONTE) can be DUMT 16
C     calculated during one program run. Each trajectory can have up to DUMT 17
C     a maximum of MAXNUM points, or exit from a given trajectory (and  DUMT 18
C     on to the next Monte-Carlo case) can be controlled by the param-  DUMT 19
C     eter EOF.                                                         DUMT 20
C                                                                       DUMT 21
C     To suppress output of the LIST and other files make sure to use   DUMT 22
C     iup = 0 on the INPUT file (or permanently change it to 0 in the   DUMT 23
C     Setup_V05 subroutine, near line 89).                              DUMT 24
C                                                                       DUMT 25
C.....................................................................  DUMT 26
C                                                                       DUMT 27
      Implicit None                                                     DUMT 28
      Integer EOF,iustdin,iustdout,jmonte,NMONTE,MAXNUM,NR1,istep,      DUMT 29
     &   iupdate,NRx                                                    DUMT 30
      Character*60 INPUTFL                                              DUMT 31
      Double Precision zhgt,zlat,zlon,pzhgt,pzlat,pzlon,NSWIND,NSpert,  DUMT 32
     &   TEMP,PRES,DENSLO,DENS,DENSHI,DENSP,RHOd,RHOu,RHOv,dens2,AMz,   DUMT 33
     &   EWWIND,EWpert,DENS1,DSCLHGT1,corlim,dsclhgt2,fmolCO2,fmolN2,   DUMT 34
     &   fmolO,fmolCO,fmolHe,fmolN,fmolH,dt,vz,az,vlat,vlon,pertstep,   DUMT 35
     &   zsec,pzsec,DAY0,dsunlat,dsunlon,dsunLs,dradau,dowlt,ALS,SZA,   DUMT 36
     &   owlt,sunlat,sunlon,VenusAU,TLOCAL                              DUMT 37
C...  Establish unit numbers for standard (screen) input and output     DUMT 38
      iustdin = 5                                                       DUMT 39
      iustdout = 6                                                      DUMT 40
C...  Establish the name of the NAMELIST format input file              DUMT 41
      Write(iustdout,*)' Enter file name for NAMELIST input'            DUMT 42
      Read(iustdin,10)INPUTFL                                           DUMT 43
  10  Format(A)                                                         DUMT 44
C                                                                       DUMT 45
C     Set ephemeris values to 0 if using built-in ephemeris routine,    DUMT 46
C     otherwise compute high-precision values here                      DUMT 47
      dsunlat = 0.0d0                                                   DUMT 48
      dsunlon = 0.0d0                                                   DUMT 49
      dsunLs = 0.0d0                                                    DUMT 50
      dradau = 0.0d0                                                    DUMT 51
      dowlt = 0.0d0                                                     DUMT 52
C                                                                       DUMT 53
C     Set up the Venus-GRAM routine (ISETUP = 1)                        DUMT 54
      iupdate = 0                                                       DUMT 55
      Call Venustraj_V05(1,0,NMONTE,0,MAXNUM,zhgt,zlat,zlon,zsec,pzhgt, DUMT 56
     & pzlat,pzlon,pzsec,DAY0,TEMP,PRES,DENSLO,DENS,DENSHI,             DUMT 57
     & DENSP,RHOd,RHOu,RHOv,EWWIND,EWpert,NSWIND,NSpert,EOF,NR1,DENS1,  DUMT 58
     & DSCLHGT1,corlim,INPUTFL,iustdout,fmolCO2,fmolN2,fmolO,fmolCO,    DUMT 59
     & fmolHe,fmolN,fmolH,AMz,iupdate,pertstep,dsunlat,dsunlon,dsunLs,  DUMT 60
     & dradau,dowlt,ALS,SZA,owlt,sunlat,sunlon,VenusAU,TLOCAL)          DUMT 61
C                                                                       DUMT 62
C...  Step through Monte Carlo runs                                     DUMT 63
      Do 200 jmonte = 1,NMONTE                                          DUMT 64
C       Initialize position, time step, velocity, and acceleration      DUMT 65
        Call initcode_V05(zsec,zhgt,zlat,zlon,dt,vz,az,vlat,vlon)       DUMT 66
C       Initialize the Venus-GRAM variables (istep = 0) and compute     DUMT 67
C       density at beginning of first trajectory step                   DUMT 68
C                                                                       DUMT 69
        NRx = NR1                                                       DUMT 70
C                                                                       DUMT 71
C.....................................................................  DUMT 72
C       If the user wants to have one or more profiles where random     DUMT 73
C       perturbations are the same, then insert required logic here.    DUMT 74
C                                                                       DUMT 75
C       Examples:                                                       DUMT 76
C.....................................................................  DUMT 77
C                                                                       DUMT 78
C       To do a sequence of pairs of profiles, with the second member   DUMT 79
C       of each pair having the same perturbations as the first of      DUMT 80
C       the pair, use -                                                 DUMT 81
C                                                                       DUMT 82
C       If (Mod(jmonte,2).eq.0)NRx = NR1 - 11                           DUMT 83
C                                                                       DUMT 84
C       To do a sequence of profiles with all members of the sequence   DUMT 85
C       having the same perturbations as the first profile, use -       DUMT 86
C                                                                       DUMT 87
C       If (jmonte.gt.1)NRx = NR1 - 11                                  DUMT 88
C                                                                       DUMT 89
C       The algorithm used here (NRx = NR1 - 11) must be the            DUMT 90
C       reverse of the algorithm used in subroutine Randinit_V05 to     DUMT 91
C       increase NR1 between successive profiles (at line RNDI  8).     DUMT 92
C                                                                       DUMT 93
C.....................................................................  DUMT 94
C                                                                       DUMT 95
C       If using externally-calculated, high-precision ephemeris        DUMT 96
C       routine, recalculate values here                                DUMT 97
C                                                                       DUMT 98
        Call Venustraj_V05(0,jmonte,NMONTE,0,MAXNUM,zhgt,zlat,zlon,     DUMT 99
     &   zsec,zhgt,zlat,zlon,zsec,DAY0,TEMP,PRES,DENSLO,DENS,DENSHI,    DUMT100
     &   DENSP,RHOd,RHOu,RHOv,EWWIND,EWpert,NSWIND,NSpert,EOF,NR1,      DUMT101
     &   DENS1,DSCLHGT1,corlim,INPUTFL,iustdout,fmolCO2,fmolN2,fmolO,   DUMT102
     &   fmolCO,fmolHe,fmolN,fmolH,AMz,iupdate,pertstep,dsunlat,        DUMT103
     &   dsunlon,dsunLs,dradau,dowlt,ALS,SZA,owlt,sunlat,sunlon,        DUMT104
     &   VenusAU,TLOCAL)                                                DUMT105
        NR1 = NRx                                                       DUMT106
C       TEMP, PRES, DENS are mean temperature, pressure, density at     DUMT107
C       beginning of 1st trajectory step; DENS1 is total (mean plus     DUMT108
C       perturbed) density at beginning of 1st trajectory step          DUMT109
      Do 100 istep = 1,MAXNUM                                           DUMT110
C       Estimate next trajectory position (based on 1st predictor       DUMT111
C       calculation and total density at beginning of trajectory step)  DUMT112
C       and store previous position for which Venus-GRAM was called.    DUMT113
C                                                                       DUMT114
        Call trajest_V05(zsec,zhgt,zlat,zlon,pzsec,pzhgt,pzlat,pzlon,   DUMT115
     &   DENS1,DSCLHGT1,vz,az,vlat,vlon,dt,istep-1)                     DUMT116
C                                                                       DUMT117
C       *** NOTE - If computed longitude zlon is positive West, then    DUMT118
C       LonEast should be set to 0 in the NAMELIST input file (read in  DUMT119
C       by the Setup_V05 subroutine) ***                                DUMT120
C                                                                       DUMT121
C       Evaluate density at estimated next trajectory position          DUMT122
        Call Venustraj_V05(0,0,NMONTE,istep,MAXNUM,zhgt,zlat,zlon,zsec, DUMT123
     &   pzhgt,pzlat,pzlon,pzsec,DAY0,TEMP,PRES,DENSLO,DENS,            DUMT124
     &   DENSHI,DENSP,RHOd,RHOu,RHOv,EWWIND,EWpert,NSWIND,NSpert,EOF,   DUMT125
     &   NR1,DENS2,DSCLHGT2,corlim,INPUTFL,iustdout,fmolCO2,fmolN2,     DUMT126
     &   fmolO,fmolCO,fmolHe,fmolN,fmolH,AMz,iupdate,pertstep,dsunlat,  DUMT127
     &   dsunlon,dsunLs,dradau,dowlt,ALS,SZA,owlt,sunlat,sunlon,        DUMT128
     &   VenusAU,TLOCAL)                                                DUMT129
C       TEMP, PRES, DENS are mean temperature, pressure, density at     DUMT130
C       estimated next trajectory position; DENS2 is total (mean plus   DUMT131
C       perturbed) density at estimated next trajectory position        DUMT132
C                                                                       DUMT133
C       If the user desires to revise the trajectory position estimate  DUMT134
C       (zhgt, zlat, zlon), this can be done here (e.g. via a corrector DUMT135
C       step in a predictor-corrector solution and/or via a scheme      DUMT136
C       that uses variation of total density along the trajectory step, DUMT137
C       i.e. between DENS1 at the beginning of the step and DENS2 at    DUMT138
C       the end of the step).  Interpolation between DENS1 and DENS2    DUMT139
C       can be done in an appropriate manner as selected by the user    DUMT140
C       (e.g. linear interpolation may be valid for small trajectory    DUMT141
C       time steps). No further calls to Venustraj_V05 should be done   DUMT142
C       during this predictor-corrector process of updating zhgt, zlat, DUMT143
C       and zlon (as this will lead to erroneous results for the        DUMT144
C       density perturbation values).                                   DUMT145
C                                                                       DUMT146
C       Store total density and scale height for use as value at        DUMT147
C       beginning of next trajectory step                               DUMT148
        DENS1 = DENS2                                                   DUMT149
        DSCLHGT1 = DSCLHGT2                                             DUMT150
C                                                                       DUMT151
C       If the user wishes to call the atmosphere model without         DUMT152
C       updating the perturbations or total perturbation step size,     DUMT153
C       this can be done here by using iupdate < 0                      DUMT154
C                                                                       DUMT155
C     Example:                                                          DUMT156
C...................................................................... DUMT157
C       iupdate = -1                                                    DUMT158
C       Call Venustraj_V05(0,0,NMONTE,istep,MAXNUM,(zhgt+pzhgt)/2.0d0,  DUMT159
C    &   (zlat+pzlat)/2.0d0,(zlon+pzlon)/2.0d0,(zsec+pzsec)/2.0d0,      DUMT160
C    &   pzhgt,pzlat,pzlon,pzsec,DAY0,TEMP,PRES,DENSLO,DENS,            DUMT161
C    &   DENSHI,DENSP,RHOd,RHOu,RHOv,EWWIND,EWpert,NSWIND,NSpert,EOF,   DUMT162
C    &   NR1,DENS2,DSCLHGT2,corlim,INPUTFL,iustdout,fmolCO2,fmolN2,     DUMT163
C    &   fmolO,fmolCO,fmolHe,fmolN,fmolH,AMz,iupdate,pertstep,dsunlat,  DUMT164
C    &   dsunlon,dsunLs,dradau,dowlt,ALS,SZA,owlt,sunlat,sunlon,        DUMT165
C    &   VenusAU,TLOCAL)                                                DUMT166
C       iupdate = 0                                                     DUMT167
C...................................................................... DUMT168
C       End this trajectory calculation if parameter EOF is 1           DUMT169
        If (EOF.eq.1)Goto 200                                           DUMT170
 100  Continue                                                          DUMT171
 200  Continue                                                          DUMT172
      End                                                               DUMT173
C---------------------------------------------------------------------  DUMT174
C                                                                       DUMT175
C     NOTE: The Venustraj_V05 subroutine should be incorporated into    DUMT176
C     your trajectory code, as a "wrapper" routine for calling          DUMT177
C     Venus-GRAM setup and subroutines. The main routine and all other  DUMT178
C     subroutines in dumytraj are for illustrative purposes only and    DUMT179
C     are not needed in your trajectory program.                        DUMT180
C                                                                       DUMT181
C---------------------------------------------------------------------  DUMT182
      Subroutine trajest_V05(t,z,xlat,xlon,pt,pz,plat,plon,dens1,       TRAJ  1
     &  sclhgt1,vz,az,vlat,vlon,dt,n)                                   TRAJ  2
C                                                                       TRAJ  3
C...  Dummy trajectory model to estimate height (z), latitude (xlat),   TRAJ  4
C     and longitude (xlon) from previous height (pz), previous          TRAJ  5
C     latitude (plat), and previous longitude (plon).  This simple      TRAJ  6
C     example estimates trajectory position by assuming Newton's        TRAJ  7
C     forward difference to compute displacements from velocity         TRAJ  8
C     components. In general it may also use the (total) density and    TRAJ  9
C     density scale height at the beginning of the trajectory step      TRAJ 10
C     (although density and scale height are not actually used in       TRAJ 11
C     this dummy version)                                               TRAJ 12
C                                                                       TRAJ 13
C     SUBSTITUTE YOUR OWN (double precision) TRAJECTORY POSITION        TRAJ 14
C     ESTIMATING ROUTINE                                                TRAJ 15
C                                                                       TRAJ 16
C...................................................................... TRAJ 17
      Implicit None                                                     TRAJ 18
      Double precision z,xlat,xlon,pz,plat,plon,vz,vlat,vlon,dt,az,     TRAJ 19
     &  density,dens1,scalehgt,sclhgt1,t,pt                             TRAJ 20
      Integer n                                                         TRAJ 21
C...  Store density and scale height at beginning of trajectory step    TRAJ 22
C     (these are not actually used in this example version)             TRAJ 23
      density = dens1                                                   TRAJ 24
      scalehgt = sclhgt1                                                TRAJ 25
C...  Store current position as previous position                       TRAJ 26
      pt = t                                                            TRAJ 27
      pz = z                                                            TRAJ 28
      plat = xlat                                                       TRAJ 29
      plon = xlon                                                       TRAJ 30
C...  If desired, update time increment dt here (or use input value)    TRAJ 31
C...  Evaluate new time t, using time increment, dt                     TRAJ 32
      t = pt + dt                                                       TRAJ 33
C...  Use vz and az to compute height change by Newton's forward        TRAJ 34
C     difference                                                        TRAJ 35
      z = pz + vz*dt + az*(2.0d0*n + 1.0d0)*dt**2                       TRAJ 36
C...  Use vlat and vlon to compute latitude-longitude change            TRAJ 37
C     (NOTE: There are no corrections here for going over the pole)     TRAJ 38
      xlat = plat + vlat*dt                                             TRAJ 39
      xlon = plon + vlon*dt                                             TRAJ 40
C     *** NOTE - If longitude xlon is positive West, then LonEast       TRAJ 41
C     should be set to 0 in the NAMELIST input file (read in by the     TRAJ 42
C     Setup_V05 subroutine) ***                                         TRAJ 43
C...  Stop if dummy trajectory goes over the pole without corrections   TRAJ 44
      If(Dabs(xlat).GT.90.0D0)Then                                      TRAJ 45
        Stop ' Dummy trajectory crossed pole without corrections'       TRAJ 46
      Endif                                                             TRAJ 47
C...  Insure that longitude stays in 0-360 bounds                       TRAJ 48
      If(xlon.lt.0.0D0)xlon = xlon + 360.0D0                            TRAJ 49
      If(xlon.ge.360.0D0) xlon = xlon - 360.0D0                         TRAJ 50
      Return                                                            TRAJ 51
      End                                                               TRAJ 52
C---------------------------------------------------------------------- TRAJ 53
      Subroutine initcode_V05(zsec,zhgt,zlat,zlon,dt,vz,az,vlat,vlon)   TINI  1
      Implicit None                                                     TINI  2
      Double Precision zhgt,zlat,zlon,dt,vz,az,vlat,vlon,t1,t2,z1,z2,   TINI  3
     & zsec                                                             TINI  4
C...  Dummy trajectory initialization routine to set up starting values TINI  5
C     of position                                                       TINI  6
C                                                                       TINI  7
C     SUBSTITUTE YOUR OWN (double precision) TRAJECTORY POSITION        TINI  8
C     INITIALIZATION ROUTINE                                            TINI  9
C                                                                       TINI 10
C     *** NOTE - If longitude zlon is positive West, LonEast should     TINI 11
C     be set to 0 in the NAMELIST input file (read in by the Setup_V05  TINI 12
C     subroutine) ***                                                   TINI 13
C                                                                       TINI 14
C...  The dummy values below reproduce the vertical profile of the      TINI 15
C     reference case (with LonEast = 1).  Other values of initial       TINI 16
C     position (zhgt, zlat, and zlon) and change in position (set       TINI 17
C     by vz, vlat, and vlon in the trajest_V05 subroutine), can be used TINI 18
C     to simulate various positions for comparison with output          TINI 19
C     from the stand-alone Venus-GRAM program.                          TINI 20
C     zsec = 0.0d0                                                      TINI 21
C     zhgt = 0.0D0                                                      TINI 22
C     zlat = -31.3D0                                                    TINI 23
C     zlon = 317.0D0                                                    TINI 24
C     dt = 500.0D0                                                      TINI 25
C     vlat = 0.0006D0                                                   TINI 26
C     vlon = 0.001D0                                                    TINI 27
C     vz = 0.002D0                                                      TINI 28
C     az = 0.0                                                          TINI 29
C                                                                       TINI 30
C...  Set initial position values                                       TINI 31
      zsec = 0.0d0                                                      TINI 32
      zhgt = 0.0D0                                                      TINI 33
      zlat = -31.3D0                                                    TINI 34
      zlon = 317.0D0                                                    TINI 35
C...  Set the time increment (dt, sec)                                  TINI 36
      dt = 500.0D0                                                      TINI 37
C...  Compute the northward velocity. Convert to a rate of change of    TINI 38
C     latitude with time (vlat, deg./s)                                 TINI 39
      vlat = 0.0006D0                                                   TINI 40
C...  Compute eastward velocity. Convert to rate of change of longitude TINI 41
C     with time (vlon, deg./s)                                          TINI 42
      vlon = 0.001D0                                                    TINI 43
C...  Set vertical velocity (vz, km/s) and "acceleration" (az, km/s**2) TINI 44
      vz = 0.002D0                                                      TINI 45
      az = 0.0                                                          TINI 46
C...  Alternately, set heights z1 and z2 (km) at times t1 and t2 (sec), TINI 47
C     and use vz and az computed from these data. NOTE: Height z = zhgt TINI 48
C     at time t = 0 is set in initial position, above.                  TINI 49
      t1 = 0.0d0                                                        TINI 50
      t2 = 0.0d0                                                        TINI 51
      z1 = 300.0d0                                                      TINI 52
      z2 = 1000.0d0                                                     TINI 53
      If (t1.ne.0.0.and.t2.ne.0.0.and.t1.ne.t2)Then                     TINI 54
        vz = (z1-zhgt)*t2/(t1*(t2-t1)) - (z2-zhgt)*t1/(t2*(t2-t1))      TINI 55
        az = (z2-zhgt)/(t2*(t2-t1)) - (z1-zhgt)/(t1*(t2-t1))            TINI 56
      Endif                                                             TINI 57
      Return                                                            TINI 58
      End                                                               TINI 59
C---------------------------------------------------------------------- TINI 60
