C...  Venus-GRAM 2005 Multi-Trajectory Program (Version 1)              MULT  1
C                           March, 2005                                 MULT  2
C                                                                       MULT  3
C     A program to illustrate how Venus-GRAM subroutines can be in-     MULT  4
C     corporated into a (double precision) trajectory-calculation code  MULT  5
C     that computes MULTIPLE trajectories during one run.               MULT  6
C                                                                       MULT  7
C     SUBSTITUTE YOUR OWN TRAJECORY CODE FOR THIS MAIN DRIVER AND FOR   MULT  8
C     SUBROUTINES trajest_V05 AND initcode_V05                          MULT  9
C                                                                       MULT 10
C     This main driver should be compiled and linked with subroutine    MULT 11
C     files Setup_V05.f, nptnsubs.f, and wrapper.f                      MULT 12
C                                                                       MULT 13
C     All interface between the main driver trajectory program and      MULT 14
C     Venus-GRAM is via the three calls to Venustraj_V05 (One with      MULT 15
C     isetup=1, the others with isetup = 0).  Input to Venus-GRAM       MULT 16
C     routine is via the NAMELIST FORMAT file INPUT, called in the      MULT 17
C     setup mode.  A number of Monte-Carlo trajectories (NMONTE) can be MULT 18
C     calculated during one program run. Each trajectory can have up to MULT 19
C     a maximum of MAXNUM points, or exit from a given trajectory (and  MULT 20
C     on to the next Monte-Carlo case) can be controlled by the param-  MULT 21
C     eter EOF.                                                         MULT 22
C                                                                       MULT 23
C     To suppress output of the LIST and other files make sure to use   MULT 24
C     iup = 0 on the INPUT file (or permanently change it to 0 in the   MULT 25
C     Setup_V05 subroutine, near line 67).                              MULT 26
C                                                                       MULT 27
C.....................................................................  MULT 28
C                                                                       MULT 29
      Implicit None                                                     MULT 30
      Integer EOF,iustdin,iustdout,jmonte,NMONTE,MAXNUM,NR1(6),istep,   MULT 31
     &   iupdate(6),ipr,nmulti,L,NRx                                    MULT 32
      Character*60 INPUTFL                                              MULT 33
      Double Precision zhgt(6),zlat(6),zlon(6),pzhgt(6),pzlat(6),       MULT 34
     &   pzlon(6),NSWIND(6),NSpert(6),TEMP(6),PRES(6),DENSLO(6),        MULT 35
     &   DENS(6),DENSHI(6),DENSP(6),RHOd(6),RHOu(6),RHOv(6),dens2(6),   MULT 36
     &   AMz(6),EWWIND(6),EWpert(6),DENS1(6),DSCLHGT1(6),corlim(6),     MULT 37
     &   dsclhgt2(6),fmolh(6),fmolhe(6),fmolco2(6),dt,vz,az,fmolco(6),  MULT 38
     &   vlat,vlon,pertstep(6),sigD,sigU,Random_V05,zsec(6),fmolo(6),   MULT 39
     &   pzsec(6),DAY0,dsunlat,dsunlon,dsunLs,dradau,dowlt,ALS,SZA,     MULT 40
     &   owlt,sunlat,sunlon,VenusAU,TLOCAL,fmolN2(6),fmolN(6),PPND_V05  MULT 41
C...  Set nmulti = 1 to 6 to do multiple trajectories at once           MULT 42
      nmulti = 1                                                        MULT 43
C...  Establish unit numbers for standard (screen) input and output     MULT 44
      iustdin = 5                                                       MULT 45
      iustdout = 6                                                      MULT 46
C...  Establish the name of the NAMELIST format input file              MULT 47
      Write(iustdout,*)' Enter file name for NAMELIST input'            MULT 48
      Read(iustdin,10)INPUTFL                                           MULT 49
  10  Format(A)                                                         MULT 50
C                                                                       MULT 51
C     Set ephemeris values to 0 if using built-in ephemeris routine,    MULT 52
C     otherwise compute high-precision values here                      MULT 53
      dsunlat = 0.0d0                                                   MULT 54
      dsunlon = 0.0d0                                                   MULT 55
      dsunLs = 0.0d0                                                    MULT 56
      dradau = 0.0d0                                                    MULT 57
      dowlt = 0.0d0                                                     MULT 58
C                                                                       MULT 59
C     Set up the Venus-GRAM routine (ISETUP = 1)                        MULT 60
      iupdate(1) = 0                                                    MULT 61
      Call Venustraj_V05(1,0,NMONTE,0,MAXNUM,zhgt(1),zlat(1),zlon(1),   MULT 62
     & zsec(1),pzhgt(1),pzlat(1),pzlon(1),pzsec(1),DAY0,                MULT 63
     & TEMP(1),PRES(1),DENSLO(1),DENS(1),DENSHI(1),DENSP(1),RHOd(1),    MULT 64
     & RHOu(1),RHOv(1),EWWIND(1),EWpert(1),NSWIND(1),NSpert(1),EOF,     MULT 65
     & NR1(1),DENS1(1),DSCLHGT1(1),corlim(1),INPUTFL,iustdout,          MULT 66
     & fmolco2(1),fmolN2(1),fmolO(1),fmolCO(1),fmolHe(1),fmolN(1),      MULT 67
     & fmolH(1),AMz(1),iupdate(1),pertstep(1),dsunlat,dsunlon,dsunLs,   MULT 68
     & dradau,dowlt,ALS,SZA,owlt,sunlat,sunlon,VenusAU,TLOCAL)          MULT 69
      If (nmulti.gt.1)Then                                              MULT 70
        Do 20 ipr = 2,nmulti                                            MULT 71
         zsec(ipr) = zsec(ipr-1)                                        MULT 72
         zhgt(ipr)=zhgt(ipr-1)                                          MULT 73
         zlat(ipr)=zlat(ipr-1)                                          MULT 74
         zlon(ipr)=zlon(ipr-1)                                          MULT 75
         pzhgt(ipr)=pzhgt(ipr-1)                                        MULT 76
         pzlat(ipr)=pzlat(ipr-1)                                        MULT 77
         pzlon(ipr)=pzlon(ipr-1)                                        MULT 78
         pzsec(ipr)=pzsec(ipr-1)                                        MULT 79
         TEMP(ipr)=TEMP(ipr-1)                                          MULT 80
         PRES(ipr)=PRES(ipr-1)                                          MULT 81
         DENSLO(ipr)=DENSLO(ipr-1)                                      MULT 82
         DENS(ipr)=DENS(ipr-1)                                          MULT 83
         DENSHI(ipr)=DENSHI(ipr-1)                                      MULT 84
         DENSP(ipr)=DENSP(ipr-1)                                        MULT 85
         RHOd(ipr)=RHOd(ipr-1)                                          MULT 86
         RHOu(ipr)=RHOu(ipr-1)                                          MULT 87
         RHOv(ipr)=RHOv(ipr-1)                                          MULT 88
         EWWIND(ipr)=EWWIND(ipr-1)                                      MULT 89
         EWpert(ipr)=EWpert(ipr-1)                                      MULT 90
         NSWIND(ipr)=NSWIND(ipr-1)                                      MULT 91
         NSpert(ipr)=NSpert(ipr-1)                                      MULT 92
         DENS1(ipr)=DENS1(ipr-1)                                        MULT 93
         DSCLHGT1(ipr)=DSCLHGT1(ipr-1)                                  MULT 94
         fmolCO2(ipr)=fmolCO2(ipr-1)                                    MULT 95
         fmolN2(ipr)=fmolN2(ipr-1)                                      MULT 96
         fmolO(ipr)=fmolO(ipr-1)                                        MULT 97
         fmolCO(ipr)=fmolCO(ipr-1)                                      MULT 98
         fmolHe(ipr)=fmolHe(ipr-1)                                      MULT 99
         fmolN(ipr)=fmolN(ipr-1)                                        MULT100
         fmolH(ipr)=fmolH(ipr-1)                                        MULT101
         AMz(ipr)=AMz(ipr-1)                                            MULT102
         pertstep(ipr)=pertstep(ipr-1)                                  MULT103
         iupdate(ipr)=iupdate(ipr-1)                                    MULT104
C...     To generate different perturbations adjust NR1(ipr) for ipr>1  MULT105
         NR1(ipr) = NR1(ipr-1) + 7*ipr                                  MULT106
  20    Enddo                                                           MULT107
      Endif                                                             MULT108
C                                                                       MULT109
C...  Step through Monte Carlo runs                                     MULT110
      Do 200 jmonte = 1,NMONTE                                          MULT111
C       Initialize position, time step, velocity, and acceleration      MULT112
        Call initcode_V05(zsec(1),zhgt(1),zlat(1),zlon(1),dt,vz,az,     MULT113
     &    vlat,vlon)                                                    MULT114
C...    Set double precision ephemeris values to 0 if using built-in    MULT115
C       ephemeris routine, otherwise compute high precision values      MULT116
        dsunlat = 0.0d0                                                 MULT117
        dsunlon = 0.0d0                                                 MULT118
        dsunLs = 0.0d0                                                  MULT119
        dradau = 0.0d0                                                  MULT120
        dowlt = 0.0d0                                                   MULT121
C       Initialize the Venus-GRAM variables (istep = 0) and compute     MULT122
C       density at beginning of first trajectory step                   MULT123
C                                                                       MULT124
        NRx = NR1(1)                                                    MULT125
C                                                                       MULT126
C.....................................................................  MULT127
C       If the user wants to have one or more profiles where random     MULT128
C       perturbations are the same, then insert required logic here.    MULT129
C                                                                       MULT130
C       Examples:                                                       MULT131
C.....................................................................  MULT132
C                                                                       MULT133
C       To do a sequence of pairs of profiles, with the second member   MULT134
C       of each pair having the same perturbations as the first of      MULT135
C       the pair, use -                                                 MULT136
C                                                                       MULT137
C       If (Mod(jmonte,2).eq.0)NRx = NR1(1) - 11                        MULT138
C                                                                       MULT139
C       To do a sequence of profiles with all members of the sequence   MULT140
C       having the same perturbations as the first profile, use -       MULT141
C                                                                       MULT142
C       If (jmonte.gt.1)NRx = NR1(1) - 11                               MULT143
C                                                                       MULT144
C       The algorithm used here (NRx = NR1 - 11) must be the            MULT145
C       reverse of the algorithm used in subroutine Randinit_V05 to     MULT146
C       increase NR1 between successive profiles (at line RNDI  8).     MULT147
C                                                                       MULT148
C.....................................................................  MULT149
C                                                                       MULT150
        Call Venustraj_V05(0,jmonte,NMONTE,0,MAXNUM,zhgt(1),zlat(1),    MULT151
     &   zlon(1),zsec(1),zhgt(1),zlat(1),zlon(1),zsec(1),DAY0,          MULT152
     &   TEMP(1),PRES(1),DENSLO(1),DENS(1),DENSHI(1),DENSP(1),          MULT153
     &   RHOd(1),RHOu(1),RHOv(1),EWWIND(1),EWpert(1),NSWIND(1),         MULT154
     &   NSpert(1),EOF,NR1(1),DENS1(1),DSCLHGT1(1),corlim(1),INPUTFL,   MULT155
     &   iustdout,fmolCO2(1),fmolN2(1),fmolO(1),fmolCO(1),fmolHe(1),    MULT156
     &   fmolN(1),fmolH(1),AMz(1),iupdate(1),pertstep(1),dsunlat,       MULT157
     &   dsunlon,dsunLs,dradau,dowlt,ALS,SZA,owlt,sunlat,sunlon,        MULT158
     &   VenusAU,TLOCAL)                                                MULT159
        NR1(1) = NRx                                                    MULT160
        If (nmulti.gt.1)Then                                            MULT161
          sigD = 50.0d0*(DENSHI(1)-DENSLO(1))/DENS(1)                   MULT162
          sigU = 7.0d0 + 0.14d0*zhgt(1)                                 MULT163
          If (sigU.gt.45.0d0)sigU = 45.0d0                              MULT164
          Do 55 ipr = 2,nmulti                                          MULT165
           zsec(ipr) = zsec(ipr-1)                                      MULT166
           zhgt(ipr)=zhgt(ipr-1)                                        MULT167
           zlat(ipr)=zlat(ipr-1)                                        MULT168
           zlon(ipr)=zlon(ipr-1)                                        MULT169
           pzhgt(ipr)=pzhgt(ipr-1)                                      MULT170
           pzlat(ipr)=pzlat(ipr-1)                                      MULT171
           pzlon(ipr)=pzlon(ipr-1)                                      MULT172
           pzsec(ipr) = pzsec(ipr-1)                                    MULT173
           TEMP(ipr)=TEMP(ipr-1)                                        MULT174
           PRES(ipr)=PRES(ipr-1)                                        MULT175
           DENSLO(ipr)=DENSLO(ipr-1)                                    MULT176
           DENS(ipr)=DENS(ipr-1)                                        MULT177
           DENSHI(ipr)=DENSHI(ipr-1)                                    MULT178
           RHOd(ipr) = RANDOM_V05(L)                                    MULT179
           RHOd(ipr) = PPND_V05(RHOd(ipr),L)                            MULT180
           DENSP(ipr) = RHOd(ipr)*sigD                                  MULT181
           RHOu(ipr) = RANDOM_V05(L)                                    MULT182
           RHOu(ipr) = PPND_V05(RHOu(ipr),L)                            MULT183
           EWpert(ipr) = RHOu(ipr)*sigU                                 MULT184
           RHOv(ipr) = RANDOM_V05(L)                                    MULT185
           RHOv(ipr) = PPND_V05(RHOv(ipr),L)                            MULT186
           NSpert(ipr) = RHOv(ipr)*sigU                                 MULT187
           EWWIND(ipr)=EWWIND(ipr-1)                                    MULT188
           NSWIND(ipr)=NSWIND(ipr-1)                                    MULT189
           DENS1(ipr)=DENS1(ipr-1)                                      MULT190
           DSCLHGT1(ipr)=DSCLHGT1(ipr-1)                                MULT191
           fmolCO2(ipr)=fmolCO2(ipr-1)                                  MULT192
           fmolN2(ipr)=fmolN2(ipr-1)                                    MULT193
           fmolO(ipr)=fmolO(ipr-1)                                      MULT194
           fmolCO(ipr)=fmolCO(ipr-1)                                    MULT195
           fmolHe(ipr)=fmolHe(ipr-1)                                    MULT196
           fmolN(ipr)=fmolN(ipr-1)                                      MULT197
           fmolH(ipr)=fmolH(ipr-1)                                      MULT198
           AMz(ipr)=AMz(ipr-1)                                          MULT199
           pertstep(ipr)=pertstep(ipr-1)                                MULT200
           iupdate(ipr)=iupdate(ipr-1)                                  MULT201
C...       To generate different perturbations adjust NR1(ipr), ipr>1   MULT202
           NR1(ipr) = NR1(ipr-1) + 7*ipr                                MULT203
  55      Enddo                                                         MULT204
        Endif                                                           MULT205
C       TEMP, PRES, DENS are mean temperature, pressure, density at     MULT206
C       beginning of 1st trajectory step; DENS1 is total (mean plus     MULT207
C       perturbed) density at beginning of 1st trajectory step          MULT208
      Do 100 istep = 1,MAXNUM                                           MULT209
C       Estimate next trajectory position (based on 1st predictor       MULT210
C       calculation and total density at beginning of trajectory step)  MULT211
C       and store previous position for which Venus-GRAM was called.    MULT212
C                                                                       MULT213
C       If using externally-calculated, high-precision ephemeris        MULT214
C       routine, recalculate values here                                MULT215
C                                                                       MULT216
        Do 70  ipr = 1,nmulti                                           MULT217
        Call trajest_V05(zsec(ipr),zhgt(ipr),zlat(ipr),zlon(ipr),       MULT218
     &   pzsec(ipr),pzhgt(ipr), pzlat(ipr),pzlon(ipr),DENS1(ipr),       MULT219
     &   DSCLHGT1(ipr),vz,az,vlat,vlon,dt,istep-1)                      MULT220
C                                                                       MULT221
C       *** NOTE - If computed longitude zlon is positive West, then    MULT222
C       LonEast should be set to 0 in the NAMELIST input file (read in  MULT223
C       by the Setup_V05 subroutine) ***                                MULT224
C                                                                       MULT225
C       Evaluate density at estimated next trajectory position          MULT226
        Call Venustraj_V05(0,0,NMONTE,istep,MAXNUM,zhgt(ipr),zlat(ipr), MULT227
     &   zlon(ipr),zsec(ipr),pzhgt(ipr),pzlat(ipr),pzlon(ipr),          MULT228
     &   pzsec(ipr),Day0,TEMP(ipr),PRES(ipr),DENSLO(ipr),               MULT229
     &   DENS(ipr),DENSHI(ipr),DENSP(ipr),RHOd(ipr),RHOu(ipr),          MULT230
     &   RHOv(ipr),EWWIND(ipr),EWpert(ipr),NSWIND(ipr),NSpert(ipr),EOF, MULT231
     &   NR1(ipr),DENS2(ipr),DSCLHGT2(ipr),corlim(ipr),INPUTFL,         MULT232
     &   iustdout,fmolCO2(ipr),fmolN2(ipr),fmolO(ipr),fmolCO(ipr),      MULT233
     &   fmolHe(ipr),fmolN(ipr),fmolH(ipr),AMz(ipr),iupdate(ipr),       MULT234
     &   pertstep(ipr),dsunlat,dsunlon,dsunLs,dradau,dowlt,ALS,SZA,     MULT235
     &   owlt,sunlat,sunlon,VenusAU,TLOCAL)                             MULT236
C       TEMP, PRES, DENS are mean temperature, pressure, density at     MULT237
C       estimated next trajectory position; DENS2 is total (mean plus   MULT238
C       perturbed) density at estimated next trajectory position        MULT239
C                                                                       MULT240
C       If the user desires to revise the trajectory position estimate  MULT241
C       (zhgt, zlat, zlon), this can be done here (e.g. via a corrector MULT242
C       step in a predictor-corrector solution and/or via a scheme      MULT243
C       that uses variation of total density along the trajectory step, MULT244
C       i.e. between DENS1 at the beginning of the step and DENS2 at    MULT245
C       the end of the step).  Interpolation between DENS1 and DENS2    MULT246
C       can be done in an appropriate manner as selected by the user    MULT247
C       (e.g. linear interpolation may be valid for small trajectory    MULT248
C       time steps). No further calls to Venustraj_V05 (except as noted MULT249
C       below, using iupdate < 0)should be done during this predictor-  MULT250
C       corrector process of updating zhgt, zlat,and zlon (as this will MULT251
C       lead to erroneous results for the density perturbation values). MULT252
C                                                                       MULT253
C       Store total density and scale height for use as value at        MULT254
C       beginning of next trajectory step                               MULT255
        DENS1(ipr) = DENS2(ipr)                                         MULT256
        DSCLHGT1(ipr) = DSCLHGT2(ipr)                                   MULT257
C                                                                       MULT258
C       If the user wishes to call the atmosphere model without         MULT259
C       updating the perturbations or total perturbation step size,     MULT260
C       this can be done here by using iupdate < 0                      MULT261
C                                                                       MULT262
C     Example:                                                          MULT263
C...................................................................... MULT264
C       iupdate = -1                                                    MULT265
C       Call Venustraj_V05(0,0,NMONTE,istep,MAXNUM,(zhgt(ipr)+          MULT266
C    &   pzhgt(ipr))/2.0d0,(zlat(ipr)+pzlat(ipr))/2.0d0,(zlon(ipr)+     MULT267
C    &   pzlon(ipr))/2.0d0,(zsec(ipr)+zsec(ipr))/2.0d0,pzhgt(ipr),      MULT268
C    &   pzlat(ipr),pzlon(ipr),pzsec(ipr),Day0,TEMP(ipr),PRES(ipr),     MULT269
C    &   DENSLO(ipr),DENS(ipr),DENSHI(ipr),DENSP(ipr),RHOd(ipr),        MULT270
C    &   RHOu(ipr),RHOv(ipr),EWWIND(ipr),EWpert(ipr),NSWIND(ipr),       MULT271
C    &   NSpert(ipr),EOF,NR1(ipr),DENS2(ipr),DSCLHGT2(ipr),corlim(ipr), MULT272
C    &   INPUTFL,iustdout,fmolCO2(ipr),fmolN2(ipr),fmolO(ipr),          MULT273
C    &   fmolCO(ipr),fmolHe(ipr),fmolN(ipr),fmolH(ipr),AMz(ipr),        MULT274
C    &   iupdate(ipr),pertstep(ipr),dsunlat,dsunlon,dsunLs,dradau,      MULT275
C    &   dowlt,ALS,SZA,owlt,sunlat,sunlon,VenusAU,TLOCAL)               MULT276
C       iupdate(ipr) = 0                                                MULT277
C...................................................................... MULT278
  70    Enddo                                                           MULT279
C       End this trajectory calculation if parameter EOF is 1           MULT280
        If (EOF.eq.1)Goto 200                                           MULT281
 100  Continue                                                          MULT282
 200  Continue                                                          MULT283
      End                                                               MULT284
C---------------------------------------------------------------------  MULT285
C                                                                       MULT286
C     NOTE: The Venustraj_V05 subroutine should be incorporated into    MULT287
C     your trajectory code, as a "wrapper" routine for calling          MULT288
C     Venus-GRAM setup and subroutines. The main routine and all        MULT289
C     other subroutines in multtraj are for illustrative purposes only  MULT290
C     and are not needed in your trajectory program.                    MULT291
C                                                                       MULT292
C---------------------------------------------------------------------  MULT293
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
C     Evaluate new time t, using time increment, dt                     TRAJ 32
      t = pt + dt                                                       TRAJ 33
C...  Use vz and az to compute height change by Newton's forward        TRAJ 34
C     difference                                                        TRAJ 35
      z = pz + vz*dt + az*(2.0d0*n + 1.0d0)*dt**2                       TRAJ 36
C...  Use vlat and vlon to compaute latitude-longitude change           TRAJ 37
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
     &  zsec                                                            TINI  4
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
C     If constant vz and az = 0 are desired above, set t1=t2=0.0 here   TINI 52
      z1 = 300.0d0                                                      TINI 53
      z2 = 1000.0d0                                                     TINI 54
      If (t1.ne.0.0.and.t2.ne.0.0.and.t1.ne.t2)Then                     TINI 55
        vz = (z1-zhgt)*t2/(t1*(t2-t1)) - (z2-zhgt)*t1/(t2*(t2-t1))      TINI 56
        az = (z2-zhgt)/(t2*(t2-t1)) - (z1-zhgt)/(t1*(t2-t1))            TINI 57
      Endif                                                             TINI 58
      Return                                                            TINI 59
      End                                                               TINI 60
C---------------------------------------------------------------------- TINI 61
