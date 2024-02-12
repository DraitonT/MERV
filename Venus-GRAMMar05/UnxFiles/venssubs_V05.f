      Subroutine CaltoJul_V05(iY,iM,iD,ihour,imin,sec,xJD)              CTOJ  1
C                                                                       CTOJ  2
C     Compute Julian day (Real*8) by method of Meeus, Astronomical      CTOJ  3
C       Algorithms, 2nd Edition, 1998, page 61. Inputs are year iY,     CTOJ  4
C       month iM, day of month iD, and time of day in hours, minutes,   CTOJ  5
C       and seconds (all integer except seconds).  Output is Real*8     CTOJ  6
C       Julian day, xJD.                                                CTOJ  7
C                                                                       CTOJ  8
      Implicit None                                                     CTOJ  9
      Double Precision sec,xJD,D                                        CTOJ 10
      Integer iY,iM,iD,ihour,imin,Y,M,A,B                               CTOJ 11
      Y = iY                                                            CTOJ 12
      M = iM                                                            CTOJ 13
C...  Consider Jan or Feb as if months 13 and 14 of previous year       CTOJ 14
      If (iM.le.2)Then                                                  CTOJ 15
        Y = iY - 1                                                      CTOJ 16
        M = iM + 12                                                     CTOJ 17
      Endif                                                             CTOJ 18
C...  Compute day of month plus fractional part                         CTOJ 19
      D = iD + ihour/2.4d1 + imin/1.440d3 + sec/8.64D4                  CTOJ 20
      A = IDint(Y/100.0d0)                                              CTOJ 21
      B = 2 - A + IDint(A/4.0d0)                                        CTOJ 22
C...  Compute Julian day with fractional part                           CTOJ 23
      xJD = IDint(365.25d0*(Y+4716)) + IDint(30.6001d0*(M+1))           CTOJ 24
     &  + D + B - 1524.5d0                                              CTOJ 25
      Return                                                            CTOJ 26
      End                                                               CTOJ 27
C---------------------------------------------------------------------- CTOJ 28
      Subroutine Datastep_V05(I,CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,     DSTP  1
     & RHOv,EOF,DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,DENS,     DSTP  2
     & DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,Hrho,HSCALE,dsunlat,    DSTP  3
     & dsunlon,dsunLs,dradau,dowlt,LonEast,corlim,DENSTOT,IERT,IUTC,    DSTP  4
     & fmolCO2,fmolN2,fmolO,fmolCO,fmolHe,fmolN,fmolH,AMz,pertstep,     DSTP  5
     & corlmin,iupdate,ALS,SZA,owlt,sunlat,sunlon,VenusAU,TLOCAL,       DSTP  6
     & profnear,proffar,nprof)                                          DSTP  7
C...  Subroutine to compute atmosphere at next trajectory or profile    DSTP  8
C     position, including perturbations, with appropriate correlation   DSTP  9
C     maintained over step size                                         DSTP 10
      Implicit None                                                     DSTP 11
      Double Precision CHGT,CLAT,CLON,DELHGT,DELLAT,DELLON,DTR,Rref,    DSTP 12
     &  NSWIND,NSpert,NStot,TEMP,PRES,DENSLO,DENS,DENSHI,DENSP,EWWIND,  DSTP 13
     &  EWpert,Hrho,HSCALE,corlim,DENSTOT,pi,gz,yo,yco,yn,amo,amco,amn, DSTP 14
     &  ysum,yh,amh,yhe,amhe,yco2,amco2,devtot,ytot,amz,presmb,         DSTP 15
     &  patsurf,sigmalevel,devhi,varx,vary,pvra,alogdens,var,sza,       DSTP 16
     &  yhp,yhep,yco2p,devav,preshgt,olon,ppnd_V05,random_V05,ogz,      DSTP 17
     &  dvra,devlo,tvra,rhou,rhov,ewtot,yop,ycop,ynp,                   DSTP 18
     &  ssp2,fc,fa,fb,fact,wlon,z1,rhod,densrand,rpscale,sigd,sigu,     DSTP 19
     &  sigv,trajlat,trajlon,correl,facthi,dplus,factlo,dminus,z2,      DSTP 20
     &  rsc,vls,hls,delns,delew,delz,trajhgt,fmolh,fmolhe,              DSTP 21
     &  pertstep,corlmin,corbeta,yn2,amn2,yn2p,profnear,proffar,        DSTP 22
     &  CSEC,DAY0,DELTIME,dsunlat,dsunlon,dsunLs,dradau,dowlt,profwgt,  DSTP 23
     &  TrajSEC,DAY,VenusAU,owlt,sunlat,sunlon,ALS,EOT,ttsec,dt,        DSTP 24
     &  TLOCAL,DayVenus,zeta,fmolCO2,fmolN2,fmolO,fmolCO,fmolN,         DSTP 25
     &  zlo,plo,dlo,tlo,zetalo,Rlo,zmd,pmd,dmd,tmd,yco2md,yn2md,yomd,   DSTP 26
     &  ycomd,yhemd,ynmd,zhi,phi,dhi,thi,yco2hi,yn2hi,yohi,             DSTP 27
     &  ycohi,yhehi,ynhi,yhhi                                           DSTP 28
      INTEGER EOF,LonEast,logscale,nvarx,nvary,iu0,iup,maxfiles,L,      DSTP 29
     &  ifault,i,npos,iupdate,IERT,IUTC,nprof                           DSTP 30
      Character*60 lstfl,outfl                                          DSTP 31
      Character*8 densunits                                             DSTP 32
C...  Venus model VIRA data at low ,middle, and high altitudes          DSTP 33
      Common /VIRAdata_V05/zlo(81),plo(81,5),dlo(81,5),tlo(81,5),       DSTP 34
     &  zetalo(81,5),Rlo(81,5),zmd(11),pmd(11,2),dmd(11,2),tmd(11,2),   DSTP 35
     &  yco2md(11,2),yn2md(11,2),yomd(11,2),ycomd(11,2),yhemd(11,2),    DSTP 36
     &  ynmd(11,2),zhi(21),phi(21,7),dhi(21,7),                         DSTP 37
     &  thi(21,7),yco2hi(21,7),yn2hi(21,7),yohi(21,7),ycohi(21,7),      DSTP 38
     &  yhehi(21,7),ynhi(21,7),yhhi(21,7)                               DSTP 39
      COMMON /DATACOM_V05/rpscale,NPOS,NVARX,NVARY,logscale,iu0,iup,    DSTP 40
     & maxfiles                                                         DSTP 41
      COMMON /FILENAME_V05/lstfl,outfl                                  DSTP 42
C...  Molecular weights for atmospheric constituents                    DSTP 43
      Data amh,amco2,amhe,amn2,amo,amco,amn/1.01d0,44.0d0,4.0d0,28.0d0, DSTP 44
     &  16.0d0,28.0d0,14.0d0/                                           DSTP 45
      DTR = DAtan(1.0d0)/45.0d0                                         DSTP 46
      pi = 4.0d0*dAtan(1.0d0)                                           DSTP 47
      EOF = 0                                                           DSTP 48
C...  Venus day length, seconds                                         DSTP 49
      DayVenus = 1.00872d7                                              DSTP 50
C...  Get radius RSC to current position                                DSTP 51
      Call rgplanet_V05(CLAT,Rref,CHGT,gz,2)                            DSTP 52
      RSC = Rref + CHGT                                                 DSTP 53
C...  Set vertical and horizontal scale parameters                      DSTP 54
C     Vertical scale approximates VLS/Hrho = 2                          DSTP 55
      VLS = 32.0d0  - 22.0d0*CHGT/65.0d0                                DSTP 56
      If (VLS.lt.10.0d0)VLS = 10.0d0                                    DSTP 57
      If (CHGT.gt.150.0d0)VLS = 10.0d0 + 0.6d0*(CHGT-150.0d0)           DSTP 58
C...  Horizontal scale selected to be fairly consistent with wavelength DSTP 59
C     estimates from Fig. 6 of Bougher and Borucki, J. Geophys. Res.    DSTP 60
C     99(E2), 3759-3776 (1994), Fig. 2 of Mayr et al., J. Geophys. Res. DSTP 61
C     93(A10), 11247-262 (1988), Kasprzak et al. J. Geophys. Res.       DSTP 62
C     93(A10), 11237-246 (1988), and Kasprzak et al. Geophys. Res.      DSTP 63
C     Lett. 20 2755-2758 (1993)                                         DSTP 64
      HLS = 60.0d0*VLS                                                  DSTP 65
C...  Relative displacements between previous and current position      DSTP 66
      If (I.ne.0)Then                                                   DSTP 67
        DELNS = DTR*RSC*(DELLAT)/HLS                                    DSTP 68
        DELEW = -DTR*RSC*dCOS(DTR*CLAT)*DELLON/HLS                      DSTP 69
        DELZ = DELHGT/VLS                                               DSTP 70
      Else                                                              DSTP 71
        DELNS = 0.0                                                     DSTP 72
        DELEW = 0.0                                                     DSTP 73
        DELZ = 0.0                                                      DSTP 74
      Endif                                                             DSTP 75
      IF(NPOS.le.0)then                                                 DSTP 76
C...    Read new position if trajectory data file is being used         DSTP 77
        READ (7,*,ERR=9998,END=999)TrajSEC,TrajHGT,TrajLAT,TrajLON      DSTP 78
C...    Convert negative longitudes                                     DSTP 79
        If (TrajLON.lt.0.0d0)TrajLON = TrajLON + 360.0d0                DSTP 80
C...    Convert to East Longitude if LonEast = 0                        DSTP 81
        If (LonEast.eq.0)TrajLON = 360.0d0 - TrajLON                    DSTP 82
        If (I.gt.0)Then                                                 DSTP 83
C...      Compute displacement magnitude of new from previous position  DSTP 84
          DELHGT = TrajHGT - CHGT                                       DSTP 85
          DELLAT = dAbs(TrajLAT - CLAT)                                 DSTP 86
          DELLON = dAbs(TrajLON - CLON)                                 DSTP 87
          DELTIME = TrajSEC - CSEC                                      DSTP 88
        Endif                                                           DSTP 89
C...    Correct DELLON for cases near 0/360 longitude discontinuity     DSTP 90
        If (DELLON.gt.180.0d0)DELLON = 360.0d0 - DELLON                 DSTP 91
        If (DELLON.lt.0.0d0)DELLON = DELLON + 360.0d0                   DSTP 92
C...    Correct DELLON and DELLAT near polar discontinuities            DSTP 93
        If (DELLON.gt.90.0d0.and.(dAbs(TrajLAT).ge.70.0d0.or.           DSTP 94
     &   dAbs(CLAT).ge.70.0d0))Then                                     DSTP 95
          DELLON = dAbs(180.0d0 - DELLON)                               DSTP 96
          DELLAT = 180.0d0 - dAbs(TrajLAT) - dAbs(CLAT)                 DSTP 97
        Endif                                                           DSTP 98
C...    Relative displacements between previous and current position    DSTP 99
        DELNS = DTR*RSC*(DELLAT)/HLS                                    DSTP100
        DELEW = -DTR*RSC*dCOS(DTR*CLAT)*DELLON/HLS                      DSTP101
        DELZ = DELHGT/VLS                                               DSTP102
C...    Set current position to new position                            DSTP103
        CHGT = TrajHGT                                                  DSTP104
        CLAT = TrajLAT                                                  DSTP105
        CLON = TrajLON                                                  DSTP106
        CSEC = TrajSEC                                                  DSTP107
      Else If (I.gt.0) then                                             DSTP108
        CHGT = CHGT + DELHGT                                            DSTP109
        CLAT = CLAT + DELLAT                                            DSTP110
        CLON = CLON + DELLON                                            DSTP111
        CSEC = CSEC + DELTIME                                           DSTP112
      Endif                                                             DSTP113
C...  Update Julian day                                                 DSTP114
      DAY = DAY0 + CSEC/8.64d4                                          DSTP115
C...  Get radius RSC to new position                                    DSTP116
      Call rgplanet_V05(CLAT,Rref,CHGT,ogz,2)                           DSTP117
      RSC = Rref + CHGT                                                 DSTP118
C...  Correct latitude and longitude if position crosses either pole    DSTP119
      IF(dABS(CLAT).GT.90.0d0)then                                      DSTP120
        CLAT = DSIGN(180.0d0,CLAT) - CLAT                               DSTP121
        CLON = CLON + 180.0d0                                           DSTP122
        DELLAT = -DELLAT                                                DSTP123
      Endif                                                             DSTP124
      IF(CLON.LT.0.0d0)CLON = CLON + 360.0d0                            DSTP125
      IF(CLON.GE.360.0d0) CLON = CLON - 360.0d0                         DSTP126
C...  Sun and Venus positions at new time                               DSTP127
C...  Use input ephemeris values, if supplied                           DSTP128
      If (dradau.gt.0.0)Then                                            DSTP129
        SUNLON = dsunlon                                                DSTP130
        SUNLAT = dsunlat                                                DSTP131
        ALS = dsunLs                                                    DSTP132
        VenusAU = dradau                                                DSTP133
        owlt = dowlt                                                    DSTP134
      Else                                                              DSTP135
C...    Use built-in Venus ephemeris routine                            DSTP136
C...    Convert to Terrestrial (Dynamical) Time, if necessary           DSTP137
        ttsec = 0.0                                                     DSTP138
        If (iutc.eq.1)Then                                              DSTP139
C...      Get terrestrial dynamical time offset (seconds)               DSTP140
          dt = (DAY - 2451545.0d0)/36525.0d0                            DSTP141
C...      Terrestrial time offset (in seconds) TT = UTC + ttsec         DSTP142
          ttsec = (64.184d0 + 95.0d0*dt + 35.0d0*dt**2)/86400.0d0       DSTP143
        Endif                                                           DSTP144
        Call venephem_V05(DAY+ttsec,sunlat,sunlon,ALS,VenusAU,owlt,EOT) DSTP145
C...    Convert to Venus-Event Time, if necessary                       DSTP146
        If(iert.eq.1)Call venephem_V05(DAY+ttsec-owlt/1440.0d0,sunlat,  DSTP147
     &    sunlon,ALS,VenusAU,owlt,EOT)                                  DSTP148
      Endif                                                             DSTP149
C...  Local true solar time in Venus hours (1/24th Venus day)           DSTP150
      TLOCAL = 12.0d0 + (SUNLON - CLON)/15.0d0                          DSTP151
      IF (TLOCAL .LT. 0.)TLOCAL = TLOCAL + 24.                          DSTP152
      IF (TLOCAL .GT. 24.)TLOCAL = TLOCAL - 24.                         DSTP153
C...  Get solar zenith angle                                            DSTP154
      Call  SolZenAng_V05(sunlat,sunlon,clat,clon,sza)                  DSTP155
C...  Evaluate Venus engineering model atmospheric parameters           DSTP156
      Call Venusatm_V05(CHGT,CLAT,CLON,TLOCAL,sza,HSCALE,TEMP,DENS,     DSTP157
     &  FACTHI,FACTLO,PRES,Hrho,AMz,EWWIND,NSWIND,ytot,yco2,yn2,yo,yco, DSTP158
     &  yhe,yn,yh,profnear,proffar,nprof,profwgt)                       DSTP159
C...  Evaluate correlation and step size relative to accuracy limit     DSTP160
      If (iupdate.ge.0)pertstep=pertstep+dAbs(DELNS) + dAbs(DELEW)      DSTP161
     &  + dAbs(DELZ)+dSqrt(EWWIND**2+NSWIND**2)*DELTIME/(1000.0d0*HLS)  DSTP162
      corlim = -pertstep/dlog(0.995d0)                                  DSTP163
      If (corlim.le.corlmin.or.iupdate.lt.0)Then                        DSTP164
        CORREL = 1.0d0                                                  DSTP165
        corbeta = 0.0                                                   DSTP166
        If (iupdate.lt.0)Then                                           DSTP167
          iupdate = -1                                                  DSTP168
        Else                                                            DSTP169
          iupdate = 0                                                   DSTP170
        Endif                                                           DSTP171
      Else                                                              DSTP172
C...    Get uniform and Gaussian random numbers from PPND_V05 and       DSTP173
C       RANDOM_V05                                                      DSTP174
  480   Z2 = RANDOM_V05(L)                                              DSTP175
        IF (L .EQ. 1)GOTO 480                                           DSTP176
        Z1 = PPND_V05(Z2,IFAULT)                                        DSTP177
        IF (IFAULT .EQ. 1)STOP ' PPND ERROR'                            DSTP178
        CORREL = dExp(-pertstep)                                        DSTP179
        corbeta = DSqrt(1.0d0 - CORREL**2)                              DSTP180
        pertstep = 0.0                                                  DSTP181
        iupdate = 1                                                     DSTP182
      Endif                                                             DSTP183
C...  Compute Density plus ~ 1 sigma                                    DSTP184
      DENSHI = DENS*FACTHI                                              DSTP185
      DPLUS = DENSHI - DENS                                             DSTP186
C...  Compute Density minus ~ 1 sigma                                   DSTP187
      DENSLO = DENS*FACTLO                                              DSTP188
      DMINUS = DENS - DENSLO                                            DSTP189
C...  Current random density perturbation value, correlated with        DSTP190
C...  previous random density perturbation                              DSTP191
      RHOd = CORREL*RHOd + corbeta*Z1                                   DSTP192
      IF(RHOd.LT.0.0d0)DensRand = RHOd*DMINUS*rpscale                   DSTP193
      IF(RHOd.GE.0.0d0)DensRand = RHOd*DPLUS*rpscale                    DSTP194
C...  Add random density perturbation                                   DSTP195
      DENSP = DENS + DensRand                                           DSTP196
C...  Check upper and lower bounds on density perturbations             DSTP197
      If (DENSP .lt. 0.1d0*DENS)DENSP = 0.1d0*DENS                      DSTP198
      If (DENSP .gt. 10.0d0*DENS)DENSP = 10.0d0*DENS                    DSTP199
C...  Save as total density, for output                                 DSTP200
      DENSTOT = DENSP                                                   DSTP201
C...  Standard deviation in random density perturbation (% of           DSTP202
C...  unperturbed mean) for output                                      DSTP203
      SIGD = rpscale*50.0d0*dAbs(DENSHI-DENSLO)/DENS                    DSTP204
C...  Adjust random DENSHI, DENSLO for rpscale                          DSTP205
      DENSHI = DENS + rpscale*(DENSHI - DENS)                           DSTP206
      DENSLO = DENS + rpscale*(DENSLO - DENS)                           DSTP207
      If (DENSLO .lt. 0.1d0*DENS)DENSLO = 0.1d0*DENS                    DSTP208
C...  Convert random density perturbation to % of (unperturbed) mean    DSTP209
      DensRand = 100.0d0*(DENSP - DENS)/DENS                            DSTP210
C...  Convert DENSP to density perturbation % of (unpertubed) mean      DSTP211
      DENSP = DensRand                                                  DSTP212
C...  Standard deviation for wind perturbations, from approximation to  DSTP213
C     VIRA model                                                        DSTP214
      If (CHGT.le.30.0d0)Then                                           DSTP215
        SIGU = 1.0d0 + 7.0d0*CHGT/30.0d0                                DSTP216
      Else If (CHGT.le.45.0d0)Then                                      DSTP217
        SIGU = 8.0d0                                                    DSTP218
      Else If (CHGT.le.60.0d0)Then                                      DSTP219
        SIGU = 8.0d0 + 7.0d0*(CHGT-45.0d0)/15.0d0                       DSTP220
      Else If (CHGT.le.160.0d0)Then                                     DSTP221
        SIGU = 9.0d0 + CHGT/10.0d0                                      DSTP222
      Else                                                              DSTP223
        SIGU = 25.0d0                                                   DSTP224
      Endif                                                             DSTP225
      SIGU = SIGU*rpscale                                               DSTP226
      SIGV = SIGU/2.0d0                                                 DSTP227
C...  Compute EW and NS wind perturbations and total wind               DSTP228
 586  If (corbeta.ne.0.0)Then                                           DSTP229
        Z2 = RANDOM_V05(L)                                              DSTP230
        If (L.eq.1)Goto 586                                             DSTP231
        Z1 = PPND_V05(Z2,ifault)                                        DSTP232
      Endif                                                             DSTP233
      RHOu = CORREL*RHOu + corbeta*Z1                                   DSTP234
C...  EW component of perturbation in wind and total wind               DSTP235
      EWpert = RHOu*SIGU                                                DSTP236
      EWtot = EWWIND + EWpert                                           DSTP237
 587  If (corbeta.ne.0.0)Then                                           DSTP238
        Z2 = RANDOM_V05(L)                                              DSTP239
        If (L.eq.1)Goto 587                                             DSTP240
        Z1 = PPND_V05(Z2,ifault)                                        DSTP241
      Endif                                                             DSTP242
      RHOv = CORREL*RHOv + corbeta*Z1                                   DSTP243
C...  NS component of perturbation in wind and total wind               DSTP244
      NSpert = RHOv*SIGV                                                DSTP245
      NStot = NSWIND + NSpert                                           DSTP246
C...  Limit winds to sound speed                                        DSTP247
C     Assume specific heat ratio = 1.286 (CO2 - N2 mixture)             DSTP248
      ssp2 = 1.286d0*PRES/DENS                                          DSTP249
      fc = ssp2 - EWtot**2 - NStot**2                                   DSTP250
      If (fc.lt.0.0d0)Then                                              DSTP251
C...    Find multiplier factor for wind perturbations that keep total   DSTP252
C       wind speed <= speed of sound                                    DSTP253
        fc = ssp2 - EWWIND**2 - NSWIND**2                               DSTP254
        fa = EWpert**2 + NSpert**2                                      DSTP255
        If (fa.le.0.0d0)fa = 1.0d0                                      DSTP256
        fb = EWpert*EWWIND + NSpert*NSWIND                              DSTP257
        fact = (-fb + dSqrt(dAbs(fb**2 + fa*fc)))/fa                    DSTP258
        If (fact.lt.0.0d0)fact = 0.0d0                                  DSTP259
        If (fact.gt.1.0d0)fact = 1.0d0                                  DSTP260
C...    Recompute perturbations and total winds with required factor    DSTP261
        EWpert = EWpert*fact                                            DSTP262
        NSpert = NSpert*fact                                            DSTP263
        EWtot = EWWIND + EWpert                                         DSTP264
        NStot = NSWIND + NSpert                                         DSTP265
      Endif                                                             DSTP266
C...  Write descriptively formatted data on LIST.txt file               DSTP267
      wlon = 360.0d0 - CLON                                             DSTP268
      zeta = PRES*AMz/(DENS*8314.472*TEMP)                              DSTP269
      If (CHGT.gt.500.)zeta = 1.0                                       DSTP270
      If(iup.gt.0)Write(iup,590)CSEC,CSEC/DayVenus,ALS,CHGT,SZA,        DSTP271
     & RSC,Rref,owlt,HSCALE,Hrho,zeta,CLAT,CLON,wlon,SUNLAT,            DSTP272
     & VenusAU,SUNLON,TLOCAL                                            DSTP273
  590 FORMAT(' Time (rel. to T0) =',F11.1,' sec. (',F7.3,' Venus',      DSTP274
     & ' Days)    Ls =',F6.1/' Height Above Reference Ellipsoid =',F9.3 DSTP275
     & ,' km ',9x,'SZA =',F7.2,' deg'/' Total Radius (Ref Radius) = ',  DSTP276
     & F9.3,' (',F9.3,') km   OWLT =',F7.2,' Min'/' Scale Heights:',    DSTP277
     & ' H(p) =',F6.2,'       H(rho) =',F6.2,' km    zeta =',F7.4   /   DSTP278
     & ' Latitude = ',F7.2,'  degrees       Longitude = ',F7.2,' E (',  DSTP279
     & F7.2,' W) deg.'/ ' Sun Latitude = ',F8.2,' deg.      Venus',     DSTP280
     & ' Orbital Radius  =',F7.4,' AU'/' Sun Longitude = ',F7.2,        DSTP281
     & ' deg.E     Local True Solar Time =',F6.2,' Venus hr')           DSTP282
C...  Compute percent deviations from Venus average values              DSTP283
      Call Venusref_V05(CHGT,tvra,pvra,dvra)                            DSTP284
      If (dvra.le.0.0d0)Then                                            DSTP285
        devlo = -99.9d0                                                 DSTP286
        devav = -99.9d0                                                 DSTP287
        devhi = -99.9d0                                                 DSTP288
        devtot = -99.9d0                                                DSTP289
      Else                                                              DSTP290
        devlo = 100.0d0*(DENSLO-dvra)/dvra                              DSTP291
        devav = 100.0d0*(DENS-dvra)/dvra                                DSTP292
        devhi = 100.0d0*(DENSHI-dvra)/dvra                              DSTP293
        devtot = 100.0d0*(DENSTOT-dvra)/dvra                            DSTP294
      Endif                                                             DSTP295
      densunits = 'kg/m**3 '                                            DSTP296
C...  Convert density units to kg/km**3 if logscale = 3                 DSTP297
      If (logscale .eq.3)Then                                           DSTP298
        DENS = 1.0D9*DENS                                               DSTP299
        DENSLO = 1.0D9*DENSLO                                           DSTP300
        DENSHI = 1.0D9*DENSHI                                           DSTP301
        DENSTOT = 1.0D9*DENSTOT                                         DSTP302
        densunits = 'kg/km**3'                                          DSTP303
      Endif                                                             DSTP304
C...  Get mole fractions (volume fractions) from number densities       DSTP305
      fmolCO2 = 100.0*yco2/ytot                                         DSTP306
      fmolN2 = 100.0*yn2/ytot                                           DSTP307
      fmolH = 100.0d0*yh/ytot                                           DSTP308
      fmolHe = 100.0d0*yhe/ytot                                         DSTP309
      fmolO = 100.0d0*yO/ytot                                           DSTP310
      fmolCO = 100.0d0*yCO/ytot                                         DSTP311
      fmolN = 100.0d0*yN/ytot                                           DSTP312
C...  Get mass fractions from number densities                          DSTP313
      ysum = yh*amh + yhe*amhe + yco2*amco2 + yn2*amn2 + yO*amO +       DSTP314
     &  yCO*amCO + yN*amN                                               DSTP315
      yhp = 100.0d0*yh*amh/ysum                                         DSTP316
      yhep = 100.0d0*yhe*amhe/ysum                                      DSTP317
      yco2p = 100.0d0*yco2*amco2/ysum                                   DSTP318
      yn2p = 100.0*yn2*amn2/ysum                                        DSTP319
      yOp = 100.0*yO*amO/ysum                                           DSTP320
      yCOp = 100.0*yCO*amCO/ysum                                        DSTP321
      yNp = 100.0*yn*amn/ysum                                           DSTP322
C...  Write formatted output to list file                               DSTP323
      If(iup.gt.0)Then                                                  DSTP324
        Write(iup,600)TEMP,PRES,profwgt,DENSLO,DENS,DENSHI,densunits,   DSTP325
     &  devlo,devav,devhi,DENSTOT,densunits,DensRand,iupdate,EWWIND,    DSTP326
     &  EWpert,EWtot,NSWIND,NSpert,NStot                                DSTP327
        Write(iup,640)yco2,yn2,yo,yco,yco2p,yn2p,yop,ycop,fmolco2,      DSTP328
     &    fmoln2,fmolo,fmolco                                           DSTP329
  640   Format(' CO2 =',1p,E10.3,' N2 =',E10.3,'  O =',E10.3,' CO =',   DSTP330
     &    E10.3,0p,' #/m**3'/F16.3,3F15.3,' % by mass'/F16.3,           DSTP331
     &    3F15.3,' % by volume')                                        DSTP332
        Write(iup,620)yhe,yn,yh,ytot,yhep,ynp,yhp,AMz,fmolhe,fmoln,     DSTP333
     &    fmolh                                                         DSTP334
        If (I.gt.0.and.corlim.lt.1.0d0.and.dAbs(CLAT).lt.89.99d0)       DSTP335
     &    Write(iup,610)corlim                                          DSTP336
        Write(iup,650)                                                  DSTP337
      Endif                                                             DSTP338
  600 FORMAT(' Temperature = ',F7.1,' K',5X,' Pressure =',1p,E10.3,     DSTP339
     & ' N/m**2   profwgt =',F6.3,/ ' Density (Low, Avg., High)  =',    DSTP340
     & 3E12.3,1X,A8/ ' Departure from Venus Avg = ',0p,F10.1,' %',      DSTP341
     & 2(F10.1,' %')/' Tot.Dens. =',1p,E10.3,0p,1X,A8,                  DSTP342
     & '    Dens.Pert. =',F7.2,' % of mean  iupdate=',I2,/              DSTP343
     & ' Eastward Wind  (Mean, Perturbed, Total) = ', 3F7.1,' m/s'/     DSTP344
     & ' Northward Wind (Mean, Perturbed, Total) = ',3F7.1,' m/s')      DSTP345
  610 Format(' Warning: Step size smaller than accuracy limit by a ',   DSTP346
     &  'factor of',F6.3)                                               DSTP347
  620 Format(1p,'  He =',E10.3,'  N =',E10.3,'  H =',E10.3,'   Total=', DSTP348
     &   E10.3,' #/m**3',0p/F16.3,2F15.3,'   % by mass   ',             DSTP349
     &   '  MolWgt=',F6.3/F16.3,2F15.3,'   % volume (or mole)',         DSTP350
     &   ' fraction')                                                   DSTP351
  650 FORMAT(' -----------------------------------------------------',  DSTP352
     & '----------------------')                                        DSTP353
C...  Compute pressure in millibars                                     DSTP354
      PRESmb = PRES/100.0d0                                             DSTP355
C...  Get pressure at surface, sigma = p/psurf, and pressure altitude   DSTP356
C     = -Ln(sigma)                                                      DSTP357
      Patsurf = plo(1,1)                                                DSTP358
      sigmalevel = PRES/Patsurf                                         DSTP359
      preshgt = -dlog(sigmalevel)                                       DSTP360
C...  Select plot output variables from codes NVARX and NVARY           DSTP361
      If(NVARX.EQ.9)VARX = PRESmb                                       DSTP362
      If(NVARY.EQ.9)VARY = PRESmb                                       DSTP363
      If(NVARX.EQ.10)VARX = preshgt                                     DSTP364
      If(NVARY.EQ.10)VARY = preshgt                                     DSTP365
      If(NVARX.EQ.11)VARX = sigmalevel                                  DSTP366
      If(NVARY.EQ.11)VARY = sigmalevel                                  DSTP367
C...  Output deviations from Venus average if logscale = 2              DSTP368
      If (logscale.eq.2)Then                                            DSTP369
        DENSLO = devlo                                                  DSTP370
        DENS = devav                                                    DSTP371
        DENSHI = devhi                                                  DSTP372
        DENSTOT = devtot                                                DSTP373
        If (pvra.le.0.0d0)Then                                          DSTP374
          PRES = -99.9d0                                                DSTP375
        Else                                                            DSTP376
          PRES = 100.0d0*(PRES-pvra)/pvra                               DSTP377
        Endif                                                           DSTP378
      Endif                                                             DSTP379
C...  Write parameters on plot format files                             DSTP380
      IF(NVARX.EQ.1)VARX = CHGT                                         DSTP381
      IF(NVARX.EQ.2)VARX = RSC                                          DSTP382
      IF(NVARX.EQ.3)VARX = CLAT                                         DSTP383
      IF(NVARX.EQ.4)Then                                                DSTP384
        VARX = CLON                                                     DSTP385
        If (LonEast.eq.0)VARX = 360.0d0 - CLON                          DSTP386
      Endif                                                             DSTP387
      If(NVARX.eq.13)Then                                               DSTP388
        VARX = CLON                                                     DSTP389
        If (VARX.gt.180.0d0)VARX = VARX - 360.0d0                       DSTP390
        If (LonEast.eq.0)VARX = -VARX                                   DSTP391
      Endif                                                             DSTP392
      If (NVARX.eq.5)VARX = CSEC                                        DSTP393
      If (NVARX.eq.6)VARX = CSEC/DayVenus                               DSTP394
      If (NVARX.eq.7)VARX = ALS                                         DSTP395
      If (NVARX.eq.8)VARX = TLOCAL                                      DSTP396
      If (NVARX.eq.12)VARX = SZA                                        DSTP397
      Alogdens = 0.0d0                                                  DSTP398
C...  Change density and pressure units as determined by LOGSCALE       DSTP399
      If (logscale .ne. 2)Alogdens = dlog10(DENS)                       DSTP400
      If (logscale .eq. 1)then                                          DSTP401
        DENS = Alogdens                                                 DSTP402
        PRES = dlog10(PRES)                                             DSTP403
        DENSLO = dlog10(DENSLO)                                         DSTP404
        DENSHI = dLOG10(DENSHI)                                         DSTP405
        DENSTOT = dlog10(DENSTOT)                                       DSTP406
      Endif                                                             DSTP407
C...  Write plotable output files                                       DSTP408
      If (NVARY .eq. 0.and.iup.gt.0)then                                DSTP409
        Write(21,796)VARX,DENSLO,DENS,DENSHI,DENSTOT,RSC,Rref,          DSTP410
     &    ogz,LOGSCALE,profwgt                                          DSTP411
        Write(22,790)VARX,SIGD,DensRand,corlim,SIGU,SIGV,iupdate,       DSTP412
     &    TEMP*(1.0-SIGD/100.),TEMP,TEMP*(1.0+SIGD/100.),               DSTP413
     &    TEMP*(1.0-DensRand/100.)                                      DSTP414
        Write(23,791)VARX,EWWIND,EWpert,EWtot,NSWIND,NSpert,NStot,      DSTP415
     &    iupdate                                                       DSTP416
        Write(24,798)VARX,TEMP,PRES,TEMP-273.15d0,PRESmb,Hrho,HSCALE,   DSTP417
     &    AMz,fmolco2,fmoln2,fmolo,fmolco,fmolhe,fmoln,fmolh,LOGSCALE   DSTP418
      Else If (iup.gt.0) Then                                           DSTP419
        IF(NVARY.EQ.1)VARY = CHGT                                       DSTP420
        IF(NVARY.EQ.2)VARY = RSC                                        DSTP421
        IF(NVARY.EQ.3)VARY = CLAT                                       DSTP422
        IF(NVARY.EQ.4)Then                                              DSTP423
          VARY = CLON                                                   DSTP424
          If (LonEast.eq.0)VARY = 360.0d0 - CLON                        DSTP425
        Endif                                                           DSTP426
        If(NVARY.eq.13)Then                                             DSTP427
          VARY = CLON                                                   DSTP428
          If (VARY.gt.180.0d0)VARY = VARY - 360.0d0                     DSTP429
          If (LonEast.eq.0)VARY = -VARY                                 DSTP430
        Endif                                                           DSTP431
        If (NVARY.eq.5)VARY = CSEC                                      DSTP432
        If (NVARY.eq.6)VARY = CSEC/DayVenus                             DSTP433
        If (NVARY.eq.7)VARY = ALS                                       DSTP434
        If (NVARY.eq.8)VARY = TLOCAL                                    DSTP435
        If (NVARY.eq.12)VARY = SZA                                      DSTP436
        Write(21,797)VARX,VARY,DENSLO,DENS,DENSHI,DENSTOT,              DSTP437
     &    RSC,Rref,ogz,LOGSCALE,profwgt                                 DSTP438
        Write(22,795)VARX,VARY,SIGD,DensRand,corlim,SIGU,SIGV,iupdate,  DSTP439
     &    TEMP*(1.0-SIGD/100.),TEMP,TEMP*(1.0+SIGD/100.),               DSTP440
     &    TEMP*(1.0-DensRand/100.)                                      DSTP441
        Write(23,792)VARX,VARY,EWWIND,EWpert,EWtot,NSWIND,NSpert,NStot, DSTP442
     &    iupdate                                                       DSTP443
        Write(24,799)VARX,VARY,TEMP,PRES,TEMP-273.15d0,PRESmb,Hrho,     DSTP444
     &    HSCALE,AMz,fmolco2,fmoln2,fmolo,fmolco,fmolhe,fmoln,fmolh,    DSTP445
     &    LOGSCALE                                                      DSTP446
      Endif                                                             DSTP447
  790 FORMAT(G13.5,F6.2,F10.3,1p,E10.3,0p,2F7.2,I5,3x,4F7.2)            DSTP448
  791 Format(G13.5,6F8.2,I5)                                            DSTP449
  792 Format(2G13.5,6F8.2,I5)                                           DSTP450
  795 Format(2G13.5,F6.2,F10.3,1p,E10.3,0p,2F7.2,I5,3x,4F7.2)           DSTP451
  796 FORMAT(G13.5,1p,4E11.3,0p,2F9.2,F7.3,I5,F9.3)                     DSTP452
  797 Format(2G13.5,1p,4E11.3,0p,2F9.2,F7.3,I5,F9.3)                    DSTP453
  798 FORMAT(G13.5,2(F7.1,1p,E11.3,0p),2F7.2,F6.2,F6.1,5F5.1,F6.1,I5)   DSTP454
  799 Format(2G13.5,2(F7.1,1p,E11.3,0p),2F7.2,F6.2,F6.1,5F5.1,F6.1,I5)  DSTP455
C...  Write non-descriptively formatted data on OUTPUT file             DSTP456
      VAR = CHGT                                                        DSTP457
      IF(NVARX.EQ.2.or.NVARY.eq.2)VAR = RSC                             DSTP458
      If(iup.gt.0)Then                                                  DSTP459
        OLON = CLON                                                     DSTP460
        If (LonEast.eq.0)OLON = 360.0d0 - CLON                          DSTP461
        If (NVARX.eq.13.or.NVARY.eq.13)Then                             DSTP462
          If(OLON.gt.180.0d0)OLON = OLON - 360.0d0                      DSTP463
        Endif                                                           DSTP464
        If(logscale.eq.0.or.logscale.eq.3)Then                          DSTP465
          WRITE(25,800)CSEC,VAR,CLAT,OLON,dens,TEMP,EWWIND,NSWIND,SIGD, DSTP466
     &      ALS,SZA,yco2p,yn2p,yop,ycop,yhep,ynp,yhp                    DSTP467
        Else If (logscale.eq.1)Then                                     DSTP468
          WRITE(25,810)CSEC,VAR,CLAT,OLON,dens,TEMP,EWWIND,NSWIND,SIGD, DSTP469
     &      ALS,SZA,yco2p,yn2p,yop,ycop,yhep,ynp,yhp                    DSTP470
        Else                                                            DSTP471
          WRITE(25,805)CSEC,VAR,CLAT,OLON,dens,TEMP,EWWIND,NSWIND,SIGD, DSTP472
     &      ALS,SZA,yco2p,yn2p,yop,ycop,yhep,ynp,yhp                    DSTP473
        Endif                                                           DSTP474
      Endif                                                             DSTP475
  800 FORMAT(F10.1,F8.2,F7.2,F8.2,1P,E9.2,0P,3F7.1,5F6.1,4F5.1,F6.1)    DSTP476
  805 FORMAT(F10.1,F8.2,F7.2,F8.2,F9.2,3F7.1,5F6.1,4F5.1,F6.1)          DSTP477
  810 FORMAT(F10.1,F8.2,F7.2,F8.2,F9.3,3F7.1,5F6.1,4F5.1,F6.1)          DSTP478
      Return                                                            DSTP479
  999 EOF = 1                                                           DSTP480
      If (NPOS.LE.0)Rewind(7)                                           DSTP481
      Return                                                            DSTP482
 9998 Stop ' Error termination reading trajectory data file!'           DSTP483
      END                                                               DSTP484
C-----------------------------------------------------------------------DSTP485
      Double Precision Function PPND_V05(p, ifault)                     PPND  1
C                                                                       PPND  2
C     Algorithm AS 111 Appl. Statist. (1977) Vol. 26, p. 118            PPND  3
C                                                                       PPND  4
C     Produces normal deviate corresponding to lower tail area of p.    PPND  5
C     Returns ifault = 1 in input p >= 1 or <= 0, ifault = 0            PPND  6
C     otherwise.  If ifault = 1, PPND_V05 value is set to 0.            PPND  7
C     Double precision version with error epsilon = 2 ** (-31).         PPND  8
C     For Single precision version, change DOUBLE PRECISION to REAL     PPND  9
C     in the FUNCTION statement and the declaration of variables;       PPND 10
C     change E0 to D0 in the DATA statements and change dABS, dLOG      PPND 11
C     and dSQRT to ABS, ALOG and SQRT in the assignment statements.     PPND 12
C     The hash sums are the sums of the moduli of the coefficients.     PPND 13
C     They have no inherent meanings, but are included for use in       PPND 14
C     checking transpositions.                                          PPND 15
C                                                                       PPND 16
      Implicit None                                                     PPND 17
      Double Precision zero, split, half, one, a0, a1, a2, a3,          PPND 18
     & b1, b2, b3, b4, c0, c1, c2, c3, d1, d2, p, q, r                  PPND 19
      Integer ifault                                                    PPND 20
C                                                                       PPND 21
      Data zero, half, one, split /0.0D0, 0.5D0, 1.0D0, 0.42D0/         PPND 22
C                                                                       PPND 23
      Data a0 /       2.50662823884D0/,                                 PPND 24
     & a1 /     -18.61500062529D0/,                                     PPND 25
     & a2 /      41.39119773534D0/,                                     PPND 26
     & a3 /     -25.44106049637D0/,                                     PPND 27
     & b1 /      -8.47351093090D0/,                                     PPND 28
     & b2 /      23.08336743743D0/,                                     PPND 29
     & b3 /     -21.06224101826D0/,                                     PPND 30
     & b4 /       3.13082909833D0/                                      PPND 31
C                                                                       PPND 32
C     Hash sum for a & b = 143.70383558076                              PPND 33
C                                                                       PPND 34
      Data c0 /      -2.78718931138D0/,                                 PPND 35
     & c1 /      -2.29796479134D0/,                                     PPND 36
     & c2 /       4.85014127135D0/,                                     PPND 37
     & c3 /       2.32121276858D0/,                                     PPND 38
     & d1 /       3.54388924762D0/,                                     PPND 39
     & d2 /       1.63706781897D0/                                      PPND 40
C                                                                       PPND 41
C     Hash sum for c & d = 17.43746520924                               PPND 42
C                                                                       PPND 43
C                                                                       PPND 44
      ifault = 0                                                        PPND 45
      q = p - half                                                      PPND 46
      If (dABS(q) .gt. split) goto 1                                    PPND 47
      r = q * q                                                         PPND 48
      PPND_V05 = q * (((a3 * r + a2) * r + a1) * r + a0) /              PPND 49
     & ((((b4 * r + b3) * r + b2) * r + b1) * r + one)                  PPND 50
      Return                                                            PPND 51
    1 r = p                                                             PPND 52
      If (q .gt.zero) r = one - p                                       PPND 53
      If (r .lt.zero) goto 2                                            PPND 54
      r = dSQRT(-dLOG(r))                                               PPND 55
      PPND_V05 = (((c3 * r + c2) * r + c1) * r + c0) /                  PPND 56
     & ((d2 * r + d1) * r + one)                                        PPND 57
      If (q .lt. zero) PPND_V05 = -PPND_V05                             PPND 58
      Return                                                            PPND 59
    2 ifault = 1                                                        PPND 60
      PPND_V05 = zero                                                   PPND 61
      Return                                                            PPND 62
      End                                                               PPND 63
C-----------------------------------------------------------------------PPND 64
      Double Precision Function RANDOM_V05(L)                           RAND  1
C                                                                       RAND  2
C     Algorithm AS 183 Appl. Statist. (1982) Vol. 31, p.188             RAND  3
C                                                                       RAND  4
C     Returns a pseudo-random number rectangularly distributed          RAND  5
C     between 0 and 1.                                                  RAND  6
C                                                                       RAND  7
C     IX, IY and IZ should be set to integer values between             RAND  8
C     1 and 30,000 before first entry.                                  RAND  9
C                                                                       RAND 10
C     Integer arithmetic up to 30323 is required.                       RAND 11
C                                                                       RAND 12
C     Returns L = 0 unless random = 0 or random = 1, in which           RAND 13
C     case L = 1                                                        RAND 14
C                                                                       RAND 15
      Implicit None                                                     RAND 16
      Double Precision one,zero                                         RAND 17
      Integer IX,IY,IZ,L                                                RAND 18
      Common /RANDCOM_V05/ IX, IY, IZ                                   RAND 19
      Data one,zero/1.0D0,0.0D0/                                        RAND 20
C     IX = 171 * Mod(IX, 177) -  2 * (IX / 177)                         RAND 21
C     IY = 172 * Mod(IY, 176) - 35 * (IY / 176)                         RAND 22
C     IZ = 170 * Mod(IZ, 178) - 63 * (IZ / 178)                         RAND 23
C                                                                       RAND 24
C     If (IX .lt. 0) IX = IX + 30269                                    RAND 25
C     If (IY .lt. 0) IY = IY + 30307                                    RAND 26
C     If (IZ .lt. 0) IZ = IZ + 30323                                    RAND 27
C                                                                       RAND 28
C     If integer arithmetic up to 5,212,632 is not available,           RAND 29
C     the preceding 6 statements may be used instead of the following 3 RAND 30
C                                                                       RAND 31
      IX = Mod(171 * IX, 30269)                                         RAND 32
      IY = Mod(172 * IY, 30307)                                         RAND 33
      IZ = Mod(170 * IZ, 30323)                                         RAND 34
C                                                                       RAND 35
C     On some machines, this may slightly decrease the speed.           RAND 36
C     The results should be identical.                                  RAND 37
C                                                                       RAND 38
      Random_V05 = dmod(dble(IX) / 30269.0d0 + dble(IY) / 30307.0d0 +   RAND 39
     & dble(IZ) / 30323.0d0, one)                                       RAND 40
      L = 0                                                             RAND 41
      If (Random_V05 .le. zero .or. Random_V05 .ge. one)L = 1           RAND 42
      Return                                                            RAND 43
      End                                                               RAND 44
C-----------------------------------------------------------------------RAND 45
        Subroutine rgplanet_V05(dlat,Rref,hgt,gz,iplanet)               RGPL  1
C...    Computes planetary radius (Rref, km), and gravity (gz,          RGPL  2
C       m/s**2) at latitude dlat (deg) and height (hgt, km) for         RGPL  3
C       planet with code iplanet (see list below)                       RGPL  4
        Implicit None                                                   RGPL  5
        Double Precision dlat,Rref,hgt,gz,GM,requ,rpol,J2,omega,per,    RGPL  6
     &    pi,flat,s2lat,P2                                              RGPL  7
        Integer iplanet                                                 RGPL  8
        pi = 4.0D0*Datan(1.0D0)                                         RGPL  9
C                                                                       RGPL 10
C...    Planet number code                                              RGPL 11
C       Venus = 2                                                       RGPL 12
C       Earth = 3                                                       RGPL 13
C       Mars = 4                                                        RGPL 14
C       Jupiter = 5                                                     RGPL 15
C       Saturn = 6                                                      RGPL 16
C       Uranus = 7                                                      RGPL 17
C       Neptune = 8                                                     RGPL 18
C       Titan = 606 (sixth satellite of Saturn)                         RGPL 19
C                                                                       RGPL 20
C...    Planetary constants                                             RGPL 21
C       GM = mass constant (m**3/s**2)                                  RGPL 22
C       requ, rpol = equatorial and polar radii (km)                    RGPL 23
C       J2 = first gravity field non-spherical term (unitless)          RGPL 24
C       per = rotation period (sec) (negative for retrograde)           RGPL 25
        If (iplanet.eq.2)Then                                           RGPL 26
C...      Venus                                                         RGPL 27
          GM = 3.2485863D14                                             RGPL 28
          requ = 6051.893d0                                             RGPL 29
          rpol = 6051.893d0                                             RGPL 30
          J2 = 0.0d0                                                    RGPL 31
          per = -20996798.0d0                                           RGPL 32
        Else If (iplanet.eq.3)Then                                      RGPL 33
C...      Earth                                                         RGPL 34
          GM = 3.986005D14                                              RGPL 35
          requ = 6378.1370D0                                            RGPL 36
          rpol = 6356.7523D0                                            RGPL 37
          J2 = 0.00108263D0                                             RGPL 38
          per = 86164.100D0                                             RGPL 39
        Else If (iplanet.eq.4)Then                                      RGPL 40
C...      Mars                                                          RGPL 41
          GM = 4.2828314258D13                                          RGPL 42
          requ = 3396.0D0                                               RGPL 43
          rpol = 3378.32D0                                              RGPL 44
          J2 = 0.001958616128D0                                         RGPL 45
          per = 88642.66D0                                              RGPL 46
        Else If (iplanet.eq.5)Then                                      RGPL 47
C...      Jupiter                                                       RGPL 48
          GM = 1.26686537D17                                            RGPL 49
          requ = 71492.0d0                                              RGPL 50
          rpol = 66854.0d0                                              RGPL 51
          J2 = 0.014687D0                                               RGPL 52
          per = 35729.685d0                                             RGPL 53
        Else If (iplanet.eq.6)Then                                      RGPL 54
C...      Saturn                                                        RGPL 55
          GM = 3.79312845D16                                            RGPL 56
          requ = 60268.0d0                                              RGPL 57
          rpol = 54364.0d0                                              RGPL 58
          J2 = 0.016358D0                                               RGPL 59
          per = 38362.4d0                                               RGPL 60
        Else If (iplanet.eq.7)Then                                      RGPL 61
C...      Uranus                                                        RGPL 62
          GM = 5.793947D15                                              RGPL 63
          requ = 25559.0d0                                              RGPL 64
          rpol = 24973.0d0                                              RGPL 65
          J2 = 0.003515D0                                               RGPL 66
          per = -62064.0d0                                              RGPL 67
        Else If (iplanet.eq.8)Then                                      RGPL 68
C...      Neptune                                                       RGPL 69
          GM = 6.835107D15                                              RGPL 70
          requ = 24766.0D0                                              RGPL 71
          rpol = 24342.0D0                                              RGPL 72
          J2 = 0.003540D0                                               RGPL 73
          per = 57996.0D0                                               RGPL 74
        Else If (iplanet.eq.606)Then                                    RGPL 75
C...      Titan                                                         RGPL 76
          GM = 8.97803D12                                               RGPL 77
          requ = 2575.5D0                                               RGPL 78
          rpol = 2575.5D0                                               RGPL 79
          J2 = 0.0D0                                                    RGPL 80
          per = 1377684.0d0                                             RGPL 81
        Else                                                            RGPL 82
          Stop ' Invalid planet code in call to rgplanet_V05'           RGPL 83
        Endif                                                           RGPL 84
C       Flattening term                                                 RGPL 85
        flat = (requ - rpol)/rpol                                       RGPL 86
C       Rotation rate (rad/sec)                                         RGPL 87
        omega = 2.0D0*pi/DAbs(per)                                      RGPL 88
C...    Planetary radius at latitude                                    RGPL 89
        s2lat = (Dsin(pi*dlat/1.8D2))**2                                RGPL 90
        Rref = requ/Sqrt(1.0D0 + flat*(flat+2.0D0)*s2lat)               RGPL 91
C...    Gravity at height hgt                                           RGPL 92
        P2 = 1.5D0*s2lat - 0.5D0                                        RGPL 93
        gz = 1.0D-6*GM/(Rref+hgt)**2                                    RGPL 94
C...    J2 correction term                                              RGPL 95
        gz = gz - gz*3.0D0*J2*P2*((requ/(Rref+hgt))**4)                 RGPL 96
C...    Rotation correction term                                        RGPL 97
        gz = gz - 1.0D3*(Rref+hgt)*(omega*Dcos(pi*dlat/1.8D2))**2       RGPL 98
        Return                                                          RGPL 99
        End                                                             RGPL100
C-----------------------------------------------------------------------RGPL101
      Subroutine Winds_V05(z,clat,time,u,v)                             WIND  1
C...  Compute zonal and meridional winds (u and v) at height z,         WIND  2
C     latitude clat, and local solar time.  Approximations to data in   WIND  3
C     Fig. 3, page 466 and Fig. 5, page 469 of "Venus II", and Fig. 8,  WIND  4
C     page 696 of "Venus".                                              WIND  5
C                                                                       WIND  6
C     Note: Many references adopt the convention for Venus that         WIND  7
C     super-rotating (westward, or retrograde) zonal winds are          WIND  8
C     positive.  We retain the traditional right-handed coordinate      WIND  9
C     convention, whereby zonal winds are positive eastward and         WIND 10
C     meridional winds are positive northward.                          WIND 11
C                                                                       WIND 12
      Implicit None                                                     WIND 13
      Double Precision xlat,z,u,v,clat,ue,pi180,ve,ut,time,usas,vsas    WIND 14
      pi180 = dAtan(1.0d0)/45.0d0                                       WIND 15
      xlat = clat                                                       WIND 16
C...  Retrograde zonal wind magnitude versus height (- for westward     WIND 17
C     superrotation)                                                    WIND 18
      ue = -1.0d0 - 99.0d0*z/80.0d0                                     WIND 19
C...  Decrease in ue above 80 km parameterized from page 333 of         WIND 20
C     Lellouch et al., Icarus 110, 315-319 (1994) and Fig 2. of Hou and WIND 21
C     Farrell, J. Atmos. Sci. 44, 1049-1061 (1987).                     WIND 22
      If (z.ge.80.0d0) ue = -100.0d0*(1.0d0 - (z-80.0d0)/30.0d0)        WIND 23
      If (ue.gt.0.0)ue = 0.0                                            WIND 24
C...  Meridional wind magnitude versus height for retrograde wind       WIND 25
      ve = 0.1d0*ue                                                     WIND 26
C...  Sub-solar to anti-solar diurnal wind (u peaks at terminator),     WIND 27
C     parameterized from Fig. 2 of Zhang et al. J. Geophys. Res.        WIND 28
C     101(E10), 23,195-205 (1996) and Fig 4. of Bougher et al. Icarus,  WIND 29
C     73, 545-573 (1988)                                                WIND 30
      ut = -3.0d0*(z - 81.0d0)                                          WIND 31
      If (ut.gt.0.0)ut = 0.0                                            WIND 32
      If (ut.lt.-237.0d0)ut = -237.0d0                                  WIND 33
C...  Time variation of sub-solar to anti-solar wind components         WIND 34
      usas = ut*dSin(pi180*15.0d0*(time - 12.0d0))                      WIND 35
      vsas = -ut*dCos(pi180*15.0d0*(time - 12.0d0))                     WIND 36
C...  Latitude variation of zonal and meridional wind                   WIND 37
      u = (ue + usas)*(1.0d0 - (xlat/90.0)**4)                          WIND 38
      v = ve*dSin(2.0d0*pi180*xlat) + vsas*dSin(pi180*xlat)             WIND 39
      Return                                                            WIND 40
      End                                                               WIND 41
C-----------------------------------------------------------------------WIND 42
      Subroutine Venusatm_V05(CHGT,CLAT,CLON,TLOCAL,sza,HSCALE,TEMP,    VATM  1
     &  DENS,FACTHI,FACTLO,PRES,Hrho,AMz,EWWIND,NSWIND,ytot,yco2,yn2,   VATM  2
     &  yo,yco,yhe,yn,yh,profnear,proffar,nprof,profwgt)                VATM  3
C...  Evaluates Venus atmospheric parameters by interpolating VIRA data VATM  4
C     to given height, latitude, and local solar time or solar zenith   VATM  5
C     angle, as necessary                                               VATM  6
      Implicit None                                                     VATM  7
      Double Precision CHGT,CLAT,NSWIND,HSCALE,TEMP,DENS,FACTHI,sza,    VATM  8
     &  FACTLO,PRES,Hrho,AMz,EWWIND,ytot,yh,yco2,yhe,yn2,yo,yco,yn,     VATM  9
     &  TLOCAL,yco2md,yn2md,yomd,R0,zfact,ynb,yn1,z,T1,T2,p1,p2,R1,R2,  VATM 10
     &  d1,d2,yh1,yh2,yhe1,yhe2,Rgas,zlo,plo,dlo,tlo,zetalo,Rlo,ycoa,   VATM 11
     &  zmd,pmd,dmd,tmd,AVn,ycomd,yhemd,ynmd,zhi,phi,dhi,thi,yco2hi,    VATM 12
     &  yn2hi,yohi,yco21,yco22,ycohi,yhehi,ynhi,yhhi,yn21,yn22,yo1,yo2, VATM 13
     &  yco1,ycob,zeta,zeta1,zeta2,pa,da,ta,zetaa,Ra,yco2a,yn2a,yoa,    VATM 14
     &  yna,yhea,tf,yco2f,yn2f,yof,ycof,yhef,ynf,yhf,df,pf,AMzp,        VATM 15
     &  profnear,proffar,profwgt,tout,pout,dout,uout,vout,yratio,CLON   VATM 16
      Integer j,jm,nprof                                                VATM 17
C...  Venus model VIRA data at low ,middle, and high altitudes          VATM 18
      Common /VIRAdata_V05/zlo(81),plo(81,5),dlo(81,5),tlo(81,5),       VATM 19
     &  zetalo(81,5),Rlo(81,5),zmd(11),pmd(11,2),dmd(11,2),tmd(11,2),   VATM 20
     &  yco2md(11,2),yn2md(11,2),yomd(11,2),ycomd(11,2),yhemd(11,2),    VATM 21
     &  ynmd(11,2),zhi(21),phi(21,7),dhi(21,7),                         VATM 22
     &  thi(21,7),yco2hi(21,7),yn2hi(21,7),yohi(21,7),ycohi(21,7),      VATM 23
     &  yhehi(21,7),ynhi(21,7),yhhi(21,7)                               VATM 24
C.... Universal gas constant                                            VATM 25
      R0 = 8314.472d0                                                   VATM 26
C...  Avogadro's number                                                 VATM 27
      AVn =  6.02214199d26                                              VATM 28
C...  Convert to height z (km) and insure 0 < z < 250 km                VATM 29
      z = CHGT                                                          VATM 30
C...  Limit heights to > 0 km                                           VATM 31
      If (z.lt.0.0d0)z = 0.0d0                                          VATM 32
C...  For heights 0-98 km use interpolation of low altitude VIRA data   VATM 33
      If (z.le.zlo(80))Then                                             VATM 34
        Do 100 j = 2,80                                                 VATM 35
C...      Find height index for interpolation                           VATM 36
          If (z.le.zlo(j))Then                                          VATM 37
            jm = j - 1                                                  VATM 38
C...        Get low altitude VIRA data at interpolation heights         VATM 39
            Call Lowterp_V05(jm,clat,p1,d1,t1,zeta1,R1,yco21,yn21,yo1,  VATM 40
     &        yco1)                                                     VATM 41
            Call Lowterp_V05(j,clat,p2,d2,t2,zeta2,R2,yco22,yn22,yo2,   VATM 42
     &        ycob)                                                     VATM 43
C...        Height interpolation factor                                 VATM 44
            zfact = (z-zlo(jm))/(zlo(j)-zlo(jm))                        VATM 45
C...        Linear interpolation of temperature                         VATM 46
            TEMP = t1 + (t2 - t1)*zfact                                 VATM 47
C...        Linear interpolation of compressibility and gas constant    VATM 48
            zeta = zeta1 + (zeta2 - zeta1)*zfact                        VATM 49
            Rgas = R1 + (R2 - R1)*zfact                                 VATM 50
C...        Logarithmic interpolation of pressure                       VATM 51
            PRES = dExp(dLog(p1) + dLog(p2/p1)*zfact)                   VATM 52
C...        Density from compressible gas law                           VATM 53
            DENS = PRES/(zeta*Rgas*TEMP)                                VATM 54
C...        Mean molecular weight                                       VATM 55
            AMz = R0/Rgas                                               VATM 56
C...        Logarithmic interpolation of CO2 and N2 number densities    VATM 57
            yco2 = dExp(dLog(yco21) + dLog(yco22/yco21)*zfact)          VATM 58
            yn2 = dExp(dLog(yn21) + dLog(yn22/yn21)*zfact)              VATM 59
C...        Logarithmic interpolation of O and CO, avoiding problems    VATM 60
C           with transition from 0 values below 86 km                   VATM 61
            If (yo2.le.0.0)Then                                         VATM 62
              yo = 0.0                                                  VATM 63
            Else                                                        VATM 64
              If (yo1.le.0.0)yo1 = 0.1d0*yo2                            VATM 65
              yo = dExp(dLog(yo1) + dLog(yo2/yo1)*zfact)                VATM 66
            Endif                                                       VATM 67
            If (ycob.le.0.0)Then                                        VATM 68
              yco = 0.0                                                 VATM 69
            Else                                                        VATM 70
              If (yco1.le.0.0)yco1 = 0.1d0*ycob                         VATM 71
              yco = dExp(dLog(yco1) + dLog(ycob/yco1)*zfact)            VATM 72
            Endif                                                       VATM 73
C...        Helium, and atomic H and N not present in low atmosphere    VATM 74
            yhe = 0.0                                                   VATM 75
            yn = 0.0                                                    VATM 76
            yh = 0.0                                                    VATM 77
C...        Total number density                                        VATM 78
            ytot = yco2 + yn2 + yo + yco                                VATM 79
C...        Scale heights for pressure and density                      VATM 80
            HSCALE = (zlo(j)-zlo(jm))/dLog(p1/p2)                       VATM 81
            Hrho = (zlo(j)-zlo(jm))/dLog(d1/d2)                         VATM 82
            Goto 900                                                    VATM 83
          Endif                                                         VATM 84
 100     Enddo                                                          VATM 85
      Else If (z.le.zlo(81))Then                                        VATM 86
C...    For height 98-100 km, interpolate using combination of low and  VATM 87
C       middle altitude VIRA data                                       VATM 88
C...    Get low altitude VIRA data at 98 km                             VATM 89
        Call Lowterp_V05(80,clat,p1,d1,t1,zeta1,R1,yco21,yn21,yo1,yco1) VATM 90
C...    Get low altitude VIRA data at 100 km                            VATM 91
        Call Lowterp_V05(81,clat,pa,da,ta,zetaa,Ra,yco2a,yn2a,yoa,ycoa) VATM 92
C...    Get middle altitude VIRA data at 100 km                         VATM 93
        Call Midterp_V05(1,TLOCAL,clat,p2,d2,t2,yco22,yn22,yo2,ycob,    VATM 94
     &    yhe2,ynb)                                                     VATM 95
C...    Assume He and H values for logarithmic interpolation            VATM 96
        yhe1 = 0.1d0*yhe2                                               VATM 97
        yn1 = 0.1d0*ynb                                                 VATM 98
C...    Logarithmic average pressure and density at 100 km from low     VATM 99
C       and middle altitude VIRA data                                   VATM100
        p2 = dSqrt(pa*p2)                                               VATM101
        d2 = dSqrt(da*d2)                                               VATM102
C...    Linear average temperature at 100 km from low and middle        VATM103
C       altitude VIRA data                                              VATM104
        t2 = (ta + t2)/2.0d0                                            VATM105
C...    Logarithmic average number densities at 100 km from low         VATM106
C       and middle altitude VIRA data                                   VATM107
        yco22 = dSqrt(yco2a*yco22)                                      VATM108
        yn22 = dSqrt(yn2a*yn22)                                         VATM109
        yo2 = dSqrt(yoa*yo2)                                            VATM110
        ycob = dSqrt(ycoa*ycob)                                         VATM111
C...    Height interpolation factor                                     VATM112
        zfact = (z - zlo(80))/(zlo(81) - zlo(80))                       VATM113
C...    Linear interpolation of temperature                             VATM114
        TEMP = t1 + (t2 - t1)*zfact                                     VATM115
C...    Gas constant at 98 and 100 km                                   VATM116
        R1 = p1/(d1*t1)                                                 VATM117
        R2 = p2/(d2*t2)                                                 VATM118
C...    Linear interpolation of gas constant                            VATM119
        Rgas = R1 + (R2 - R1)*zfact                                     VATM120
C...    Logarithmic interpolation of pressure                           VATM121
        PRES = dExp(dLog(p1) + dLog(p2/p1)*zfact)                       VATM122
C...    Density from perfect gas law (zeta = 1)                         VATM123
        DENS = PRES/(Rgas*TEMP)                                         VATM124
C...    Mean molecular weight                                           VATM125
        AMz = R0/Rgas                                                   VATM126
C...    Logarithmic interpolation of number densities                   VATM127
        yco2 = dExp(dLog(yco21) + dLog(yco22/yco21)*zfact)              VATM128
        yn2 = dExp(dLog(yn21) + dLog(yn22/yn21)*zfact)                  VATM129
        yo = dExp(dLog(yo1) + dLog(yo2/yo1)*zfact)                      VATM130
        yco = dExp(dLog(yco1) + dLog(ycob/yco1)*zfact)                  VATM131
        yhe = dExp(dLog(yhe1) + dLog(yhe2/yhe1)*zfact)                  VATM132
        yn = dExp(dLog(yn1) + dLog(ynb/yn1)*zfact)                      VATM133
C.......Atomic hydrogen not present at this height                      VATM134
        yh = 0.0                                                        VATM135
C...    Total number density                                            VATM136
        ytot = yco2 + yn2 + yo + yco + yhe + yn                         VATM137
C...    Scale heights for pressure and density                          VATM138
        HSCALE = (zlo(81)-zlo(80))/dLog(p1/p2)                          VATM139
        Hrho = (zlo(81)-zlo(80))/dLog(d1/d2)                            VATM140
        Goto 900                                                        VATM141
      Else If (z.le.zmd(2))Then                                         VATM142
C...    For height 100-105 km, interpolate using combination of low and VATM143
C       middle altitude VIRA data                                       VATM144
C...    Get low altitude VIRA data at 100 km                            VATM145
        Call Lowterp_V05(81,clat,p1,d1,t1,zeta1,R1,yco21,yn21,yo1,yco1) VATM146
C...    Get middle altitude VIRA data at 100 km                         VATM147
        Call Midterp_V05(1,TLOCAL,clat,pa,da,ta,yco2a,yn2a,yoa,ycoa,    VATM148
     &    yhea,yna)                                                     VATM149
C...    Get middle altitude VIRA data at 105 km                         VATM150
        Call Midterp_V05(2,TLOCAL,clat,p2,d2,t2,yco22,yn22,yo2,ycob,    VATM151
     &    yhe2,ynb)                                                     VATM152
C...    Logarithmic average pressure and density at 100 km from low     VATM153
C       and middle altitude VIRA data                                   VATM154
        p1 = dSqrt(pa*p1)                                               VATM155
        d1 = dSqrt(da*d1)                                               VATM156
C...    Linear average temperature at 100 km from low and middle        VATM157
C       altitude VIRA data                                              VATM158
        t1 = (ta + t1)/2.0d0                                            VATM159
C...    Logarithmic average number densities at 100 km from low         VATM160
C       and middle altitude VIRA data                                   VATM161
        yco21 = dSqrt(yco2a*yco21)                                      VATM162
        yn21 = dSqrt(yn2a*yn21)                                         VATM163
        yo1 = dSqrt(yoa*yo1)                                            VATM164
        yco1 = dSqrt(ycoa*yco1)                                         VATM165
C...    Take 100 km He and N number densities from middle atmosphere    VATM166
C       VIRA data                                                       VATM167
        yhe1 = yhea                                                     VATM168
        yn1 = yna                                                       VATM169
C...    Height interpolation factor                                     VATM170
        zfact = (z - zmd(1))/(zmd(2) - zmd(1))                          VATM171
C...    Linear interpolation of temperature                             VATM172
        TEMP = t1 + (t2 - t1)*zfact                                     VATM173
C...    Gas constant at 100 and 105 km                                  VATM174
        R1 = p1/(d1*t1)                                                 VATM175
        R2 = p2/(d2*t2)                                                 VATM176
C...    Linear interpolation of gas constant                            VATM177
        Rgas = R1 + (R2 - R1)*zfact                                     VATM178
C...    Logarithmic interpolation of pressure                           VATM179
        PRES = dExp(dLog(p1) + dLog(p2/p1)*zfact)                       VATM180
C...    Density from perfect gas law (zeta = 1)                         VATM181
        DENS = PRES/(Rgas*TEMP)                                         VATM182
C...    Mean molecular weight                                           VATM183
        AMz = R0/Rgas                                                   VATM184
C...    Logarithmic interpolation of number densities                   VATM185
        yco2 = dExp(dLog(yco21) + dLog(yco22/yco21)*zfact)              VATM186
        yn2 = dExp(dLog(yn21) + dLog(yn22/yn21)*zfact)                  VATM187
        yo = dExp(dLog(yo1) + dLog(yo2/yo1)*zfact)                      VATM188
        yco = dExp(dLog(yco1) + dLog(ycob/yco1)*zfact)                  VATM189
        yhe = dExp(dLog(yhe1) + dLog(yhe2/yhe1)*zfact)                  VATM190
        yn = dExp(dLog(yn1) + dLog(ynb/yn1)*zfact)                      VATM191
C...    Atomic hydrogen not present at this height                      VATM192
        yh = 0.0                                                        VATM193
C...    Total number density                                            VATM194
        ytot = yco2 + yn2 + yo + yco + yhe + yn                         VATM195
C...    Scale heights for pressure and density                          VATM196
        HSCALE = (zmd(2)-zmd(1))/dLog(p1/p2)                            VATM197
        Hrho = (zmd(2)-zmd(1))/dLog(d1/d2)                              VATM198
        Goto 900                                                        VATM199
      Else If (z.le.zmd(10))Then                                        VATM200
C...    For height 105-145 km, interpolate using middle altitude        VATM201
C       VIRA data                                                       VATM202
        Do 200 j = 2,10                                                 VATM203
C...      Find height index for interpolation                           VATM204
          If (z.le.zmd(j))Then                                          VATM205
            jm = j - 1                                                  VATM206
C...        Get middle altitude VIRA data at interpolation heights      VATM207
            Call Midterp_V05(jm,TLOCAL,clat,p1,d1,t1,yco21,yn21,yo1,    VATM208
     &        yco1,yhe1,yn1)                                            VATM209
            Call Midterp_V05(j,TLOCAL,clat,p2,d2,t2,yco22,yn22,yo2,     VATM210
     &        ycob,yhe2,ynb)                                            VATM211
C...        Height interpolation factor                                 VATM212
            zfact = (z-zmd(jm))/(zmd(j)-zmd(jm))                        VATM213
C...        Linear interpolation of temperature                         VATM214
            TEMP = t1 + (t2 - t1)*zfact                                 VATM215
C...        Gas constant at two interpolation heights                   VATM216
            R1 = p1/(d1*t1)                                             VATM217
            R2 = p2/(d2*t2)                                             VATM218
C...        Linear interpolation of gas constant                        VATM219
            Rgas = R1 + (R2 - R1)*zfact                                 VATM220
C...        Logarithmic interpolation of pressure                       VATM221
            PRES = dExp(dLog(p1) + dLog(p2/p1)*zfact)                   VATM222
C...        Density from perfect gas law                                VATM223
            DENS = PRES/(Rgas*TEMP)                                     VATM224
C...        Mean molecular weight                                       VATM225
            AMz = R0/Rgas                                               VATM226
C...        Logarithmic interpolation of number densities               VATM227
            yco2 = dExp(dLog(yco21) + dLog(yco22/yco21)*zfact)          VATM228
            yn2 = dExp(dLog(yn21) + dLog(yn22/yn21)*zfact)              VATM229
            yo = dExp(dLog(yo1) + dLog(yo2/yo1)*zfact)                  VATM230
            yco = dExp(dLog(yco1) + dLog(ycob/yco1)*zfact)              VATM231
            yhe = dExp(dLog(yhe1) + dLog(yhe2/yhe1)*zfact)              VATM232
            yn = dExp(dLog(yn1) + dLog(ynb/yn1)*zfact)                  VATM233
C...        Atomic hydrogen not present in this height range            VATM234
            yh = 0.0                                                    VATM235
C...        Total number density                                        VATM236
            ytot = yco2 + yn2 + yo + yco + yhe + yn                     VATM237
C...        Scale heights for pressure and density                      VATM238
            HSCALE = (zmd(j)-zmd(jm))/dLog(p1/p2)                       VATM239
            Hrho = (zmd(j)-zmd(jm))/dLog(d1/d2)                         VATM240
            Goto 900                                                    VATM241
           Endif                                                        VATM242
 200    Enddo                                                           VATM243
      Else If (z.le.zmd(11))Then                                        VATM244
C...    For height 145-150 km, interpolate using combination of middle  VATM245
C       and high altitude VIRA data                                     VATM246
C...    Get middle altitude VIRA data at 145 km                         VATM247
        Call Midterp_V05(10,TLOCAL,clat,p1,d1,t1,yco21,yn21,yo1,yco1,   VATM248
     &    yhe1,yn1)                                                     VATM249
C...    Get middle altitude VIRA data at 150 km                         VATM250
        Call Midterp_V05(11,TLOCAL,clat,pa,da,ta,yco2a,yn2a,yoa,ycoa,   VATM251
     &    yhea,yna)                                                     VATM252
C...    Get high altitude VIRA data at 150 km                           VATM253
        Call Highterp_V05(1,sza,p2,d2,t2,yco22,yn22,yo2,ycob,yhe2,ynb,  VATM254
     &    yh2)                                                          VATM255
C...    Logarithmic average pressure and density at 150 km from middle  VATM256
C       and high altitude VIRA data                                     VATM257
        p2 = dSqrt(pa*p2)                                               VATM258
        d2 = dSqrt(da*d2)                                               VATM259
C...    Linear average temperature at 150 km from middle and high       VATM260
C       altitude VIRA data                                              VATM261
        t2 = (ta + t2)/2.0d0                                            VATM262
C...    Logarithmic average number densities at 150 km from middle      VATM263
C       and high altitude VIRA data                                     VATM264
        yco22 = dSqrt(yco2a*yco22)                                      VATM265
        yn22 = dSqrt(yn2a*yn22)                                         VATM266
        yo2 = dSqrt(yoa*yo2)                                            VATM267
        ycob = dSqrt(ycoa*ycob)                                         VATM268
        yhe2 = dSqrt(yhea*yhe2)                                         VATM269
        ynb = dSqrt(yna*ynb)                                            VATM270
C...    Assume H number density at 145 km for logarithmic interpolation VATM271
        yh1 = 0.1d0*yh2                                                 VATM272
C...    Height interpolation factor                                     VATM273
        zfact = (z - zmd(10))/(zmd(11) - zmd(10))                       VATM274
C...    Linear interpolation of temperature                             VATM275
        TEMP = t1 + (t2 - t1)*zfact                                     VATM276
C...    Gas constant at 145 and 150 km                                  VATM277
        R1 = p1/(d1*t1)                                                 VATM278
        R2 = p2/(d2*t2)                                                 VATM279
C...    Linear interpolation of gas constant                            VATM280
        Rgas = R1 + (R2 - R1)*zfact                                     VATM281
C...    Logarithmic interpolation of pressure                           VATM282
        PRES = dExp(dLog(p1) + dLog(p2/p1)*zfact)                       VATM283
C...    Density from perfect gas law                                    VATM284
        DENS = PRES/(Rgas*TEMP)                                         VATM285
C...    Mean molecular weight                                           VATM286
        AMz = R0/Rgas                                                   VATM287
C...    Logarithmic interpolation of number densities                   VATM288
        yco2 = dExp(dLog(yco21) + dLog(yco22/yco21)*zfact)              VATM289
        yn2 = dExp(dLog(yn21) + dLog(yn22/yn21)*zfact)                  VATM290
        yo = dExp(dLog(yo1) + dLog(yo2/yo1)*zfact)                      VATM291
        yco = dExp(dLog(yco1) + dLog(ycob/yco1)*zfact)                  VATM292
        yhe = dExp(dLog(yhe1) + dLog(yhe2/yhe1)*zfact)                  VATM293
        yn = dExp(dLog(yn1) + dLog(ynb/yn1)*zfact)                      VATM294
        yh = dExp(dLog(yh1) + dLog(yh2/yh1)*zfact)                      VATM295
C...    Total number density                                            VATM296
        ytot = yco2 + yn2 + yo + yco + yhe + yn + yh                    VATM297
C...    Scale heights for pressure and density                          VATM298
        HSCALE = (zmd(11)-zmd(10))/dLog(p1/p2)                          VATM299
        Hrho = (zmd(11)-zmd(10))/dLog(d1/d2)                            VATM300
        Goto 900                                                        VATM301
      Else If (z.le.zhi(2))Then                                         VATM302
C...    For height 150-155 km, interpolate using combination of middle  VATM303
C       and high altitude VIRA data                                     VATM304
C...    Get middle altitude VIRA data at 150 km                         VATM305
        Call Midterp_V05(11,TLOCAL,clat,pa,da,ta,yco2a,yn2a,yoa,ycoa,   VATM306
     &    yhea,yna)                                                     VATM307
C...    Get high altitude VIRA data at 150 km                           VATM308
        Call Highterp_V05(1,sza,p1,d1,t1,yco21,yn21,yo1,yco1,yhe1,yn1,  VATM309
     &    yh1)                                                          VATM310
C...    Get high altitude VIRA data at 155 km                           VATM311
        Call Highterp_V05(2,sza,p2,d2,t2,yco22,yn22,yo2,ycob,yhe2,ynb,  VATM312
     &    yh2)                                                          VATM313
C...    Logarithmic average pressure and density at 150 km from middle  VATM314
C       and high altitude VIRA data                                     VATM315
        p1 = dSqrt(pa*p1)                                               VATM316
        d1 = dSqrt(da*d1)                                               VATM317
C...    Linear average temperature at 150 km from middle and high       VATM318
C       altitude VIRA data                                              VATM319
        t1 = (ta + t1)/2.0d0                                            VATM320
C...    Logarithmic average number densities at 150 km from middle      VATM321
C       and high altitude VIRA data                                     VATM322
        yco21 = dSqrt(yco2a*yco21)                                      VATM323
        yn21 = dSqrt(yn2a*yn21)                                         VATM324
        yo1 = dSqrt(yoa*yo1)                                            VATM325
        yco1 = dSqrt(ycoa*yco1)                                         VATM326
        yhe1 = dSqrt(yhea*yhe1)                                         VATM327
        yn1 = dSqrt(yna*yn1)                                            VATM328
C...    Height interpolation factor                                     VATM329
        zfact = (z - zhi(1))/(zhi(2) - zhi(1))                          VATM330
C...    Linear interpolation of temperature                             VATM331
        TEMP = t1 + (t2 - t1)*zfact                                     VATM332
C...    Gas constant at 150 and 155 km                                  VATM333
        R1 = p1/(d1*t1)                                                 VATM334
        R2 = p2/(d2*t2)                                                 VATM335
C...    Linear interpolation of gas constant                            VATM336
        Rgas = R1 + (R2 - R1)*zfact                                     VATM337
C...    Logarithmic interpolation of pressure                           VATM338
        PRES = dExp(dLog(p1) + dLog(p2/p1)*zfact)                       VATM339
C...    Density from perfect gas law                                    VATM340
        DENS = PRES/(Rgas*TEMP)                                         VATM341
C...    Mean molecular weight                                           VATM342
        AMz = R0/Rgas                                                   VATM343
C...    Logarithmic interpolation of number densities                   VATM344
        yco2 = dExp(dLog(yco21) + dLog(yco22/yco21)*zfact)              VATM345
        yn2 = dExp(dLog(yn21) + dLog(yn22/yn21)*zfact)                  VATM346
        yo = dExp(dLog(yo1) + dLog(yo2/yo1)*zfact)                      VATM347
        yco = dExp(dLog(yco1) + dLog(ycob/yco1)*zfact)                  VATM348
        yhe = dExp(dLog(yhe1) + dLog(yhe2/yhe1)*zfact)                  VATM349
        yn = dExp(dLog(yn1) + dLog(ynb/yn1)*zfact)                      VATM350
        yh = dExp(dLog(yh1) + dLog(yh2/yh1)*zfact)                      VATM351
C...    Total number density                                            VATM352
        ytot = yco2 + yn2 + yo + yco + yhe + yn + yh                    VATM353
C...    Scale heights for pressure and density                          VATM354
        HSCALE = (zhi(2)-zhi(1))/dLog(p1/p2)                            VATM355
        Hrho = (zhi(2)-zhi(1))/dLog(d1/d2)                              VATM356
        Goto 900                                                        VATM357
      Else If (z.le.zhi(21))Then                                        VATM358
C...    For height 155-250 km, interpolate using high altitude          VATM359
C       VIRA data                                                       VATM360
        Do 300 j = 2,21                                                 VATM361
C...      Find height index for interpolation                           VATM362
          If (z.le.zhi(j))Then                                          VATM363
            jm = j - 1                                                  VATM364
C...        Get high altitude VIRA data at interpolation heights        VATM365
            Call Highterp_V05(jm,sza,p1,d1,t1,yco21,yn21,yo1,yco1,yhe1, VATM366
     &        yn1,yh1)                                                  VATM367
            Call Highterp_V05(j,sza,p2,d2,t2,yco22,yn22,yo2,ycob,yhe2,  VATM368
     &        ynb,yh2)                                                  VATM369
C...        Height interpolation factor                                 VATM370
            zfact = (z-zhi(jm))/(zhi(j)-zhi(jm))                        VATM371
C...        Linear interpolation of temperature                         VATM372
            TEMP = t1 + (t2 - t1)*zfact                                 VATM373
C...        Gas constant at two interpolation heights                   VATM374
            R1 = p1/(d1*t1)                                             VATM375
            R2 = p2/(d2*t2)                                             VATM376
C...        Linear interpolation of gas constant                        VATM377
            Rgas = R1 + (R2 - R1)*zfact                                 VATM378
            PRES = dExp(dLog(p1) + dLog(p2/p1)*zfact)                   VATM379
C...        Density from perfect gas law                                VATM380
            DENS = PRES/(Rgas*TEMP)                                     VATM381
C...        Mean molecular weight                                       VATM382
            AMz = R0/Rgas                                               VATM383
C...        Logarithmic interpolation of number densities               VATM384
            yco2 = dExp(dLog(yco21) + dLog(yco22/yco21)*zfact)          VATM385
            yn2 = dExp(dLog(yn21) + dLog(yn22/yn21)*zfact)              VATM386
            yo = dExp(dLog(yo1) + dLog(yo2/yo1)*zfact)                  VATM387
            yco = dExp(dLog(yco1) + dLog(ycob/yco1)*zfact)              VATM388
            yhe = dExp(dLog(yhe1) + dLog(yhe2/yhe1)*zfact)              VATM389
            yn = dExp(dLog(yn1) + dLog(ynb/yn1)*zfact)                  VATM390
            yh = dExp(dLog(yh1) + dLog(yh2/yh1)*zfact)                  VATM391
C...        Total number density                                        VATM392
            ytot = yco2 + yn2 + yo + yco + yhe + yn + yh                VATM393
C...        Scale heights for pressure and density                      VATM394
            HSCALE = (zhi(j)-zhi(jm))/dLog(p1/p2)                       VATM395
            Hrho = (zhi(j)-zhi(jm))/dLog(d1/d2)                         VATM396
            Goto 900                                                    VATM397
          Endif                                                         VATM398
 300    Enddo                                                           VATM399
      Else                                                              VATM400
C...  Evaluate atmosphere with constant temperature thermosphere        VATM401
C...    Get high altitude VIRA data at 250 km                           VATM402
        Call Highterp_V05(21,sza,pf,df,tf,yco2f,yn2f,yof,ycof,yhef,ynf, VATM403
     &    yhf)                                                          VATM404
C...    Get thermospheric model values at heights z+1 km and z          VATM405
        TEMP = tf                                                       VATM406
        Call Thermos_V05(z+1.0d0,clat,250.0d0,yco2f,yn2f,yof,ycof,yhef, VATM407
     &    ynf,yhf,tf,PRES,DENS,AMzp,yco2,yn2,yo,yco,yhe,yn,yh,ytot,     VATM408
     &    HSCALE)                                                       VATM409
        Call Thermos_V05(z,clat,250.0d0,yco2f,yn2f,yof,ycof,yhef,ynf,   VATM410
     &    yhf,tf,PRES,DENS,AMz,yco2,yn2,yo,yco,yhe,yn,yh,ytot,HSCALE)   VATM411
C...    Get density scale height from pressure scale height and dM/dz   VATM412
        Hrho = HSCALE/(1.0d0 - HSCALE*(AMzp-AMz)/AMz)                   VATM413
      Endif                                                             VATM414
C...  Get wind components from approximation to VIRA data               VATM415
 900  Call Winds_V05(z,CLAT,TLOCAL,EWWIND,NSWIND)                       VATM416
C...  Compute high and low factors for perturbations: parameterized     VATM417
C     from data in Figs. 1-8(d) and 1-12(a) of Seiff et al. VIRA data,  VATM418
C     pp. 247, 259 & 278 of "Venus", pp. 200 & 201 of "The Planet       VATM419
C     Venus", p. 283. of "Venus II", and Fig. 6 of Hinson and Jenkins   VATM420
C     Icarus 114, 310-327 (1995).                                       VATM421
      If (z.lt.50.0d0)Then                                              VATM422
        FACTHI = 1.02d0                                                 VATM423
      Else                                                              VATM424
        FACTHI = 1.02d0 + 0.0013d0*(z-50.0d0)                           VATM425
      Endif                                                             VATM426
      If (FACTHI.gt.1.15d0)FACTHI = 1.15d0                              VATM427
      FACTLO = 1.0d0/FACTHI                                             VATM428
C---  Use weighted average profile data if profnear > 0. Weight=1 if    VATM429
C     lat-lon radius < profnear. Weight=0 if lat-lon radius > proffar.  VATM430
      If (profnear.gt.0.0d0)Then                                        VATM431
        Call ProfTerp_V05(z,CLAT,CLON,TEMP,PRES,DENS,EWWIND,NSWIND,     VATM432
     &    tout,pout,dout,uout,vout,nprof,profnear,proffar,profwgt)      VATM433
        yratio = dout/DENS                                              VATM434
        yco2 = yco2*yratio                                              VATM435
        yn2 = yn2*yratio                                                VATM436
        yo = yo*yratio                                                  VATM437
        yco = yco*yratio                                                VATM438
        yhe = yhe*yratio                                                VATM439
        yn = yn*yratio                                                  VATM440
        yh = yh*yratio                                                  VATM441
        ytot = ytot*yratio                                              VATM442
        TEMP = tout                                                     VATM443
        PRES = pout                                                     VATM444
        DENS = dout                                                     VATM445
        EWWIND = uout                                                   VATM446
        NSWIND = vout                                                   VATM447
      Endif                                                             VATM448
      Return                                                            VATM449
      End                                                               VATM450
C-----------------------------------------------------------------------VATM451
      Subroutine Venusref_V05(CHGT,tvra,pvra,dvra)                      VREF  1
C...  Evaluates Venus reference atmospheric parameters from approximate VREF  2
C     average VIRA data                                                 VREF  3
      Implicit None                                                     VREF  4
      Double Precision CHGT,tvra,pvra,dvra,z,T1,T2,p1,p2,R1,R2,delz,    VREF  5
     &  dztot,R,HSCALE,zref,pref,dref,tref,AMz,yco2,yn2,yo,yco,yhe,yn,  VREF  6
     &  yh,totnd                                                        VREF  7
      Integer iz,i                                                      VREF  8
C...  Common block for VIRA reference data arrays                       VREF  9
      Common /VIRAref_V05/zref(111),pref(111),dref(111),tref(111)       VREF 10
C...  Convert to height z (km)                                          VREF 11
      z = CHGT                                                          VREF 12
C...  Check that height is in bounds                                    VREF 13
      If (z.lt.zref(1))z = zref(1)                                      VREF 14
      If (z.gt.zref(111))Then                                           VREF 15
C...    Evaluate thermosphere at height z for SZA = 90 deg and 250 km   VREF 16
C       boundary values                                                 VREF 17
        Call Thermos_V05(z,0.0d0,250.0d0,3.07D6,8.61D8,1.05D12,1.13D9,  VREF 18
     &    3.07D12,2.18D10,1.05D12,230.0d0,pvra,dvra,AMz,yco2,yn2,yo,    VREF 19
     &    yco,yhe,yn,yh,totnd,hscale)                                   VREF 20
        tvra = 230.0d0                                                  VREF 21
        Return                                                          VREF 22
      Endif                                                             VREF 23
C...  Find height index for vertical interpolation                      VREF 24
      Do 10 iz = 1,110                                                  VREF 25
        If (zref(iz).le.z.and.zref(iz+1).ge.z)Then                      VREF 26
          i = iz                                                        VREF 27
          Goto 20                                                       VREF 28
        Endif                                                           VREF 29
  10  Enddo                                                             VREF 30
C...  Get Venus average temperature, pressure, and gas law constant     VREF 31
C     at upper and lower height indexes                                 VREF 32
  20  T1 = tref(i)                                                      VREF 33
      T2 = tref(i+1)                                                    VREF 34
      p1 = pref(i)                                                      VREF 35
      p2 = pref(i+1)                                                    VREF 36
      R1 = pref(i)/(dref(i)*tref(i))                                    VREF 37
      R2 = pref(i+1)/(dref(i+1)*tref(i+1))                              VREF 38
C...  Linear height interpolation on temperature                        VREF 39
      delz = z - zref(i)                                                VREF 40
      dztot = zref(i+1) - zref(i)                                       VREF 41
      tvra = T1 + (T2 - T1)*delz/dztot                                  VREF 42
C...  Pressure scale height and vertical pressure interpolation         VREF 43
      HSCALE = dztot/dLog(p1/p2)                                        VREF 44
      pvra = P1*dExp(-delz/HSCALE)                                      VREF 45
C...  Linear height interpolation for gas constant                      VREF 46
      R = R1 + (R2 - R1)*delz/dztot                                     VREF 47
C...  Density from perfect gas law                                      VREF 48
      dvra = pvra/(R*tvra)                                              VREF 49
      Return                                                            VREF 50
      End                                                               VREF 51
C-----------------------------------------------------------------------VREF 52
      Subroutine Rescale_V05(x)                                         RSCL  1
C...  Puts x into range 0 - 360                                         RSCL  2
      Double precision x                                                RSCL  3
      x = x/360.0d0 - Dint(x/360.0d0) + 1.0d0                           RSCL  4
      x = (x - Dint(x))*360.0d0                                         RSCL  5
      Return                                                            RSCL  6
      End                                                               RSCL  7
C---------------------------------------------------------------------- RSCL  8
      Subroutine Shiftdif_V05(x)                                        SHFD  1
C...  Shifts difference x to be +/- and close to 0.0                    SHFD  2
      Double Precision x                                                SHFD  3
      If (x.gt.180.0d0)Then                                             SHFD  4
        x = x - 360.0d0                                                 SHFD  5
      Else If (x.lt.-180.0d0)Then                                       SHFD  6
        x = x + 360.0d0                                                 SHFD  7
      Endif                                                             SHFD  8
      Return                                                            SHFD  9
      End                                                               SHFD 10
C---------------------------------------------------------------------- SHFD 11
      Subroutine venephem_V05(xday,sunlat,sunlon,sunLsubs,radius,owlt,  VEPH  1
     &  EOT)                                                            VEPH  2
C...  Computes sunlat, sunlon= latitude and longitude of sub-solar      VEPH  3
C     point on the surface, sunLsubs= planetocentric longitude of Sun   VEPH  4
C     (Ls), radius= current orbital radius from Sun to Venus, heliolon= VEPH  5
C     Venus heliocentric longitude, owlt= Venus-Earth one-way light     VEPH  6
C     time (minutes), and EOT= equation of time (deg), calculated from  VEPH  7
C     Julian day and time, xday.  Notes: input xday is NOT UTC, but     VEPH  8
C     Terrestrial (Dynamical) Venus-Event Time (NOT Earth-Receive Time).VEPH  9
C     Venus Local Mean Solar Time (hrs ) = Local True Solar Time (hrs)  VEPH 10
C     minus EOT (in hrs). Output is for Terrestrial (Dynamical)         VEPH 11
C     Venus Event Time (corresponding to input xday).                   VEPH 12
C                                                                       VEPH 13
C     Equations for "moderately accurate" Venus solar time, seasonal    VEPH 14
C     parameters, and one-way Venus-Earth light time, adapted from      VEPH 15
C     Allison and McEwen, Planet. Space Sci., 48, 215-235 (2000),       VEPH 16
C     and Allison, Geophys Res. Lett., 24(16), 1967-1970 (1997).        VEPH 17
C                                                                       VEPH 18
      Implicit None                                                     VEPH 19
      Double Precision xday,sunlat,sunlon,sunLsubs,radius,pi180,dt,     VEPH 20
     &  anomM,alphFMS,alsrad,helilon,alphs,EOT,pmr,gE,rE,dlat1,         VEPH 21
     &  helonE,Vm,owlt,yranom,anom0,yrtrop,veqlon0,perlon0,ecc,obl,     VEPH 22
     &  dlat2,dlat3,veqlon1,inc,anlon0,siday,rad0,ecc2,ecc3,ecc4,       VEPH 23
     &  ecc5,ecc6,Vm0,Vmday0,xE,yE,xpl,ypl,zpl,eqcenter,argper,         VEPH 24
     &  trueanom,coslat                                                 VEPH 25
      pi180 = Datan(1.0d0)/45.0d0                                       VEPH 26
C...  Days since 2000 January 1.5                                       VEPH 27
      dt = xday - 2451545.0d0                                           VEPH 28
C                                                                       VEPH 29
C.....................................................................  VEPH 30
C                                                                       VEPH 31
C...  Planetary orbit parameters                                        VEPH 32
C                                                                       VEPH 33
C     Semi-major axis (AU) = mean distance from Sun                     VEPH 34
      rad0 = 0.723330d0                                                 VEPH 35
C     Anomalistic year (days, perihelion-to-perihelion)                 VEPH 36
      yranom = 224.7d0                                                  VEPH 37
C     Tropical year (days, for rate of fictitious mean sun)             VEPH 38
      yrtrop = 224.6994d0                                               VEPH 39
C     Mean anomaly for J2000 (degrees)                                  VEPH 40
      anom0 = 50.4084d0                                                 VEPH 41
C     Heliocentric longitude of perihelion at J2000 (deg)               VEPH 42
      perlon0 = 131.5709d0                                              VEPH 43
C     Terms for heliocentric longitude at Ls=0 (deg)                    VEPH 44
      veqlon0 = 237.841d0                                               VEPH 45
      veqlon1 = 9.77d-6                                                 VEPH 46
C     Eccentricity and powers                                           VEPH 47
      ecc = 0.006773d0 - 1.302D-9*dt                                    VEPH 48
      ecc2 = ecc**2                                                     VEPH 49
      ecc3 = ecc2*ecc                                                   VEPH 50
      ecc4 = ecc3*ecc                                                   VEPH 51
      ecc5 = ecc4*ecc                                                   VEPH 52
      ecc6 = ecc5*ecc                                                   VEPH 53
C     Obliquity angle (radians)                                         VEPH 54
      obl = (177.36d0 + 0.0d0*dt)*pi180                                 VEPH 55
C     Inclination (radians)                                             VEPH 56
      inc = (3.3946d0 +2.75D-8*dt)*pi180                                VEPH 57
C     Longitude of ascending node at J2000 (deg)                        VEPH 58
      anlon0 = 76.6799d0                                                VEPH 59
C     Sidereal period of rotation (Earth days)                          VEPH 60
      siday = -243.02011d0                                              VEPH 61
C     Heliocentric lon of prime meridian (deg) at Julian day Vmday0     VEPH 62
      Vm0 = 75.1289d0                                                   VEPH 63
      Vmday0 = 2451545.0d0                                              VEPH 64
C     Difference terms, planetocentric to planetographic lat (deg)      VEPH 65
      dlat1 = -0.002d0                                                  VEPH 66
      dlat2 = 0.002d0                                                   VEPH 67
      dlat3 = 0.0d0                                                     VEPH 68
C                                                                       VEPH 69
C.....................................................................  VEPH 70
C                                                                       VEPH 71
C...  Mean anomaly (radians)                                            VEPH 72
C...  Allison & McEwen (2000) equation (16)                             VEPH 73
      anomM = (anom0 + (360.0d0/yranom)*dt)*pi180                       VEPH 74
C...  Right ascension of fictitious mean sun (deg)                      VEPH 75
C...  Allison & McEwen (2000) equation (17)                             VEPH 76
      alphFMS = perlon0 - veqlon0 + anom0 + (360.0d0/yrtrop)*dt         VEPH 77
C...  Venus equation of center, A&M eqn. (4) (degrees)                  VEPH 78
      eqcenter = ((2.0d0*ecc - 0.25d0*ecc3 +                            VEPH 79
     &  (5.0d0/96.0d0)*ecc5)*Dsin(anomM) +                              VEPH 80
     &  (1.25d0*ecc2 - (11.0d0/24.0d0)*ecc4 +                           VEPH 81
     &  (17.0d0/192.0d0)*ecc6)*Dsin(2.0d0*anomM) +                      VEPH 82
     &  ((13.0d0/12.0d0)*ecc3 - (43.0d0/63.0d0)*ecc5)*                  VEPH 83
     &  Dsin(3.0d0*anomM) + ((103.0d0/96.0d0)*ecc4 -                    VEPH 84
     &  (451.0d0/480.0d0)*ecc6)*Dsin(4.0d0*anomM) +                     VEPH 85
     &  ((1097.0d0/960.0d0)*ecc5)*Dsin(5.0d0*anomM) +                   VEPH 86
     &  ((12323.0d0/960.0d0)*ecc6)*Dsin(6.0d0*anomM))/pi180             VEPH 87
C...  True planetocentric solar longitude (Ls), A&M eqns. (2) and (4)   VEPH 88
      sunLsubs = alphFMS + eqcenter                                     VEPH 89
      Call Rescale_V05(sunLsubs)                                        VEPH 90
C...  Ls angle in radians                                               VEPH 91
      alsrad = sunLsubs*pi180                                           VEPH 92
C...  Sub-solar latitude of sun (planetographic solar declination),     VEPH 93
C     Allison (1997) eqn. (5) with empirical Ls and 3*Ls terms          VEPH 94
      sunlat = DAsin(Sin(obl)*Dsin(alsrad))/pi180 + dlat1*Dsin(alsrad)  VEPH 95
     &  + dlat2*Dcos(alsrad) + dlat3*Dsin(3.0d0*alsrad)                 VEPH 96
C...  Solar right ascension, un-numbered equation, A&M page 217         VEPH 97
      alphs = Datan2(Dcos(obl)*dSin(alsrad),dCos(alsrad))/pi180         VEPH 98
C...  Venus orbital radius, Astronomical Almanac page E4                VEPH 99
      radius = rad0*(1.0d0 - ecc2)/(1.0d0 + ecc*DCos(anomM + alsrad -   VEPH100
     &  alphFMS*pi180))                                                 VEPH101
C.... Approximate Venus heliocentric longitude, A&M eqn, (11)           VEPH102
      helilon = sunLsubs + veqlon0 - veqlon1*dt-(Dtan(0.5d0*inc)**2)*   VEPH103
     &  Dsin(2.0d0*(alsrad + (veqlon0 - anlon0)*pi180))/pi180           VEPH104
      Call Rescale_V05(helilon)                                         VEPH105
C...  Equation of time (deg)                                            VEPH106
      EOT = alphFMS - alphs                                             VEPH107
      Call Rescale_V05(EOT)                                             VEPH108
      Call Shiftdif_V05(EOT)                                            VEPH109
      Call Rescale_V05(alphs)                                           VEPH110
C...  Earth heliocentric distance and longitude, Allison eqns (20)-     VEPH111
C     (22)                                                              VEPH112
      gE = (357.528d0 + 0.9856003d0*dt)*pi180                           VEPH113
      rE = 1.00014d0 - 0.01671d0*Dcos(gE) - 0.00014d0*Dcos(2.0d0*gE)    VEPH114
      helonE = 100.472d0 + 0.9856474d0*dt + 1.915d0*Dsin(gE)            VEPH115
     &  + 0.020d0*Dsin(2.0d0*gE)                                        VEPH116
C...  Earth Cartesian coordinates                                       VEPH117
      xE = rE*dCos(helonE*pi180)                                        VEPH118
      yE = rE*dSin(helonE*pi180)                                        VEPH119
C...  Venus true anolmaly (radians)                                     VEPH120
      trueanom = eqcenter*pi180 + anomM                                 VEPH121
C...  Venus argument of perihelion (radians)                            VEPH122
      argper = (54.8910d0 + 1.38374d-5*dt)*pi180                        VEPH123
C...  Venus Cartesian coordinates                                       VEPH124
      zpl = radius*dSin(trueanom + argper)*dSin(inc)                    VEPH125
      coslat = dSqrt(1.0d0 - (zpl/radius)**2)                           VEPH126
      xpl = radius*dCos((helilon+3.82394d-5*dt)*pi180)*coslat           VEPH127
      ypl = radius*dSin((helilon+3.82394d-5*dt)*pi180)*coslat           VEPH128
C...  One-way light time (minutes), Allison eqn.(19)                    VEPH129
      owlt = dSqrt((xpl-xE)**2+(ypl-yE)**2+zpl**2)*499.005d0/60.0d0     VEPH130
C...  Venus (Heliocentric) prime meridian, Allison eqn (11)             VEPH131
      Vm = Vm0 + (360.0d0/dAbs(siday))*(xday - Vmday0)                  VEPH132
C...  Sub-solar longitude from true solar time at prime meridian,       VEPH133
C     A&M page 217                                                      VEPH134
      pmr = (Vm - alphs)/360.0d0                                        VEPH135
      sunlon = (pmr - Dint(pmr))*360.0d0 + 180.0d0                      VEPH136
      Call Rescale_V05(sunlon)                                          VEPH137
      Return                                                            VEPH138
      End                                                               VEPH139
C---------------------------------------------------------------------- VEPH140
      Subroutine SolZenAng_V05(sunlat,sunlon,sitelat,sitelon,sza)       SZAN  1
C...  Solar zenith angle (sza, degrees) from latitude and longitude     SZAN  2
C     of sun and site (degrees)                                         SZAN  3
      Implicit None                                                     SZAN  4
      Double Precision sunlat,sunlon,sitelat,sitelon,sza,pi180,csza     SZAN  5
      pi180 = dAtan(1.0d0)/45.0d0                                       SZAN  6
C...  Cosine of solar zenith angle                                      SZAN  7
      csza = dSin(sunlat*pi180)*dSin(sitelat*pi180) +                   SZAN  8
     &  dCos(sunlat*pi180)*dCos(sitelat*pi180)*                         SZAN  9
     &  dCos(pi180*(sunlon-sitelon))                                    SZAN 10
C...  Solar zenith angle                                                SZAN 11
      sza = dAcos(csza)/pi180                                           SZAN 12
      Return                                                            SZAN 13
      End                                                               SZAN 14
C---------------------------------------------------------------------- SZAN 15
      Subroutine Lowterp_V05(i,clat,p,d,t,zeta,Rgas,yco2,yn2,yo,yco)    LTRP  1
C...  Interpolation routine for low altitude VIRA data (0-100 km)       LTRP  2
C     Computes pressure, density, temperature (p,d,t), compressibility  LTRP  3
C     (zeta), gas constant (Rgas), and number densities (yco2,yn2,yo,   LTRP  4
C     and yco) from VIRA low-altitude index (i) and current latitude    LTRP  5
C     (clat)                                                            LTRP  6
      Implicit None                                                     LTRP  7
      Double Precision clat,p,d,t,zeta,Rgas,zlo,plo,dlo,tlo,zetalo,     LTRP  8
     &  Rlo,zmd,pmd,dmd,tmd,yco2md,yn2md,yomd,ycomd,yhemd,ynmd,R0,      LTRP  9
     &  zhi,phi,dhi,thi,yco2hi,yn2hi,yohi,ycohi,yhehi,ynhi,yhhi,AM,     LTRP 10
     &  vlat(5),xlat,flat,yco2,yn2,yo,yco,fco2,fn2,fo,fco,AVn,AMx       LTRP 11
      Integer i,j,jm                                                    LTRP 12
C...  Venus model VIRA data at low ,middle, and high altitudes          LTRP 13
      Common /VIRAdata_V05/zlo(81),plo(81,5),dlo(81,5),tlo(81,5),       LTRP 14
     &  zetalo(81,5),Rlo(81,5),zmd(11),pmd(11,2),dmd(11,2),tmd(11,2),   LTRP 15
     &  yco2md(11,2),yn2md(11,2),yomd(11,2),ycomd(11,2),yhemd(11,2),    LTRP 16
     &  ynmd(11,2),zhi(21),phi(21,7),dhi(21,7),                         LTRP 17
     &  thi(21,7),yco2hi(21,7),yn2hi(21,7),yohi(21,7),ycohi(21,7),      LTRP 18
     &  yhehi(21,7),ynhi(21,7),yhhi(21,7)                               LTRP 19
C...  Latitudes for low-altitude VIRA data                              LTRP 20
      Data vlat/30.0d0,45.0d0,60.0d0,75.0d0,85.0d0/                     LTRP 21
C...  Universal gas constant                                            LTRP 22
      R0 = 8314.472d0                                                   LTRP 23
C...  Avogadro's number                                                 LTRP 24
      AVn = 6.02214199d26                                               LTRP 25
      xlat = dAbs(clat)                                                 LTRP 26
C...  Use VIRA data for lat=0-30 if abs(lat)<30                         LTRP 27
      If (xlat.le.vlat(1))Then                                          LTRP 28
        p = plo(i,1)                                                    LTRP 29
        d = dlo(i,1)                                                    LTRP 30
        t = tlo(i,1)                                                    LTRP 31
        zeta = zetalo(i,1)                                              LTRP 32
        Rgas = Rlo(i,1)                                                 LTRP 33
C...  Use VIRA data for lat=85-90 if abs(lat)>85                        LTRP 34
      Else If (xlat.ge.vlat(5))Then                                     LTRP 35
        p = plo(i,5)                                                    LTRP 36
        d = dlo(i,5)                                                    LTRP 37
        t = tlo(i,5)                                                    LTRP 38
        zeta = zetalo(i,5)                                              LTRP 39
        Rgas = Rlo(i,5)                                                 LTRP 40
      Else                                                              LTRP 41
C...    Find latitude interpolation index j                             LTRP 42
        Do 10 j = 2,5                                                   LTRP 43
          If (xlat.le.vlat(j))Then                                      LTRP 44
            jm = j - 1                                                  LTRP 45
C...        Get latitude interpolation factor                           LTRP 46
            flat = (xlat - vlat(jm))/(vlat(j) - vlat(jm))               LTRP 47
C...        Logarithmic interpolation on pressure                       LTRP 48
            p = dExp(dLog(plo(i,jm)) + dLog(plo(i,j)/plo(i,jm))*flat)   LTRP 49
C...        Linear interpolation on temperature, zeta, and gas constant LTRP 50
            t = tlo(i,jm) + (tlo(i,j)-tlo(i,jm))*flat                   LTRP 51
            zeta = zetalo(i,jm) + (zetalo(i,j)-zetalo(i,jm))*flat       LTRP 52
            Rgas = Rlo(i,jm) + (Rlo(i,j)-Rlo(i,jm))*flat                LTRP 53
C,,,        Density from perfect gas law (with compressibility zeta)    LTRP 54
            d = p/(zeta*Rgas*t)                                         LTRP 55
            Goto 20                                                     LTRP 56
          Endif                                                         LTRP 57
  10    Enddo                                                           LTRP 58
      Endif                                                             LTRP 59
  20  AM = R0/Rgas                                                      LTRP 60
C...  Use uniform mixing ratios of CO2&N2 for heights up to 82 km       LTRP 61
      If (i.le.73)Then                                                  LTRP 62
        fco2 = 0.965d0                                                  LTRP 63
        fn2 = 0.035d0                                                   LTRP 64
        fco = 0.0                                                       LTRP 65
        fo = 0.0                                                        LTRP 66
      Else                                                              LTRP 67
C...    Use height-variable CO2/N2/O/CO mixing ratios from 82 to 100 km LTRP 68
        AMx = AM                                                        LTRP 69
        If (AMx.gt.43.44d0)AMx = 43.44d0                                LTRP 70
C...    Get mixing ratios from mean molecular weight                    LTRP 71
        fco2 = -1.723936d0 + 6.19d-2*AMx                                LTRP 72
        fn2 = 2.515424d0 - 5.71d-2*AMx                                  LTRP 73
        fo = 3.4752d-2 - 8.0d-4*AMx                                     LTRP 74
        fco = 0.17376d0 - 4.0d-3*AMx                                    LTRP 75
        If (fo.le.0.0)fo = 0.0                                          LTRP 76
        If (fco.le.0.0)fco = 0.0                                        LTRP 77
      Endif                                                             LTRP 78
C...  Get number densities from mixing ratios and mass density          LTRP 79
      yco2 = fco2*d*AVn/AM                                              LTRP 80
      yn2 = fn2*d*AVn/AM                                                LTRP 81
      yco = fco*d*AVn/AM                                                LTRP 82
      yo = fo*d*AVn/AM                                                  LTRP 83
      Return                                                            LTRP 84
      End                                                               LTRP 85
C---------------------------------------------------------------------- LTRP 86
      Subroutine Midterp_V05(i,time,clat,p,d,t,yco2,yn2,yo,yco,yhe,yn)  MTRP  1
C...  Interpolation routine for middle altitude VIRA data (100-150 km)  MTRP  2
C     Computes pressure, density, temperature (p,d,t), and number       MTRP  3
C     densities (yco2,yn2,yo, yco, yhe and yn) from VIRA midddle-       MTRP  4
C     altitude index (i), current local solar time (time) and current   MTRP  5
C     latitude (clat)                                                   MTRP  6
      Implicit None                                                     MTRP  7
      Double Precision time,p,d,t,zlo,plo,dlo,tlo,zetalo,               MTRP  8
     &  Rlo,zmd,pmd,dmd,tmd,yco2md,yn2md,yomd,ycomd,yhemd,ynmd,         MTRP  9
     &  zhi,phi,dhi,thi,yco2hi,yn2hi,yohi,ycohi,yhehi,ynhi,yhhi,        MTRP 10
     &  yco2,yn2,yo,yco,yhe,yn,ftime,A,B,R1,R2,Rgas,clat,alat,flat      MTRP 11
      Integer i                                                         MTRP 12
C...  Venus model VIRA data at low ,middle, and high altitudes          MTRP 13
      Common /VIRAdata_V05/zlo(81),plo(81,5),dlo(81,5),tlo(81,5),       MTRP 14
     &  zetalo(81,5),Rlo(81,5),zmd(11),pmd(11,2),dmd(11,2),tmd(11,2),   MTRP 15
     &  yco2md(11,2),yn2md(11,2),yomd(11,2),ycomd(11,2),yhemd(11,2),    MTRP 16
     &  ynmd(11,2),zhi(21),phi(21,7),dhi(21,7),                         MTRP 17
     &  thi(21,7),yco2hi(21,7),yn2hi(21,7),yohi(21,7),ycohi(21,7),      MTRP 18
     &  yhehi(21,7),ynhi(21,7),yhhi(21,7)                               MTRP 19
C...  Diurnal time variation parameter                                  MTRP 20
      ftime = dSin(dAtan(1.0d0)*(time - 6.0)/3.0d0)                     MTRP 21
C...  Latitude factor (suppresses diurnal variation near poles)         MTRP 22
      flat = 1.0d0                                                      MTRP 23
      alat = dAbs(clat)                                                 MTRP 24
      If (alat.ge.70.0d0)flat = 1.0d0 - ((alat-70.0d0)/20.0d0)**2       MTRP 25
C...  Mean (A) and diurnal amplitude (B) for temperature                MTRP 26
      A = 0.5d0*(tmd(i,2) + tmd(i,1))                                   MTRP 27
      B = 0.5d0*(tmd(i,2) - tmd(i,1))*flat                              MTRP 28
C...  Time-dependent temperature                                        MTRP 29
      t = A + B*ftime                                                   MTRP 30
C...  Mean (A) and diurnal amplitude (B) for pressure                   MTRP 31
      A = 0.5d0*(dLog(pmd(i,2)*pmd(i,1)))                               MTRP 32
      B = 0.5d0*(dLog(pmd(i,2)/pmd(i,1)))*flat                          MTRP 33
C...  Time-dependent pressure                                           MTRP 34
      p = dExp(A + B*ftime)                                             MTRP 35
C...  Gas constant at LST = 0 and 12 hr                                 MTRP 36
      R1 = pmd(i,1)/(dmd(i,1)*tmd(i,1))                                 MTRP 37
      R2 = pmd(i,2)/(dmd(i,2)*tmd(i,2))                                 MTRP 38
C...  Mean (A) and diurnal amplitude (B) for gas constant               MTRP 39
      A = 0.5d0*(R2 + R1)                                               MTRP 40
      B = 0.5d0*(R2 - R1)*flat                                          MTRP 41
C...  Time-dependent gas constant                                       MTRP 42
      Rgas = A + B*ftime                                                MTRP 43
C...  Density from perfect gas law                                      MTRP 44
      d = p/(Rgas*t)                                                    MTRP 45
C...  Mean (A) and diurnal amplitude (B) for CO2 number density         MTRP 46
      A = 0.5d0*(dLog(yco2md(i,2)*yco2md(i,1)))                         MTRP 47
      B = 0.5d0*(dLog(yco2md(i,2)/yco2md(i,1)))*flat                    MTRP 48
C...  Time-dependent CO2 number density                                 MTRP 49
      yco2 = dExp(A + B*ftime)                                          MTRP 50
C...  Mean (A) and diurnal amplitude (B) for N2 number density          MTRP 51
      A = 0.5d0*(dLog(yn2md(i,2)*yn2md(i,1)))                           MTRP 52
      B = 0.5d0*(dLog(yn2md(i,2)/yn2md(i,1)))*flat                      MTRP 53
C...  Time-dependent N2 number density                                  MTRP 54
      yn2 = dExp(A + B*ftime)                                           MTRP 55
C...  Mean (A) and diurnal amplitude (B) for O number density           MTRP 56
      A = 0.5d0*(dLog(yomd(i,2)*yomd(i,1)))                             MTRP 57
      B = 0.5d0*(dLog(yomd(i,2)/yomd(i,1)))*flat                        MTRP 58
C...  Time-dependent O number density                                   MTRP 59
      yo = dExp(A + B*ftime)                                            MTRP 60
C...  Mean (A) and diurnal amplitude (B) for CO number density          MTRP 61
      A = 0.5d0*(dLog(ycomd(i,2)*ycomd(i,1)))                           MTRP 62
      B = 0.5d0*(dLog(ycomd(i,2)/ycomd(i,1)))*flat                      MTRP 63
C...  Time-dependent CO number density                                  MTRP 64
      yco = dExp(A + B*ftime)                                           MTRP 65
C...  Mean (A) and diurnal amplitude (B) for He number density          MTRP 66
      A = 0.5d0*(dLog(yhemd(i,2)*yhemd(i,1)))                           MTRP 67
      B = 0.5d0*(dLog(yhemd(i,2)/yhemd(i,1)))*flat                      MTRP 68
C...  Time-dependent He number density                                  MTRP 69
      yhe = dExp(A + B*ftime)                                           MTRP 70
C...  Mean (A) and diurnal amplitude (B) for N number density           MTRP 71
      A = 0.5d0*(dLog(ynmd(i,2)*ynmd(i,1)))                             MTRP 72
      B = 0.5d0*(dLog(ynmd(i,2)/ynmd(i,1)))*flat                        MTRP 73
C...  Time-dependent N number density                                   MTRP 74
      yn = dExp(A + B*ftime)                                            MTRP 75
      Return                                                            MTRP 76
      End                                                               MTRP 77
C---------------------------------------------------------------------- MTRP 78
      Subroutine Highterp_V05(i,sza,p,d,t,yco2,yn2,yo,yco,yhe,yn,yh)    HTRP  1
C...  Interpolation routine for high altitude VIRA data (150-250 km)    HTRP  2
C     Computes pressure, density, temperature (p,d,t), and number       HTRP  3
C     densities (yco2,yn2,yo, yco, yhe, yn, and yh) from VIRA high-     HTRP  4
C     altitude index (i), and current solar zenith angle (sza)          HTRP  5
      Implicit None                                                     HTRP  6
      Double Precision sza,p,d,t,zlo,plo,dlo,tlo,zetalo,R1,R2,Rgas,     HTRP  7
     &  Rlo,zmd,pmd,dmd,tmd,yco2md,yn2md,yomd,ycomd,yhemd,ynmd,         HTRP  8
     &  zhi,phi,dhi,thi,yco2hi,yn2hi,yohi,ycohi,yhehi,ynhi,yhhi,        HTRP  9
     &  vsza(7),xsza,yco2,yn2,yo,yco,yhe,yn,yh,fsza                     HTRP 10
      Integer i,j,jm                                                    HTRP 11
C...  Venus model VIRA data at low ,middle, and high altitudes          HTRP 12
      Common /VIRAdata_V05/zlo(81),plo(81,5),dlo(81,5),tlo(81,5),       HTRP 13
     &  zetalo(81,5),Rlo(81,5),zmd(11),pmd(11,2),dmd(11,2),tmd(11,2),   HTRP 14
     &  yco2md(11,2),yn2md(11,2),yomd(11,2),ycomd(11,2),yhemd(11,2),    HTRP 15
     &  ynmd(11,2),zhi(21),phi(21,7),dhi(21,7),                         HTRP 16
     &  thi(21,7),yco2hi(21,7),yn2hi(21,7),yohi(21,7),ycohi(21,7),      HTRP 17
     &  yhehi(21,7),ynhi(21,7),yhhi(21,7)                               HTRP 18
C,,,  Solar zenith angles for high-altitude VIRA data                   HTRP 19
      Data vsza/15.0d0,34.0d0,61.0d0,90.0d0,119.0d0,146.0d0,165.0d0/    HTRP 20
      xsza = dAbs(sza)                                                  HTRP 21
C...  Use VIRA data for 0-15 deg if abs(sza) < 15 deg                   HTRP 22
      If (xsza.le.vsza(1))Then                                          HTRP 23
        p = phi(i,1)                                                    HTRP 24
        d = dhi(i,1)                                                    HTRP 25
        t = thi(i,1)                                                    HTRP 26
        yco2 = yco2hi(i,1)                                              HTRP 27
        yn2 = yn2hi(i,1)                                                HTRP 28
        yo = yohi(i,1)                                                  HTRP 29
        yco = ycohi(i,1)                                                HTRP 30
        yhe = yhehi(i,1)                                                HTRP 31
        yn = ynhi(i,1)                                                  HTRP 32
        yh = yhhi(i,1)                                                  HTRP 33
C...  Use VIRA data for 165 deg if abs(sza) > 165 deg                   HTRP 34
      Else If (xsza.ge.vsza(7))Then                                     HTRP 35
        p = phi(i,7)                                                    HTRP 36
        d = dhi(i,7)                                                    HTRP 37
        t = thi(i,7)                                                    HTRP 38
        yco2 = yco2hi(i,7)                                              HTRP 39
        yn2 = yn2hi(i,7)                                                HTRP 40
        yo = yohi(i,7)                                                  HTRP 41
        yco = ycohi(i,7)                                                HTRP 42
        yhe = yhehi(i,7)                                                HTRP 43
        yn = ynhi(i,7)                                                  HTRP 44
        yh = yhhi(i,7)                                                  HTRP 45
      Else                                                              HTRP 46
C...    Find sza interpolation index j                                  HTRP 47
        Do 10 j = 2,7                                                   HTRP 48
          If (xsza.le.vsza(j))Then                                      HTRP 49
            jm = j - 1                                                  HTRP 50
C...        Solar zenith angle interpolation factor                     HTRP 51
            fsza = (xsza - vsza(jm))/(vsza(j) - vsza(jm))               HTRP 52
C...        Logarithmic interpolation on pressure                       HTRP 53
            p = dExp(dLog(phi(i,jm)) + dLog(phi(i,j)/phi(i,jm))*fsza)   HTRP 54
C...        Linear interpolation on temperature and gas constant        HTRP 55
            t = thi(i,jm) + (thi(i,j)-thi(i,jm))*fsza                   HTRP 56
            R1 = phi(i,jm)/(dhi(i,jm)*thi(i,jm))                        HTRP 57
            R2 = phi(i,j)/(dhi(i,j)*thi(i,j))                           HTRP 58
            Rgas = R1 + (R2-R1)*fsza                                    HTRP 59
C...        Density from perfect gas law                                HTRP 60
            d = p/(Rgas*t)                                              HTRP 61
C...        Logarithmic interpolation for number densities              HTRP 62
            yco2 = dExp(dLog(yco2hi(i,jm)) +                            HTRP 63
     &        dLog(yco2hi(i,j)/yco2hi(i,jm))*fsza)                      HTRP 64
            yn2 = dExp(dLog(yn2hi(i,jm)) +                              HTRP 65
     &        dLog(yn2hi(i,j)/yn2hi(i,jm))*fsza)                        HTRP 66
            yo=dExp(dLog(yohi(i,jm))+dLog(yohi(i,j)/yohi(i,jm))*fsza)   HTRP 67
            yco = dExp(dLog(ycohi(i,jm)) +                              HTRP 68
     &        dLog(ycohi(i,j)/ycohi(i,jm))*fsza)                        HTRP 69
            yhe = dExp(dLog(yhehi(i,jm)) +                              HTRP 70
     &        dLog(yhehi(i,j)/yhehi(i,jm))*fsza)                        HTRP 71
            yn=dExp(dLog(ynhi(i,jm))+dLog(ynhi(i,j)/ynhi(i,jm))*fsza)   HTRP 72
            yh=dExp(dLog(yhhi(i,jm))+dLog(yhhi(i,j)/yhhi(i,jm))*fsza)   HTRP 73
            Return                                                      HTRP 74
          Endif                                                         HTRP 75
  10    Enddo                                                           HTRP 76
      Endif                                                             HTRP 77
      Return                                                            HTRP 78
      End                                                               HTRP 79
C---------------------------------------------------------------------- HTRP 80
      Subroutine Thermos_V05(z,clat,zf,yco2f,yn2f,yof,ycof,yhef,ynf,    THRM  1
     &  yhf,Tf,presz,densz,AMz,yco2,yn2,yo,yco,yhe,yn,yh,totnd,hscale)  THRM  2
C...  Special Venus thermosphere model asssuming constant exospheric    THRM  3
C     temperature above height zf (250 km)                              THRM  4
C...  Evaluates pressure (presz), density (densz), molecular weight     THRM  5
C     (AMz), scale height (hscale), and number densities yxxx for 7     THRM  6
C     species, given conditions at height zf (= 250 km), for altitude   THRM  7
C     (z) and latitude (clat)                                           THRM  8
      Implicit None                                                     THRM  9
      Double Precision z,zf,yco2f,yn2f,yof,ycof,yhef,ynf,yhf,Tf,presz,  THRM 10
     &  densz,AMz,yco2,yn2,yo,yco,yhe,yn,yh,totnd,amh,amco2,amhe,amn2,  THRM 11
     &  amo,amco,amn,zzf,bk,R0,Rf,Rsfc,gf,clat,y,ppco2f,ppn2f,ppof,     THRM 12
     &  ppcof,pphef,ppnf,pphf,Tinf,Tz,Hh,Hco2,Hhe,Hn2,Ho,Hco,Hn,ymin,   THRM 13
     &  pph,ppco2,pphe,ppn2,ppo,ppco,ppn,hscale,gz                      THRM 14
C...  Molecular weights for atmospheric constituents                    THRM 15
      Data amh,amco2,amhe,amn2,amo,amco,amn/1.01d0,44.0d0,4.0d0,28.0d0, THRM 16
     &  16.0d0,28.0d0,14.0d0/                                           THRM 17
C...  Height above 250 km                                               THRM 18
      zzf = z - zf                                                      THRM 19
C...  Boltzmann constant                                                THRM 20
      bk = 1.38065D-23                                                  THRM 21
C...  Universal gas constant                                            THRM 22
      R0 = 8314.472D0                                                   THRM 23
C...  Surface radius (Rsfc) and gravity (gf) at height zf               THRM 24
      Call rgplanet_V05(clat,Rsfc,zf,gf,2)                              THRM 25
C...  Radius at height zf                                               THRM 26
      Rf = Rsfc + zf                                                    THRM 27
C...  Partial pressures of constituents at height zf                    THRM 28
      ppco2f = bk*yco2f*Tf                                              THRM 29
      ppn2f = bk*yn2f*Tf                                                THRM 30
      ppof = bk*yof*Tf                                                  THRM 31
      ppcof = bk*ycof*Tf                                                THRM 32
      pphef = bk*yhef*Tf                                                THRM 33
      ppnf = bk*ynf*Tf                                                  THRM 34
      pphf = bk*yhf*Tf                                                  THRM 35
C...  Height parameter                                                  THRM 36
      y = zzf*Rf/(Rf + zzf)                                             THRM 37
C...  Assume constant temperature versus height                         THRM 38
      Tinf = tf                                                         THRM 39
      Tz = tf                                                           THRM 40
C...  Get scale height, partial pressure, and number density of each    THRM 41
C     constituent                                                       THRM 42
      Hco2 = R0*Tinf/(1000.0d0*amco2*gf)                                THRM 43
      ppco2 = ppco2f*Exp(-y/Hco2)                                       THRM 44
      yco2 = ppco2/(bk*Tz)                                              THRM 45
      Hn2 = R0*Tinf/(1000.0d0*amn2*gf)                                  THRM 46
      ppn2 = ppn2f*Exp(-y/Hn2)                                          THRM 47
      yn2 = ppn2/(bk*Tz)                                                THRM 48
      Ho = R0*Tinf/(1000.0d0*amo*gf)                                    THRM 49
      ppo = ppof*Exp(-y/Ho)                                             THRM 50
      yo = ppo/(bk*Tz)                                                  THRM 51
      Hco = R0*Tinf/(1000.0d0*amco*gf)                                  THRM 52
      ppco = ppcof*Exp(-y/Hco)                                          THRM 53
      yco = ppco/(bk*Tz)                                                THRM 54
      Hhe = R0*Tinf/(1000.0d0*amhe*gf)                                  THRM 55
      pphe = pphef*Exp(-y/Hhe)                                          THRM 56
      yhe = pphe/(bk*Tz)                                                THRM 57
      Hn = R0*Tinf/(1000.0d0*amn*gf)                                    THRM 58
      ppn = ppnf*Exp(-y/Hn)                                             THRM 59
      yn = ppn/(bk*Tz)                                                  THRM 60
      Hh = R0*Tinf/(1000.0d0*amh*gf)                                    THRM 61
      pph = pphf*Exp(-y/Hh)                                             THRM 62
      yh = pph/(bk*Tz)                                                  THRM 63
C...  Limit number densities to > 1 per km**3                           THRM 64
      ymin = 1.0d-9                                                     THRM 65
      If (yco2.lt.ymin)yco2=ymin                                        THRM 66
      If (yn2.lt.ymin)yn2=ymin                                          THRM 67
      If (yco.lt.ymin)yco=ymin                                          THRM 68
C...  Get total pressure from partial pressures                         THRM 69
      presz = ppco2 + ppn2 + ppo + ppco + pphe + ppn + pph              THRM 70
C...  Get total number density from constituent number densities        THRM 71
      totnd = yco2 + yn2 + yo + yco + yhe + yn + yh                     THRM 72
C...  Get mean molecular weight                                         THRM 73
      Amz = (yco2*amco2+yn2*amn2+yo*amo+yco*amco+yhe*amhe+yn*amn+       THRM 74
     &  yh*amh)/totnd                                                   THRM 75
C...  Get density from perfect gas law                                  THRM 76
      densz = presz*Amz/(R0*Tz)                                         THRM 77
C...  Get gravity at height z                                           THRM 78
      Call rgplanet_V05(clat,Rsfc,z,gz,2)                               THRM 79
C...  Get pressure scale height                                         THRM 80
      hscale = R0*Tz/(1000.0d0*AMz*gz)                                  THRM 81
      Return                                                            THRM 82
      End                                                               THRM 83
C---------------------------------------------------------------------- THRM 84
        Subroutine ProfTerp_V05(chgt,clat,clon,tin,pin,din,uin,vin,     PTRP  1
     &    ptemp,ppres,pdens,puwin,pvwin,nprof,profnear,proffar,profwgt) PTRP  2
C---    Interpolates profile data to current position (chgt,clat,clon)  PTRP  3
C       and weights results (with factor profwgt) with input values     PTRP  4
C       (tin,pin,din,uin,vin), yielding weighted average (ptemp,ppres,  PTRP  5
C       pdens,puwin,pvwin).  Input profnear is lat-lon radius over      PTRP  6
C       which profile is weighted with 1.0; proffar is lat-lon radius   PTRP  7
C       beyond which profile is given zero weight.                      PTRP  8
        Implicit Double Precision (A-H,O-Z)                             PTRP  9
        Parameter (npmax = 100000)                                      PTRP 10
        Common /pterp_V05/phgt(npmax),plat(npmax),plon(npmax),          PTRP 11
     &    ptmp(npmax),pprs(npmax),pden(npmax),puwn(npmax),pvwn(npmax)   PTRP 12
C---    Calculate pi/2                                                  PTRP 13
        pi2 = 2.0d0*dAtan(1.0d0)                                        PTRP 14
        If (chgt.lt.phgt(1))Then                                        PTRP 15
C---    Profile weighting zero for height below 1st profile data point  PTRP 16
          profwgt = 0.0d0                                               PTRP 17
          pilat = plat(1)                                               PTRP 18
          pilon = plon(1)                                               PTRP 19
          ptemp = ptmp(1)                                               PTRP 20
          ppres = pprs(1)                                               PTRP 21
          pdens = pden(1)                                               PTRP 22
          puwin = puwn(1)                                               PTRP 23
          pvwin = pvwn(1)                                               PTRP 24
        Else If (chgt.gt.phgt(nprof))Then                               PTRP 25
C---    Profile weighting zero for height above last profile data point PTRP 26
          profwgt = 0.0d0                                               PTRP 27
          pilat = plat(nprof)                                           PTRP 28
          pilon = plon(nprof)                                           PTRP 29
          ptemp = ptmp(nprof)                                           PTRP 30
          ppres = pprs(nprof)                                           PTRP 31
          pdens = pden(nprof)                                           PTRP 32
          puwin = puwn(nprof)                                           PTRP 33
          pvwin = pvwn(nprof)                                           PTRP 34
        Else                                                            PTRP 35
C---    Find index values i1 and i2=i1+1 bracketing current height      PTRP 36
          Do 20 i = 1,nprof-1                                           PTRP 37
            If (chgt.ge.phgt(i).and.chgt.le.phgt(i+1))Then              PTRP 38
              i1 = i                                                    PTRP 39
              i2 = i + 1                                                PTRP 40
              Goto 25                                                   PTRP 41
            Endif                                                       PTRP 42
   20     Enddo                                                         PTRP 43
C---      Compute factor for linear height interpolation                PTRP 44
   25     factor = (chgt - phgt(i1))/(phgt(i2)-phgt(i1))                PTRP 45
C---      Linear height interpolation for lat,lon,temperature,winds     PTRP 46
          pilat = plat(i1) + factor*(plat(i2)-plat(i1))                 PTRP 47
          pilon = plon(i1) + factor*(plon(i2)-plon(i1))                 PTRP 48
          ptemp = ptmp(i1) + factor*(ptmp(i2)-ptmp(i1))                 PTRP 49
          puwin = puwn(i1) + factor*(puwn(i2)-puwn(i1))                 PTRP 50
          pvwin = pvwn(i1) + factor*(pvwn(i2)-pvwn(i1))                 PTRP 51
C---      Power-law interpolation for pressure (unless profile pressure PTRP 52
C         is zero, for which zero weight will be used)                  PTRP 53
          pdens = 0.0d0                                                 PTRP 54
          If (pden(i1).gt.0.0d0)pdens = pden(i1)*                       PTRP 55
     &       (pden(i2)/pden(i1))**factor                                PTRP 56
C---      Power-law interpolation for density (unless profile density   PTRP 57
C         is zero, for which zero weight will be used)                  PTRP 58
          ppres = 0.0d0                                                 PTRP 59
          If (pprs(i1).gt.0.0d0)ppres = pprs(i1)*                       PTRP 60
     &      (pprs(i2)/pprs(i1))**factor                                 PTRP 61
C---      Initialize weighting factor components for height and lat-lon PTRP 62
          facthgt = 1.0d0                                               PTRP 63
          factll = 1.0d0                                                PTRP 64
          If (chgt.le.phgt(2))Then                                      PTRP 65
C---      Sin-squared variation of height weighting from 0 at 1st point PTRP 66
C         to 1 at 2nd point                                             PTRP 67
            facthgt=(chgt-phgt(1))/(phgt(2)-phgt(1))                    PTRP 68
            facthgt = (dSin(pi2*facthgt))**2                            PTRP 69
          Else If (chgt.ge.phgt(nprof-1))Then                           PTRP 70
C---      Sin-squared variation of height weighting from 0 at next-to-  PTRP 71
C         last point to 1 at last point                                 PTRP 72
            facthgt=(chgt-phgt(nprof))/(phgt(nprof-1)-phgt(nprof))      PTRP 73
            facthgt = (dSin(pi2*facthgt))**2                            PTRP 74
          Endif                                                         PTRP 75
C---      Compute absolute lat-lon difference of current position from  PTRP 76
C         profile lat-lon                                               PTRP 77
          dlat = dAbs(clat - pilat)                                     PTRP 78
          dlon = dAbs(clon - pilon)                                     PTRP 79
C---      Adjust lon difference for wrap at lon 360                     PTRP 80
          If (dlon.gt.180.0d0)dlon = 360.0d0 - dlon                     PTRP 81
C---      Lat-lon radius of current position from profile lat-lon       PTRP 82
          radius = dSqrt(dlat**2 + dlon**2)                             PTRP 83
C---      Use weight=0 if radius>proffar, weight=1 if radius<profnear,  PTRP 84
C         with sin-squared variation between proffar and profnear       PTRP 85
          If (radius.ge.proffar)Then                                    PTRP 86
            factll = 0.0d0                                              PTRP 87
          Else If (radius.le.profnear)Then                              PTRP 88
            factll = 1.0d0                                              PTRP 89
          Else                                                          PTRP 90
            factll = (proffar-radius)/(proffar - profnear)              PTRP 91
            factll = (dSin(pi2*factll))**2                              PTRP 92
          Endif                                                         PTRP 93
C---      Total weight = product of weights for lat-lon and height      PTRP 94
          profwgt = factll*facthgt                                      PTRP 95
        Endif                                                           PTRP 96
        tpdwgt = profwgt                                                PTRP 97
        uvwgt = profwgt                                                 PTRP 98
C---    Set profile weight to zero for p,d, & t if profile values are 0 PTRP 99
        If (ptemp*ppres*pdens.eq.0.0d0)tpdwgt=0.0d0                     PTRP100
C---    Set profile weight to zero for u & v if profile values are 0    PTRP101
        If (dAbs(puwin)+dAbs(pvwin).eq.0.0d0)uvwgt = 0.0d0              PTRP102
C---    Apply weighted averaging of profile values with input values    PTRP103
        ptemp = tpdwgt*ptemp + (1.0d0 - tpdwgt)*tin                     PTRP104
        ppres = tpdwgt*ppres + (1.0d0 - tpdwgt)*pin                     PTRP105
        pdens = tpdwgt*pdens + (1.0d0 - tpdwgt)*din                     PTRP106
        puwin = uvwgt*puwin + (1.0d0 - uvwgt)*uin                       PTRP107
        pvwin = uvwgt*pvwin + (1.0d0 - uvwgt)*vin                       PTRP108
        Return                                                          PTRP109
        End                                                             PTRP110
C---------------------------------------------------------------------- PTRP111
        Subroutine RdProf_V05(profile,nprof,LonEast)                    RDPF  1
C---    Reads alternate profile data file profile. Returns number of    RDPF  2
C       lines of data (nprof).  Converts input longitudes from East to  RDPF  3
C       West if LonEast = 1                                             RDPF  4
        Implicit Double Precision (A-H,O-Z)                             RDPF  5
        Character*60 profile                                            RDPF  6
        Character*1 dummy                                               RDPF  7
        Parameter (npmax = 100000)                                      RDPF  8
        Common /pterp_V05/phgt(npmax),plat(npmax),plon(npmax),          RDPF  9
     &    ptmp(npmax),pprs(npmax),pden(npmax),puwn(npmax),pvwn(npmax)   RDPF 10
C---    Compute string length for profile file name                     RDPF 11
        lenprof = index(profile,' ')-1                                  RDPF 12
        If (lenprof.lt.1.or.lenprof.gt.60)lendir = 60                   RDPF 13
C---    Open profile data file                                          RDPF 14
        Open(33,file=profile(1:lenprof),status='old')                   RDPF 15
C---    Read and ignore header line                                     RDPF 16
        Read(33,5)dummy                                                 RDPF 17
   5    Format(A1)                                                      RDPF 18
        n = 0                                                           RDPF 19
C---    Start of loop to read profile data                              RDPF 20
  10    Read(33,*,End=99)zhgt,xlat,xlon,t,p,d,u,v                       RDPF 21
C---    Convert negative longitudes                                     RDPF 22
        If (xlon.lt.0.0d0)xlon = xlon + 360.0d0                         RDPF 23
C---    Convert to West Longitude if LonEast = 1                        RDPF 24
        If (LonEast.eq.1)xlon = 360.0d0 - xlon                          RDPF 25
C---    Count number of lines read                                      RDPF 26
        n = n + 1                                                       RDPF 27
C---    Store profile data in arrays, for common pterp_V05              RDPF 28
        phgt(n) = zhgt                                                  RDPF 29
C---    Stop if two successive heights are the same                     RDPF 30
        If (n.gt.1.and.phgt(n).eq.phgt(n-1))                            RDPF 31
     &    Stop ' Consecutive profile heights cannot be same'            RDPF 32
        plat(n) = xlat                                                  RDPF 33
        plon(n) = xlon                                                  RDPF 34
        ptmp(n) = t                                                     RDPF 35
        pprs(n) = p                                                     RDPF 36
        pden(n) = d                                                     RDPF 37
        puwn(n) = u                                                     RDPF 38
        pvwn(n) = v                                                     RDPF 39
        nprof = n                                                       RDPF 40
C---    Cycle back to read another line of profile data                 RDPF 41
        Goto 10                                                         RDPF 42
C---    Close profile input file when end-of-file encountered           RDPF 43
  99    Close(33)                                                       RDPF 44
        Return                                                          RDPF 45
        End                                                             RDPF 46
C---------------------------------------------------------------------- RDPF 47
