      Subroutine Venustraj_V05(isetup,jmonte,NMONTE,istep,MAXNUM,zhgt,  VENT  1
     & zlat,zlon,zsec,pzhgt,pzlat,pzlon,pzsec,DAY0,TEMP,PRES,           VENT  2
     & DENSLO,DENS,DENSHI,DENSP,RHOd,RHOu,RHOv,EWWIND,EWpert,NSWIND,    VENT  3
     & NSpert,EOF,NR1,DENSTOT,Hrho,corlim,INPUTFL,iustdout,fmolCO2,     VENT  4
     & fmolN2,fmolO,fmolCO,fmolHe,fmolN,fmolH,AMz,iupdate,pertstep,     VENT  5
     & dsunlat,dsunlon,dsunLs,dradau,dowlt,ALS,SZA,owlt,sunlat,sunlon,  VENT  6
     & VenusAU,TLOCAL)                                                  VENT  7
C                                                                       VENT  8
C     Input argument list variables, supplied from trajectory code:     VENT  9
C                                                                       VENT 10
C     isetup = 1 for Venus-GRAM setup mode (read NAMELIST INPUT file)   VENT 11
C              0 for Venus-GRAM evaluation mode (computes atmosphere)   VENT 12
C     jmonte = counter for number of Monte-Carlo cases:                 VENT 13
C              > 0 initializes next Monte-Carlo case                    VENT 14
C              = 0 during trajectory for each Monte-Carlo case          VENT 15
C     istep  = counter for number of trajectory point steps.            VENT 16
C              Trajectory computations end when istep=MAXNUM or EOF=1   VENT 17
C     zhgt   = next height at which Venus-GRAM is evaluated             VENT 18
C     zlat   = next latitude at which Venus-GRAM is evaluated           VENT 19
C     zlon   = next longitude at which Venus-GRAM is evaluated          VENT 20
C              *** NOTE - If zlon is positive West, LonEast should be   VENT 21
C              set to 0 in the NAMELIST input file (read in by the      VENT 22
C              Setup_V05 subroutine) ***                                VENT 23
C     zsec   = next time (seconds), measured from from start            VENT 24
C     pzhgt  = last height at which Venus-GRAM was evaluated            VENT 25
C     pzlat  = last latitude at which Venus-GRAM was evaluated          VENT 26
C     pzlon  = last longitude at which Venus-GRAM was evaluated         VENT 27
C              *** NOTE - If pzlon is positive West, LonEast should be  VENT 28
C              set to 0 in the NAMELIST input file (read in by the      VENT 29
C              Setup_V05 subroutine) ***                                VENT 30
C     pzsec  = previous time (seconds), measured from from start        VENT 31
C     DAY0    = Starting Julian day (from date and time INPUT file or   VENT 32
C               user-provided input).  If provided as user input, DAY0  VENT 33
C               should be consistent with choices used for IERT and     VENT 34
C               IUTC time-basis input parameters                        VENT 35
C     INPUTFL  = name for NAMELIST format input file                    VENT 36
C     iustdout = unit number for standard (screen) output               VENT 37
C     iupdate  = input value >= 0 to update total step for perturbation VENT 38
C                used; <0 for no update of total stp for perturbations  VENT 39
C     Optional high precision ephemeris inputs are:                     VENT 40
C       dsunlat = high precision latitude of sub-solar point (deg)      VENT 41
C       dsunlon = high precision longitude of sub-solar point (deg)     VENT 42
C       dsunLs  = high precision Ls angle (deg)                         VENT 43
C       dradau  = high precision Venus orbital radius (AU)              VENT 44
C       dowlt   = high precision one-way light time (minutes)           VENT 45
C     If high precision ephemeris data are not used, input 0 values to  VENT 46
C     use the regular ephemeris subroutine of Venus-GRAM                VENT 47
C                                                                       VENT 48
C     Output argument list variables, supplied from Venus-GRAM:         VENT 49
C                                                                       VENT 50
C     NMONTE  = number of Monte-Carlo cases (from INPUT file)           VENT 51
C     MAXNUM  = maximum points in trajectory (from INPUT file)          VENT 52
C     TEMP    = atmospheric temperature (K)                             VENT 53
C     PRES    = atmospheric pressure (N/m**2)                           VENT 54
C     DENSLO  = low (approx. -1 sigma) density (kg/m**3)                VENT 55
C     DENS    = average (mean) density (kg/m**3)                        VENT 56
C     DENSHI  = high (approx. +1 sigma) density (kg/m**3)               VENT 57
C     DENSP   = density perturbation (percent of unperturbed mean)      VENT 58
C     RHOd    = random part of density perturbation (in sigma units)    VENT 59
C     RHOu    = random part of EW wind perturbation (in sigma units)    VENT 60
C     RHOv    = random part of NS wind perturbation (in sigma units)    VENT 61
C     EWWIND  = mean Eastward wind component (m/s)                      VENT 62
C     EWpert  = perturbed part of Eastward wind (m/s)                   VENT 63
C     NSWIND  = mean Northward wind component (m/s)                     VENT 64
C     NSpert  = perturbed part of Northward wind component (m/s)        VENT 65
C     EOF     = Parameter to end trajectory calculation (if EOF = 1)    VENT 66
C     NR1     = Starting random number seed (from INPUT file)           VENT 67
C     DENSTOT = Total (mean plus perturbed) density (kg/m**3)           VENT 68
C     Hrho    = Density scale height (km)                               VENT 69
C     corlim  = ratio of step size to smallest step size for assured    VENT 70
C               accuracy in perturbation model (should be > 1)          VENT 71
C     fmolCO2 = molar (or volume) percentage Carbon Dioxide             VENT 72
C     fmolN2  = molar (or volume) percentage Nitrogen                   VENT 73
C     fmolO   = molar (or volume) percentage Atomic Oxygen              VENT 74
C     fmolCO  = molar (or volume) percentage Carbon Monoxide            VENT 75
C     fmolHe  = molar (or volume) percentage Helium                     VENT 76
C     fmolN   = molar (or volume) percentage Atomic Nitrogen            VENT 77
C     fmolH   = molar (or volume) percentage Atomic Hydrogen            VENT 78
C     AMz     = molecular weight (kg/k-mole)                            VENT 79
C     iupdate = output value = 1 if perturbations updated, 0 if not     VENT 80
C               updated but step distance updated, -1 if neither        VENT 81
C               perturbations nor total step distance updated           VENT 82
C     pertstep=accumulating step distance, relative to accuracy limit   VENT 83
C     ALS     = Planeto-centric longitude of Sun (Ls, degrees)          VENT 84
C     SZA     = Solar zenith angle (degrees)                            VENT 85
C     owlt    = Venus-Earth one-way light time (minutes)                VENT 86
C     sunlat  = Sub-Solar latitude (degrees)                            VENT 87
C     sunlon  = Sub-Solar longitude (degrees East)                      VENT 88
C     VenusAU = Venus orbital radius (AU)                               VENT 89
C     TLOCAL  = Local Solar time (Venus hours)                          VENT 90
C                                                                       VENT 91
C     Other variables from the argument list of subroutine Datastep_V05 VENT 92
C     can be passed to your trajectory program by adding them to the    VENT 93
C     argument list of the Venustraj_V05 subroutine.  See comments      VENT 94
C     below describing argument list variables of Datastep_V05 routine. VENT 95
C                                                                       VENT 96
C...................................................................    VENT 97
      Implicit None                                                     VENT 98
      Double Precision zhgt,zlat,zlon,pzhgt,pzlat,pzlon,TEMP,PRES,      VENT 99
     & DENSLO,DENS,DENSHI,DENSP,CHGT,CLAT,CLON,DELHGT,DELLAT,DELLON,    VENT100
     & RHOd,RHOu,RHOv,EWWIND,EWpert,NSWIND,NSpert,DENSTOT,Hrho,fmolCO2, VENT101
     & corlim,Hpres,rpscale,fmolN2,fmolO,fmolCO,fmolHe,fmolN,fmolH,AMz, VENT102
     & pertstep,corlmin,CSEC,pzsec,DELTIME,dsunlat,dsunlon,dsunLs,      VENT103
     & dradau,dowlt,zsec,DAY0,ALS,SZA,owlt,sunlat,sunlon,VenusAU,       VENT104
     & TLOCAL,profnear,proffar                                          VENT105
      Character*60 INPUTFL                                              VENT106
      Integer EOF,isetup,jmonte,NMONTE,istep,MAXNUM,iustdout,maxfiles,  VENT107
     &  iulist,nvarx,nvary,iu0,iup,loneast,logscale,npos,NR1,iupdate,   VENT108
     &  IERT,IUTC,nprof                                                 VENT109
      COMMON /DATACOM/rpscale,NPOS,NVARX,NVARY,logscale,iu0,iup,        VENT110
     & maxfiles                                                         VENT111
      If (isetup.eq.1)Then                                              VENT112
C...    Initialize data and read input file with Setup_V05 subroutine   VENT113
C       See Venus-GRAM code for description of NAMELIST format input    VENT114
C       file                                                            VENT115
        Call Setup_V05(CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,DELHGT,  VENT116
     &   DELLAT,DELLON,DELTIME,MAXNUM,NR1,NMONTE,LonEast,INPUTFL,       VENT117
     &   iustdout,iulist,IERT,IUTC,corlmin,profnear,proffar,nprof)      VENT118
        pertstep = 0.0                                                  VENT119
        If (NMONTE.lt.1)NMONTE = 1                                      VENT120
C...    Make sure not to read trajectory from input TRAJDATA file       VENT121
        If (MAXNUM.eq.99999)Stop ' Set NPOS > 0 in INPUT file'          VENT122
        Return                                                          VENT123
      Endif                                                             VENT124
C...  Store position                                                    VENT125
      CSEC = pzsec                                                      VENT126
      CHGT = pzhgt                                                      VENT127
      CLAT = pzlat                                                      VENT128
      CLON = pzlon                                                      VENT129
C...  Re-initialize random seed for each Monte Carlo run                VENT130
      If(jmonte.gt.1)Then                                               VENT131
        Call Randinit_V05(jmonte,NR1,RHOd,RHOu,RHOv,iup,iustdout)       VENT132
        pertstep = 0.0                                                  VENT133
      Endif                                                             VENT134
C...  Store increments of position                                      VENT135
      If(istep.gt.0)Then                                                VENT136
        DELTIME = zsec - pzsec                                          VENT137
        DELHGT = zhgt - pzhgt                                           VENT138
        DELLAT = zlat - pzlat                                           VENT139
        DELLON = zlon - pzlon                                           VENT140
      Endif                                                             VENT141
C...  Convert CLON and DELLON to West longitude if LonEast = 0          VENT142
C     *** NOTE - If trajectory-calculated longitude zlon is positive    VENT143
C     West, then LonEast should be set to 0 in the NAMELIST input file  VENT144
C     (read in by the Setup_V05 subroutine) ***                         VENT145
      If (LonEast.eq.0)Then                                             VENT146
        CLON = 360.0d0 - CLON                                           VENT147
        DELLON = -DELLON                                                VENT148
      Endif                                                             VENT149
      If (DAbs(DELLON).gt.180.0d0)DELLON=DELLON-DSign(360.0d0,DELLON)   VENT150
C...  Call Venus-GRAM routine to evaluate atmospheric parameters        VENT151
      Call Datastep_V05(istep,CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,  VENT152
     & EOF,DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,DENS,DENSHI,   VENT153
     & DENSP,EWWIND,EWpert,NSWIND,NSpert,Hrho,Hpres,dsunlat,dsunlon,    VENT154
     & dsunLs,dradau,dowlt,LonEast,corlim,DENSTOT,IERT,IUTC,fmolCO2,    VENT155
     & fmolN2,fmolO,fmolCO,fmolHe,fmolN,fmolH,AMz,pertstep,corlmin,     VENT156
     & iupdate,ALS,SZA,owlt,sunlat,sunlon,VenusAU,TLOCAL,profnear,      VENT157
     & proffar,nprof)                                                   VENT158
C.....................................................................  VENT159
C                                                                       VENT160
C....   Relevant parameters passed as output from Datastep_V05 are:     VENT161
C       TEMP = temperature (K)                                          VENT162
C       PRES = average (mean) pressure (N/m**2)                         VENT163
C       DENSLO = nominal low density (kg/m**3), approx. -1 sigma        VENT164
C       DENS   = average (mean) density (kg/m**3)                       VENT165
C       DENSHI = nominal high density (kg/m**3), approx. +1 sigma       VENT166
C       DENSP  = density perturbation (% of unperturbed mean)           VENT167
C       EWWIND = mean eastward wind component (m/s)                     VENT168
C       EWpert = perturbation in eastward wind component (m/s)          VENT169
C       NSWIND = mean northward wind component (m/s)                    VENT170
C       NSpert = perturbation in northward wind component (m/s)         VENT171
C       Hrho   = density scale height (km)                              VENT172
C       Hpres  = pressure scale height (km)                             VENT173
C       corlim = ratio of step size to minimum step size for assured    VENT174
C                accuracy in perturbations (should be >= 1)             VENT175
C       DENSTOT= total density (mean plus perturbed), kg/m**3           VENT176
C       fmolCO2= molar (or volume) percentage Carbon Dioxide            VENT177
C       fmolN2 = molar (or volume) percentage Nitrogen                  VENT178
C       fmolO  = molar (or volume) percentage Atomic Oxygen             VENT179
C       fmolCO = molar (or volume) percentage Carbon Monoxide           VENT180
C       fmolHe = molar (or volume) percentage Helium                    VENT181
C       fmolN  = molar (or volume) percentage Atomic Nitrogen           VENT182
C       fmolH  = molar (or volume) percentage Atomic Hydrogen           VENT183
C       AMz    = Molecular weight (kg/k-mole)                           VENT184
C       iupdate= 1 if perturbations updated, 0 if perturbations not     VENT185
C                 updated but step distance updated, -1 if neither      VENT186
C                 perturbations nor total step distance updated         VENT187
C                                                                       VENT188
C.....................................................................  VENT189
C                                                                       VENT190
C     Make sure density doesn't go negative (e.g. from LOGSCALE = 1     VENT191
C     or 2 on INPUT)                                                    VENT192
      If (DENSTOT.le.0.)Stop ' Negative density!  Set LOGSCALE = 0'     VENT193
      If (LOGSCALE.ne.0)Stop ' LOGSCALE .ne. 0 on INPUT file'           VENT194
      Return                                                            VENT195
      END                                                               VENT196
C---------------------------------------------------------------------- VENT197
