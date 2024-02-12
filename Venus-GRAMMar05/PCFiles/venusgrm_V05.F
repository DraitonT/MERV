C     Venus Global Reference Atmospheric Model 2005                     VGRM  1
C                 (Version 1) - March, 2005                             VGRM  2
C                                                                       VGRM  3
C     A program to evaluate density, temperature, pressure and wind     VGRM  4
C     components at any given time and position in the atmosphere       VGRM  5
C     of Venus                                                          VGRM  6
C                                                                       VGRM  7
C     Dependence of mean atmospheric parameters on height, latitude,    VGRM  8
C     and time-of-day (or solar zenith angle) based on Kliore, A. J.,   VGRM  9
C     V. I. Moroz, and G. M. Keating (1986), "The Venus International   VGRM 10
C     Reference Atmosphere", Advances in Space Research, vol. 5, no.    VGRM 11
C     11, 1985, pages 1-304, Pergamon Press, Oxford, referred to here   VGRM 12
C     as VIRA.                                                          VGRM 13
C                                                                       VGRM 14
C     Venus-GRAM is not a US ITAR item.  Export of Venus-GRAM is        VGRM 15
C     allowed under Export Control Classification EAR-99.               VGRM 16
C     However, no recipient of this code should forward copies outside  VGRM 17
C     the United States without explicit approval by NASA Marshall      VGRM 18
C     Space Flight Center.                                              VGRM 19
C                                                                       VGRM 20
C.....................................................................  VGRM 21
C                                                                       VGRM 22
      Implicit None                                                     VGRM 23
      Double Precision FHGT,CHGT,FLAT,CLAT,FLON,CLON,FDHGT,fmolO,fmolN, VGRM 24
     &  DELHGT,FDLAT,DELLAT,FDLON,DELLON,RHOd,RHOu,RHOv,fMolCO,         VGRM 25
     &  PRES,DENSLO,DENSAV,DENSHI,DENSP,EWWIND,EWpert,NSWIND,TEMP,      VGRM 26
     &  NSpert,Hrho,Hpres,corlim,DENSTOT,fmolH,fmolHe,fmolCO2,AMz,      VGRM 27
     &  pertstep,corlmin,fmolN2,DAY0,CSEC,FSEC,DELTIME,FDTIME,ALS,SZA,  VGRM 28
     &  owlt,sunlat,sunlon,VenusAU,TLOCAL,profnear,proffar              VGRM 29
      Integer EOF,MAXNUM,NR1,NMONTE,LonEast,iustdout,iulist,I,          VGRM 30
     &  J,iustdin,iupdate,IERT,IUTC,nprof                               VGRM 31
      Character*60 INPUTFL                                              VGRM 32
C...  Set unit numbers for standard (screen) input and output           VGRM 33
      iustdout = 6                                                      VGRM 34
      iustdin = 5                                                       VGRM 35
C...  Read file name for NAMELIST format input file                     VGRM 36
      Write(iustdout,*)' Enter file name for NAMELIST input'            VGRM 37
      Read(iustdin,10)INPUTFL                                           VGRM 38
 10   Format(A)                                                         VGRM 39
C                                                                       VGRM 40
C.....................................................................  VGRM 41
C     Input is read by Setup_V05 routine in NAMELIST form.  Example:    VGRM 42
C                                                                       VGRM 43
C $INPUT                                                                VGRM 44
C  LSTFL     = 'LIST.txt'            ! List file name (CON for          VGRM 45
C                                    !   console listing)               VGRM 46
C  OUTFL     = 'OUTPUT.txt'          ! Output file name                 VGRM 47
C  TRAJFL    = 'TRAJDATA.txt'        ! (Optional) Trajectory input      VGRM 48
C                                    !   file name                      VGRM 49
C  profile  = 'null'                 ! (Optional) auxiliary profile     VGRM 50
C                                    !   input file name                VGRM 51
C  DATADIR   = 'C:\Venus\Data\'      ! Directory for VIRA input data    VGRM 52
C  IERT      = 1       ! 1 for time input as Earth-receive time (ERT),  VGRM 53
C                      !   or 0 Venus-event time (VET)                  VGRM 54
C  IUTC      = 1       ! 1 for time input as Coordinated Universal Time VGRM 55
C                      !   (UTC), or 0 for Terrestrial (Dynamical) Time VGRM 56
C                      !   (TT)                                         VGRM 57
C  MONTH     = 11      ! month of year                                  VGRM 58
C  MDAY      = 12      ! day of month                                   VGRM 59
C  MYEAR     = 1980    ! year (4-digit, or 1970-2069 can be 2-digit)    VGRM 60
C  IHR       = 0       ! Hour of day (ERT or VET, controlled by IERT    VGRM 61
C                      !   and UTC or TT, controlled by IUTC)           VGRM 62
C  IMIN      = 0       ! minute of hour (meaning controlled by IERT and VGRM 63
C                      !   IUTC)                                        VGRM 64
C  SEC       = 0.0     ! seconds of minute (meaning controlled by IERT  VGRM 65
C                      !   and IUTC).  IHR:IMIN:SEC is time for initial VGRM 66
C                      ! position to be evaluated                       VGRM 67
C  NPOS      = 21      ! max # positions to evaluate (0 = read data     VGRM 68
C                      !                   from trajectory input file)  VGRM 69
C  LonEast   = 0       ! 1 for East longitudes positive (default);      VGRM 70
C                      !   0 for West longitudes positive;              VGRM 71
C  NR1       = 1001    ! starting random number (0 < NR1 < 30000)       VGRM 72
C  NVARX     = 1       ! x-code for plotable output (1=height above     VGRM 73
C                      !   reference ellipsoid). See file xycodes.txt   VGRM 74
C  NVARY     = 0       ! y-code for 2-D plotable output (0 for 1-D      VGRM 75
C                      !                                       plots)   VGRM 76
C  LOGSCALE  = 0       ! 0=regular SI units, 1=log-base-10 scale,       VGRM 77
C                      !    2=percentage deviations from Mean model,    VGRM 78
C                      !    3=SI units with density in kg/km**3         VGRM 79
C  FLAT      = 22.0    ! initial latitude (N positive), degrees         VGRM 80
C  FLON      = 48.0    ! initial longitude (West positive if LonEast=0  VGRM 81
C                      !   or East positive if LonEast = 1), degrees    VGRM 82
C  FHGT      = 0.0     ! initial height above reference ellipsoid (km)  VGRM 83
C  DELHGT    = 10.0    ! height increment (km) between steps            VGRM 84
C  DELLAT    = 0.0     ! latitude increment (deg) between steps         VGRM 85
C  DELLON    = 0.0     ! longitude increment (deg) between steps (West  VGRM 86
C                      !   positive if LonEast = 0, East positive if    VGRM 87
C                      !   LonEast = 1)                                 VGRM 88
C  DELTIME   = 0.0     ! time increment (seconds) between steps. Time   VGRM 89
C                      !   increment is in ERT or VET, as controlled by VGRM 90
C                      !   input parameter IERT, and UTC or TT, as      VGRM 91
C                      !   controlled by input parameter IUTC           VGRM 92
C  profnear = 0.0      ! Lat-lon radius within which weight for         VGRM 93
C                      !   auxiliary profile is 1.0                     VGRM 94
C  proffar  = 0.0      ! Lat-lon radius beyond which weight for         VGRM 95
C                      !   auxiliary profile is 0.0                     VGRM 96
C  rpscale   = 1.0     ! random perturbation scale factor (0-2)         VGRM 97
C  NMONTE    = 1       ! number of Monte Carlo runs                     VGRM 98
C  iup       = 11      ! 0 for no LIST and graphics output, unit number VGRM 99
C                      !  for LIST file otherwise                       VGRM100
C  corlmin   = 0.0     ! Minimum relative step size for perturbations   VGRM101
C                      !  (0.0 - 1.0)                                   VGRM102
C $END                                                                  VGRM103
C.....................................................................  VGRM104
C                                                                       VGRM105
C...  Set up information for start of run                               VGRM106
      Call Setup_V05(CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,DELHGT,    VGRM107
     & DELLAT,DELLON,DELTIME,MAXNUM,NR1,NMONTE,LonEast,INPUTFL,         VGRM108
     & iustdout,iulist,IERT,IUTC,corlmin,profnear,proffar,nprof)        VGRM109
C                                                                       VGRM110
C...  Save initial position and position displacement values            VGRM111
      FHGT = CHGT                                                       VGRM112
      FLAT = CLAT                                                       VGRM113
      FLON = CLON                                                       VGRM114
      FSEC = CSEC                                                       VGRM115
      FDHGT = DELHGT                                                    VGRM116
      FDLAT = DELLAT                                                    VGRM117
      FDLON = DELLON                                                    VGRM118
      FDTIME = DELTIME                                                  VGRM119
C...  Initialize total perturbation step                                VGRM120
      pertstep = 0.0                                                    VGRM121
      iupdate = 0                                                       VGRM122
C...  Step through number of Monte Carlo runs                           VGRM123
      Do 910 J = 1,NMONTE                                               VGRM124
C...  Re-initialize random number, position and time for each run       VGRM125
        If (J.gt.1)Then                                                 VGRM126
          Call Randinit_V05(J,NR1,RHOd,RHOu,RHOv,iulist,iustdout)       VGRM127
          CHGT = FHGT                                                   VGRM128
          CLAT = FLAT                                                   VGRM129
          CLON = FLON                                                   VGRM130
          CSEC = FSEC                                                   VGRM131
          DELHGT = FDHGT                                                VGRM132
          DELLAT = FDLAT                                                VGRM133
          DELLON = FDLON                                                VGRM134
          DELTIME = FDTIME                                              VGRM135
C...    Re-initialize total perturbation step                           VGRM136
        pertstep = 0.0                                                  VGRM137
        iupdate = 0                                                     VGRM138
        Endif                                                           VGRM139
C...  Step through max Number of points for each Monte Carlo run        VGRM140
      DO 900 I = 0,MAXNUM                                               VGRM141
C                                                                       VGRM142
        Call Datastep_V05(I,CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,    VGRM143
     &   EOF,DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,DENSAV,      VGRM144
     &   DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,Hrho,Hpres,0.0d0,     VGRM145
     &   0.0d0,0.0d0,0.0d0,0.0d0,LonEast,corlim,DENSTOT,IERT,IUTC,      VGRM146
     &   fmolCO2,fmolN2,fmolO,fmolCO,fmolHe,fmolN,fmolH,AMz,pertstep,   VGRM147
     &   corlmin,iupdate,ALS,SZA,owlt,sunlat,sunlon,VenusAU,TLOCAL,     VGRM148
     &   profnear,proffar,nprof)                                        VGRM149
C                                                                       VGRM150
C.....................................................................  VGRM151
C                                                                       VGRM152
C....   Parameters passed as output from Datastep_V05 are:              VGRM153
C       TEMP   = temperature (K)                                        VGRM154
C       PRES   = pressure (N/m**2)                                      VGRM155
C       DENSLO = nominal low density (kg/m**3), approx. -1 sigma        VGRM156
C       DENSAV = mean density (kg/m**3)                                 VGRM157
C       DENSHI = nominal high density (kg/m**3), approx. +1 sigma       VGRM158
C       DENSP  = density perturbation (% of unperturbed mean)           VGRM159
C       EWWIND = mean eastward wind component (m/s)                     VGRM160
C       EWpert = eastward wind component perturbation (m/s)             VGRM161
C       NSWIND = mean northward wind component (m/s)                    VGRM162
C       NSpert = northward wind component perturbation (m/s)            VGRM163
C       Hrho   = density scale height (km)                              VGRM164
C       Hpres  = pressure scale height (km)                             VGRM165
C       corlim = ratio of step size to minimum step size for assured    VGRM166
C                accuracy in perturbations (should be >= 1)             VGRM167
C       DENSTOT= total density (mean plus perturbed), kg/m**3           VGRM168
C       fmolCO2= molar (or volume) concentration (%) of Carbon Dioxide  VGRM169
C       fmolN2 = molar (or volume) concentration (%) of Nitrogen        VGRM170
C       fmolO  = molar (or volume) concentration (%) of atomic Oxygen   VGRM171
C       fmolCO = molar (or volume) concentration (%) of Carbon Monoxide VGRM172
C       fmolHe = molar (or volume) concentration (%) of Helium          VGRM173
C       fmolN  = molar (or volume) concentration (%) of atomic Nitrogen VGRM174
C       fmolH  = molar (or volume) concentration (%) of atomic Hydrogen VGRM175
C       AMz    = Molecular weight (kg/k-mole)                           VGRM176
C       iupdate= 1 if perturbations updated, 0 if perturbations not     VGRM177
C                updated but perturbation step updated, -1 if           VGRM178
C                neither perturbations nor step updated                 VGRM179
C       ALS    = Planeto-centric longitude of Sun (Ls, degrees)         VGRM180
C       SZA    = Solar zenith angle (degrees)                           VGRM181
C       owlt   = Venus-Earth one-way light time (minutes)               VGRM182
C       sunlat = Sub-Solar latitude (degrees)                           VGRM183
C       sunlon = Sub-Solar longitude (degrees East)                     VGRM184
C       VenusAU= Venus orbital radius (AU)                              VGRM185
C       TLOCAL = Local Solar time (Venus hours)                         VGRM186
C                                                                       VGRM187
C       In addition to being passed to other routines, these            VGRM188
C       parameters may also be written out here.                        VGRM189
C                                                                       VGRM190
C       Optional high resolution ephemeris inputs are:                  VGRM191
C         dsunlat = latitude of sub-solar point (deg)                   VGRM192
C         dsunlon = longitude of sub-solar point (deg)                  VGRM193
C         dsunLs  = solar Ls angle (deg)                                VGRM194
C         dradau  = Venus orbital radius (AU)                           VGRM195
C         dowlt   = Earth-Venus one-way light-time (minutes)            VGRM196
C       Values of 0.0D0, used here, cause use of the internal ephemeris VGRM197
C       subroutine to compute these parameters                          VGRM198
C.....................................................................  VGRM199
C                                                                       VGRM200
C       Go to next Monte Carlo run if EOF=1                             VGRM201
        If (EOF .eq. 1)Goto 910                                         VGRM202
  900 Continue                                                          VGRM203
  910 Continue                                                          VGRM204
      STOP ' Normal Termination'                                        VGRM205
      END                                                               VGRM206
C---------------------------------------------------------------------- VGRM207
