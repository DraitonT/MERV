      Subroutine Setup_V05(CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,     SETU  1
     & DHGT,DLAT,DLON,DTIME,MAXNUM,NRN1,NMCR1,LnEast,INPUTFL,iustdout,  SETU  2
     & iulist,InERT,InUTC,stepmin,profnr,proffr,nprof)                  SETU  3
C                                                                       SETU  4
    1 Format(' Venus-GRAM 2005 (Version ',A1,') - March, 2005')         SETU  5
C                                                                       SETU  6
      Implicit None                                                     SETU  7
      Character*1 version,EWlon                                         SETU  8
      Parameter (version = '1')                                         SETU  9
      Character*60 DATADIR,profile                                      SETU 10
      Character*60 TRAJFL,INPUTFL                                       SETU 11
      Double Precision Flat,Flon,FHgt,DelHgt,Dellat,Dellon,CHGT,        SETU 12
     & CLAT,CLON,DHGT,DLAT,DLON,RHOd,RHOu,RHOv,rpfactor,Z1,PPND_V05,    SETU 13
     & rpscale,corlmin,stepmin,CSEC,DAY0,DAY,DTIME,Sec,Deltime,         SETU 14
     &  zlo,plo,dlo,tlo,zetalo,Rlo,zmd,pmd,dmd,tmd,yco2md,yn2md,yomd,   SETU 15
     &  ycomd,yhemd,ynmd,zhi,phi,dhi,thi,yco2hi,yn2hi,yohi,RANDOM_V05,  SETU 16
     &  ycohi,yhehi,ynhi,yhhi,profnr,proffr,profnear,proffar            SETU 17
      Integer MAXNUM,NRN1,NMCR1,LnEast,iustdout,iulist,J,IERR,IERR1,    SETU 18
     & IERR2,IERR3,IERR4,IERR5,IERR6,IERR7,IX,IY,IZ,NPOS,NVARX,NVARY,   SETU 19
     & logscale,iu0,iup,maxfiles,nmonte,L,loneast,NR1,ioerr,IERT,       SETU 20
     & IUTC,Month,Mday,Myear,Ihr,Imin,InERT,InUTC,idterr,nprof          SETU 21
      Dimension IERR(6)                                                 SETU 22
C                                                                       SETU 23
      Character*60 lstfl,files(6),outfl                                 SETU 24
      Character*7 HgtLbl                                                SETU 25
      Character*8 DenLbl                                                SETU 26
C                                                                       SETU 27
      COMMON /RANDCOM_V05/IX,IY,IZ                                      SETU 28
C...  Venus model VIRA data at low ,middle, and high altitudes          SETU 29
      Common /VIRAdata_V05/zlo(81),plo(81,5),dlo(81,5),tlo(81,5),       SETU 30
     &  zetalo(81,5),Rlo(81,5),zmd(11),pmd(11,2),dmd(11,2),tmd(11,2),   SETU 31
     &  yco2md(11,2),yn2md(11,2),yomd(11,2),ycomd(11,2),yhemd(11,2),    SETU 32
     &  ynmd(11,2),zhi(21),phi(21,7),dhi(21,7),                         SETU 33
     &  thi(21,7),yco2hi(21,7),yn2hi(21,7),yohi(21,7),ycohi(21,7),      SETU 34
     &  yhehi(21,7),ynhi(21,7),yhhi(21,7)                               SETU 35
      COMMON /DATACOM_V05/rpfactor,NPOS,NVARX,NVARY,logscale,iu0,iup,   SETU 36
     & maxfiles                                                         SETU 37
C...................................................................... SETU 38
C                                                                       SETU 39
      COMMON /FILENAME_V05/lstfl,outfl                                  SETU 40
      EQUIVALENCE (IERR1,IERR(1)),(IERR2,IERR(2)),(IERR3,IERR(3)),      SETU 41
     & (IERR4,IERR(4)),(IERR5,IERR(5)),(IERR6,IERR(6))                  SETU 42
C                                                                       SETU 43
      DATA FILES/'LIST.txt','Density.txt','Perturb.txt','Winds.txt',    SETU 44
     & 'TPresHgt.txt','OUTPUT.txt'/                                     SETU 45
C...................................................................... SETU 46
C...  Establish default values for input parameters                     SETU 47
      Data TRAJFL/'TRAJDATA.txt'/                                       SETU 48
      Data DATADIR,profile/'null','null'/                               SETU 49
C...  Default time = Pioneer probe encounter date                       SETU 50
      Data Month,Mday,Myear,Ihr,Imin,Sec/12,9,1978,19,47,59.0d0/        SETU 51
C...  Default position                                                  SETU 52
      Data Flat,Flon,Fhgt,Delhgt,Dellat,Dellon,Deltime/-31.3d0,317.0d0, SETU 53
     &  0.0d0,1.0d0,0.3d0,0.5d0,500.0d0/                                SETU 54
C...  Default random perturbation scale factor                          SETU 55
      Data rpscale/1.0d0/                                               SETU 56
C...................................................................... SETU 57
C     Definition of the Namelist input data                             SETU 58
      Namelist /INPUT/LSTFL,OUTFL,TRAJFL,profile,DATADIR,IERT,IUTC,     SETU 59
     & Month,Mday,Myear,Ihr,Imin,Sec,NPOS,LonEast,NR1,NVARX,NVARY,      SETU 60
     & LOGSCALE,FLAT,FLON,FHGT,DELHGT,DELLAT,DELLON,DELTIME,profnear,   SETU 61
     & proffar,rpscale,NMONTE,iup,corlmin                               SETU 62
C.....................................................................  SETU 63
C                                                                       SETU 64
C...  Default corlmin value                                             SETU 65
      corlmin = 0.0d0                                                   SETU 66
C...  Default profile parameters                                        SETU 67
      profnear = 0.0d0                                                  SETU 68
      proffar = 0.0d0                                                   SETU 69
C...  Default time options IERT=1 for Earth-receive time and IUTC=1     SETU 70
C     for UTC time (not Terrestrial Dynamical Time)                     SETU 71
      IERT = 1                                                          SETU 72
      IUTC = 1                                                          SETU 73
C...  Set unit number for screen I/O and pass it into Common            SETU 74
      iu0 = iustdout                                                    SETU 75
C...  default list and output files                                     SETU 76
      lstfl = 'LIST.txt'                                                SETU 77
      outfl = 'OUTPUT.txt'                                              SETU 78
C...  Default number of positions                                       SETU 79
      NPOS = 251                                                        SETU 80
C...  Default use East Longitude positive (Convention for retrograde)   SETU 81
      LonEast = 1                                                       SETU 82
C...  Default plot variable = height above reference ellipsoid          SETU 83
      NVARX = 1                                                         SETU 84
      NVARY = 0                                                         SETU 85
C...  Default to regular linear scale                                   SETU 86
      LOGSCALE = 0                                                      SETU 87
C...  Default random number seed and Number of Monte Carlo runs         SETU 88
      NR1 = 1234                                                        SETU 89
      NMONTE = 1                                                        SETU 90
C...  Default unit number for print output data file                    SETU 91
      iup = 13                                                          SETU 92
C...  Open Namelist data file                                           SETU 93
      OPEN(8,file=INPUTFL,status='old',iostat=ioerr)                    SETU 94
      If (ioerr.ne.0)Then                                               SETU 95
        Write(iustdout,*)' Error opening NAMELIST input file'           SETU 96
        Stop                                                            SETU 97
      Endif                                                             SETU 98
C...  Read Namelist data                                                SETU 99
      Read(8,INPUT)                                                     SETU100
      Close(8)                                                          SETU101
C.....................................................................  SETU102
C     For compilers not supporting the NAMELIST input mode, the         SETU103
C     previous Read statement may be replaced by:                       SETU104
C                                                                       SETU105
C     Read(8,10)LSTFL                                                   SETU106
C     Read(8,10)OUTFL                                                   SETU107
C     Read(8,10)TRAJFL                                                  SETU108
C     Read(8,10)DATADIR                                                 SETU109
C     Read(*,10)profile                                                 SETU110
C 10  Format(A)                                                         SETU111
C     Read(8,*)IERT,IUTC,Month,Mday,Myear,Ihr,Imin,Sec,NPOS,LonEast,    SETU112
C    & NR1,NVARX,NVARY,LOGSCALE,FLAT,FLON,FHGT,DELHGT,DELLAT,           SETU113
C    & DELLON,DELTIME,profnear,proffar,rpscale,NMONTE,iup,corlmin       SETU114
C                                                                       SETU115
C     and the NAMELIST file INPUT may be modified to contain free-      SETU116
C     field input data as in the above list.                            SETU117
C.....................................................................  SETU118
C...  Check that unit iup is in allowable range                         SETU119
      If ((iup.ge.5.and.iup.le.12).or.(iup.ge.21.and.iup.le.28))        SETU120
     &   Stop ' Unit iup conflict with another file'                    SETU121
      If (FLAT .lt. -90.0d0 .or. FLAT .gt. 90.0d0)then                  SETU122
        Write(iu0,382)                                                  SETU123
  382   Format(' Error in first latitude or longitude.')                SETU124
        Goto 9998                                                       SETU125
      Endif                                                             SETU126
      If (FLON .lt. 0.0d0)FLON = FLON + 360.0d0                         SETU127
      If (FLON .lt. 0.0d0 .or. FLON .gt. 360.0d0)then                   SETU128
        Write(iu0,382)                                                  SETU129
        Goto 9998                                                       SETU130
      Endif                                                             SETU131
C...  Store values for output arguments                                 SETU132
      profnr = profnear                                                 SETU133
      proffr = proffar                                                  SETU134
      LnEast = LonEast                                                  SETU135
C...  Test profnear and proffar.  Read profile data if profnear > 0     SETU136
      If (profnear.gt.0.0d0)Then                                        SETU137
        If(proffar.le.profnear)Stop ' proffar must be > profnear'       SETU138
        Call RdProf_V05(profile,nprof,LonEast)                          SETU139
      Endif                                                             SETU140
      iulist = iup                                                      SETU141
C...  Convert FLON, DELLON to East if LonEast=0                         SETU142
      If (LonEast.eq.0)Then                                             SETU143
        FLON = 360.0d0 - FLON                                           SETU144
        DELLON = -DELLON                                                SETU145
      Endif                                                             SETU146
C...  Insure corlmin input value within proper bounds                   SETU147
      If (corlmin.lt.0.0d0)corlmin=0.0d0                                SETU148
      If (corlmin.gt.1.0d0)corlmin=1.0d0                                SETU149
      rpfactor = rpscale                                                SETU150
      If (rpscale.lt.0.0d0.or.rpscale.gt.2.0d0)                         SETU151
     &  Stop ' Must have 0 <= rpscale <= 2'                             SETU152
      If (NMONTE.LT.1)NMONTE = 1                                        SETU153
C...  Pass corlmin value to output                                      SETU154
      stepmin = corlmin                                                 SETU155
C...  Pass 1st random number and Number Monte Carlo runs to output      SETU156
      NRN1 = NR1                                                        SETU157
      NMCR1 = NMONTE                                                    SETU158
C...  Pass option values to output                                      SETU159
      InERT = IERT                                                      SETU160
      InUTC = IUTC                                                      SETU161
C                                                                       SETU162
      DHGT = DELHGT                                                     SETU163
      DLAT = DELLAT                                                     SETU164
      DLON = DELLON                                                     SETU165
      DTIME = DELTIME                                                   SETU166
C                                                                       SETU167
      files(1) = lstfl                                                  SETU168
      files(6) = outfl                                                  SETU169
      If (lstfl .ne. 'CON' .and. lstfl .ne. 'con'.and.iup.gt.0)Then     SETU170
        OPEN(iup,file=lstfl,iostat=ierr1)                               SETU171
      Endif                                                             SETU172
C...  Write version number and file names to standard output            SETU173
      Write(iu0,1)version                                               SETU174
      If(iup.gt.0)Then                                                  SETU175
        Write(iu0,14)LSTFL,OUTFL                                        SETU176
      Else                                                              SETU177
        Write(iu0,14)' null ',' null '                                  SETU178
      Endif                                                             SETU179
      Write(iu0,15)DATADIR                                              SETU180
      If (npos.eq.0)Write(iu0,16)TRAJFL                                 SETU181
      IF(NPOS.gt.0) Goto 12                                             SETU182
C...  If NPOS = 0 is entered, program reads position data from          SETU183
C      unit 7, trajectory data file                                     SETU184
C...  Each trajectory file record contains height (km), latitude        SETU185
C      (degrees, North positive), and longitude (degrees, West positive SETU186
C      if LonEast=0 or East positive if LonEast=1).                     SETU187
      Open(7,file=TRAJFL,status='old',iostat=ierr7)                     SETU188
      If(ierr7.ne.0)then                                                SETU189
        Write(iu0,11)                                                   SETU190
   11   Format(' Unable to open Trajectory Data file!')                 SETU191
        Goto 9998                                                       SETU192
      Endif                                                             SETU193
   12 MAXNUM = NPOS - 1                                                 SETU194
      If (profnear.gt.0.0d0)Write(iu0,13)profile                        SETU195
      IF(NPOS.LE.0)MAXNUM = 99999                                       SETU196
C...  Write version number and file names to LIST file                  SETU197
      If (iup.gt.0)Then                                                 SETU198
        Write(iup,1)version                                             SETU199
        Write(iup,14)LSTFL,OUTFL                                        SETU200
        Write(iup,15)DATADIR                                            SETU201
        If (npos.eq.0)Write(iup,16)TRAJFL                               SETU202
        If (profnear.gt.0.0d0)Write(iup,13)profile                      SETU203
  13  Format(' Profile file= ',(A))                                     SETU204
      Endif                                                             SETU205
  14  Format(' LIST file= ',(A)/'    OUTPUT file= ',(A))                SETU206
  15  Format(' Data directory= ',A60)                                   SETU207
  16  Format(' Position input from trajectory file= ',(A))              SETU208
C...  Files on units 21-27 contain parameters suitable for plotting.    SETU209
C...  Data are in either of two forms: (1)  X  Y1 Y2 ..., where X       SETU210
C...  is the variable to be plotted against (e.g. height), and Y1, Y2,  SETU211
C...  etc. are variables to be plotted, or (2) X Y Z1 Z2 ..., where X   SETU212
C...  and Y are two variables (e.g. latitude and height) to provide     SETU213
C...  position for plotting contour plots of one of the variables       SETU214
C...  Z1, Z2, etc.                                                      SETU215
      If (iup.gt.0)Then                                                 SETU216
C...    Unit 21 file = 'Density.txt': Headers for variables are -       SETU217
C        DENSLO   = low (average -1 sigma) density                      SETU218
C        DENSAV   = average density (based on VIRA data)                SETU219
C        DENSHI   = high (average +1 sigma) density                     SETU220
C        DENSTOT  = total (average+perturbed) density (units = kg/m**3, SETU221
C                   log-10 scale, % from VIRA avg., or kg/km**3,        SETU222
C                   depending on value of LOGSCALE input parameter)     SETU223
C        Radius   = Radial distance (km), planetary center of mass to   SETU224
C                   spacecraft position (planet radius plus height)     SETU225
C        Rref     = local planetary radius (km)                         SETU226
C        Grav     = local acceleration of gravity (m/s**2)              SETU227
C        LOGSCALE = Option controlling density  units (0=kg/m**3, 1=    SETU228
C                   log10(kg/m**3), 2=% from VIRA avg., 3=kg/km**3)     SETU229
C        profwgt  =  Weight factor for auxiliary input profile data     SETU230
C...                                                                    SETU231
        OPEN(21,file=files(2),iostat=ierr2)                             SETU232
        If(NVARY.eq.0)Then                                              SETU233
          Write(21,621)                                                 SETU234
        Else                                                            SETU235
          Write(21,721)                                                 SETU236
        Endif                                                           SETU237
 621    Format('    Var_X        DENSLO     DENSAV     DENSHI',         SETU238
     &    '    DENSTOT   Radius     Rref   Grav LOGSCALE profwgt')      SETU239
 721    Format('    Var_X        Var_Y        DENSLO     DENSAV',       SETU240
     &    '     DENSHI    DENSTOT   Radius     Rref   Grav LOGSCALE',   SETU241
     &    ' profwgt')                                                   SETU242
C...    Unit 22 file = 'Perturb.txt': Headers for variables are:        SETU243
C         SigD     = Density standard deviation (% of unperturbed mean) SETU244
C         DensRand = Random density perturbation (% of unpert. mean)    SETU245
C         corlim   = Ratio of step size to correlation accuracy limit   SETU246
C                     (ideally should be 1.0 or larger)                 SETU247
C         SigU     = Stnd dev for eastward wind perturbations (m/s)     SETU248
C         SigV     = Stnd dev for northward wind perturbations (m/s)    SETU249
C         iupdate  = 1 if perturbations updated, 0 if perturbations not SETU250
C                    updated but perturbation step updated, -1 if       SETU251
C                    neither perturbations not step updated             SETU252
C...                                                                    SETU253
        OPEN(22,file=files(3),iostat=ierr3)                             SETU254
        If(NVARY.eq.0)Then                                              SETU255
          Write(22,622)                                                 SETU256
        Else                                                            SETU257
          Write(22,722)                                                 SETU258
        Endif                                                           SETU259
 622    Format('    Var_X      SigD  DensRand    corlim   SigU   SigV', SETU260
     &    ' iupdate   Tlow   Tavg  Thigh  Tpert')                       SETU261
 722    Format('    Var_X        Var_Y      SigD  DensRand    corlim',  SETU262
     &    '   SigU   SigV iupdate   Tlow   Tavg  Thigh  Tpert')         SETU263
C...    Unit 23 file = 'Winds.txt' : Headers for variables are -        SETU264
C         EWmean   = mean eastward wind component (m/s)                 SETU265
C         EWpert   = perturbation in eastward wind component (m/s)      SETU266
C         EWtot    = total (mean + perturbed) eastward wind (m/s)       SETU267
C         NSmean   = mean northward wind component (m/s)                SETU268
C         NSpert   = perturbation in northward wind component (m/s)     SETU269
C         NStot    = total (mean + perturbed) northward wind (m/s)      SETU270
C         iupdate  = 1 if perturbations updated, 0 if perturbations not SETU271
C                    updated but perturbation step updated, -1 if       SETU272
C                    neither perturbations not step updated             SETU273
C         Tlow     = Mean minus 1-sigma temperature (K)                 SETU274
C         Tavg     = Mean temperature (K)                               SETU275
C         Thigh    = Mean plus 1-sigma temperature (K)                  SETU276
C         Tpert    = perturbed temperature (K)                          SETU277
C...                                                                    SETU278
        OPEN(23,file=files(4),iostat=ierr4)                             SETU279
        If (NVARY.eq.0)Then                                             SETU280
          Write(23,623)                                                 SETU281
        Else                                                            SETU282
          Write(23,723)                                                 SETU283
        Endif                                                           SETU284
 623    Format('    Var_X      EWmean  EWpert   EWtot  NSmean  ',       SETU285
     &   'NSpert   NStot iupdate')                                      SETU286
 723    Format('    Var_X        Var_Y      EWmean  EWpert   EWtot',    SETU287
     &   '  NSmean  NSpert   NStot iupdate')                            SETU288
C...    Unit 24 file = 'TPresHgt.txt' : Headers for variables are -     SETU289
C         Temp     - Mean temperature (K)                               SETU290
C         Pres     - Mean pressure (N/m**2, log-10, or % from VIRA      SETU291
C                    mean atmosphere)                                   SETU292
C         TdegC    - Mean temperature (degrees C)                       SETU293
C         Pres_mb  - Mean pressure (mb)                                 SETU294
C         Hrho     - Density scale height (km)                          SETU295
C         Hpres    - Pressure scale height (km)                         SETU296
C         MolWt    - molecular weight (kg/kg-mole)                      SETU297
C         CO2%v    - mole (or volume) fraction for Carbon Dioxide       SETU298
C         N2%v     - mole (or volume) fraction for Nitrogen             SETU299
C         O%v      - mole (or volume) fraction for Atomic Oxygen        SETU300
C         CO%v     - mole (or volume) fraction for Carbon Monoxide      SETU301
C         He%v     - mole (or volume) fraction for Helium               SETU302
C         N%v      - mole (or volume) fraction for Atomic Nitrogen      SETU303
C         H%v      - mole (or volume) fraction for Atomic Hydrogen      SETU304
C         LOGSCALE - Option controlling pressure units (0 or 3=N/m**2,  SETU305
C                     1=log10(N/m**2), 2=%from VIRA avg.)               SETU306
C...                                                                    SETU307
        OPEN(24,file=files(5),iostat=ierr5)                             SETU308
        If (NVARY.eq.0)Then                                             SETU309
          Write(24,624)                                                 SETU310
        Else                                                            SETU311
          Write(24,724)                                                 SETU312
        Endif                                                           SETU313
 624    Format('    Var_X       Temp      Pres   TdegC',                SETU314
     &   '   Pres_mb    Hrho  Hpres MolWt CO2%v N2%v  O%v CO%v',        SETU315
     &   ' He%v  N%v   H%v LOGSCALE')                                   SETU316
 724    Format('    Var_X        Var_Y       Temp      Pres   TdegC',   SETU317
     &   '   Pres_mb    Hrho  Hpres MolWt CO2%v N2%v  O%v CO%v',        SETU318
     &   ' He%v  N%v   H%v LOGSCALE')                                   SETU319
C...    Unit 25 file = OUTPUT file containing list of variables given   SETU320
C       in Datastep_V05 routine (Format 800 or Format 810)              SETU321
C                                                                       SETU322
C...    Unit 25 file = 'OUTPUT.txt' : Headers for variables are -       SETU323
C         Time    = time after initial input time (sec); interval       SETU324
C                   controlled by input options IERT and IUTC           SETU325
C         Height  = height (km) above reference ellipsoid (OR total     SETU326
C                   radius to spacecraft (km) if NVARX=2 or NVARY=2)    SETU327
C         Lat = latitude (deg.)                                         SETU328
C         LonE or LonW = East (or West) longitude, as controlled by     SETU329
C                        input value of LonEast                         SETU330
C         DENSAV  = Mean density (kg/m**3 or other units, as controlled SETU331
C                   by input value of LOGSCALE)                         SETU332
C         Temp    = Mean temperature (K)                                SETU333
C         EWind   = Mean Eastward wind component (m/s)                  SETU334
C         NWind   = Mean Northward wind component (m/s)                 SETU335
C         sigD    = std. dev. for density perturbations (% of mean)     SETU336
C         Ls      = planetocentric longitude of Sun (degrees)           SETU337
C         SZA     = Solar zenith angle (degrees)                        SETU338
C         CO2%m   = percentage carbon dioxide concentration by mass     SETU339
C         N2%m    = percentage nitrogen concentration by mass           SETU340
C         O%m     = percentage atomic oxygen concentration by mass      SETU341
C         CO%m    = percentage carbon monoxide concentration by mass    SETU342
C         He%m    = percentage helium concentration by mass             SETU343
C         N%m     = percentage atomic nitrogen concentration by mass    SETU344
C         H%m     = percentage atomic hydrogen concentration by mass    SETU345
C...                                                                    SETU346
        OPEN(25,file=files(6),iostat=ierr6)                             SETU347
        EWlon = 'E'                                                     SETU348
        If (LonEast.eq.0)EWlon = 'W'                                    SETU349
        HgtLbl = ' Height'                                              SETU350
        If (NVARX.eq.2.or.NVARY.eq.2)HgtLbl = ' Radius'                 SETU351
        DenLbl = 'Denkgm3 '                                             SETU352
        If (LOGSCALE.eq.1)DenLbl = ' Logkgm3'                           SETU353
        If (LOGSCALE.eq.2)DenLbl = 'Den%VenA'                           SETU354
        If (LOGSCALE.eq.3)DenLbl = 'Denkgkm3'                           SETU355
        Write(25,629)HgtLbl,EWlon,DenLbl                                SETU356
 629    Format('     Time  'A7,'   Lat    Lon',A1,2X,A8,'   Temp',      SETU357
     &    '  EWind  NWind  sigD   Ls    SZA CO2%m  N2%m  O%m CO%m',     SETU358
     &    ' He%m  N%m   H%m')                                           SETU359
      Endif                                                             SETU360
C...  Test for file open error condition                                SETU361
      Do 40 j=1,6                                                       SETU362
        If(ierr(j).ne.0)then                                            SETU363
          Write(iu0,60)files(j),ierr(j)                                 SETU364
   60     Format(1x,(A),' File open error! Error =',i5)                 SETU365
          Goto 9998                                                     SETU366
        Endif                                                           SETU367
   40 Enddo                                                             SETU368
C...................................................................... SETU369
C...  Read VIRA model atmosphere data                                   SETU370
      Call ReadVIRA_V05(DATADIR)                                        SETU371
C...  Write error message and exit if bad date/time                     SETU372
      Call chkdt_V05(MYEAR,month,mday,ihr,imin,sec,idterr)              SETU373
      If (idterr.lt.-6)Then                                             SETU374
        Write(iu0,92)                                                   SETU375
      Else If (idterr.lt.0)Then                                         SETU376
        Write(iu0,91)                                                   SETU377
      Endif                                                             SETU378
      If (idterr.lt.0)Goto 9998                                         SETU379
  91  Format(' Input error in month, day or year.')                     SETU380
  92  Format(' Input error in hour, minute or seconds.')                SETU381
C...  Get Julian Day                                                    SETU382
      Call CaltoJul_V05(Myear,Month,Mday,ihr,imin,sec,DAY)              SETU383
      DAY0 = DAY                                                        SETU384
      If (iup.gt.0)Then                                                 SETU385
        If (IERT.eq.1)Then                                              SETU386
          Write(iup,283)                                                SETU387
        Else                                                            SETU388
          Write(iup,284)                                                SETU389
        Endif                                                           SETU390
        If (IUTC.eq.1)Then                                              SETU391
          Write(iup,285)                                                SETU392
        Else                                                            SETU393
          Write(iup,286)                                                SETU394
        Endif                                                           SETU395
  283   Format(' Input time is Earth-Receive Time (ERT)')               SETU396
  284   Format(' Input time is Venus-Event Time (VET)')                 SETU397
  285   Format(' Input time is Coordinated Universal Time (UTC)')       SETU398
  286   Format(' Input time is Terrestrial (Dynamical) Time (TT)')      SETU399
        Write(iup,290)MONTH,MDAY,MYEAR,DAY,IHR,IMIN,SEC                 SETU400
  290   FORMAT(' Date = ',I2,'/',I2,'/',I4,'  Julian Day = ',F13.5,     SETU401
     &   '  Time = ',I2,':',I2,':',F4.1)                                SETU402
      Endif                                                             SETU403
      IF (NR1 .LE. 0 .OR. NR1 .GE. 30000)then                           SETU404
        Write(iu0,298)                                                  SETU405
  298   Format(' Error in starting random number.')                     SETU406
        GOTO 9998                                                       SETU407
      Endif                                                             SETU408
      IX = NR1                                                          SETU409
      IY = 172 * Mod(IX, 176) - 35 * (IX / 176)                         SETU410
      IZ = 170 * Mod(IX, 178) - 63 * (IX / 178)                         SETU411
      If (IY .lt. 0) IY = IY + 30307                                    SETU412
      If (IZ .lt. 0) IZ = IZ + 30323                                    SETU413
      z1 = RANDOM_V05(L)                                                SETU414
      RHOd = PPND_V05(z1,L)                                             SETU415
      z1 = RANDOM_V05(L)                                                SETU416
      RHOu = PPND_V05(z1,L)                                             SETU417
      z1 = RANDOM_V05(L)                                                SETU418
      RHOv = PPND_V05(z1,L)                                             SETU419
      IF (L .EQ. 1)then                                                 SETU420
        Write(iu0,298)                                                  SETU421
        GOTO 9998                                                       SETU422
      Endif                                                             SETU423
      If(lstfl .ne. 'CON' .and. lstfl .ne. 'con'.and.iup.gt.0)Then      SETU424
        If (iup.gt.0)Write(iup,370)NR1,rpscale,corlmin                  SETU425
      Endif                                                             SETU426
  370 FORMAT('   Random seed =',I6,'   Scale factor =',F4.1,            SETU427
     & '   corlmin =',F6.3)                                             SETU428
  380 FORMAT(/' Select x-code and y-code for plotable output versus',   SETU429
     & ' desired parameter(s):'//                                       SETU430
     & ' Code             Parameter'/                                   SETU431
     & ' ----  -------------------------------------------------'/      SETU432
     & '   1   Height (above reference ellipsoid, km)'/                 SETU433
     & '   2   Radius (geocentric radius, km)'/                         SETU434
     & '   3   Latitude (deg.)'/                                        SETU435
     & '   4   Longitude (deg.) West if LonEast=0, East if LonEast=1'/  SETU436
     & '   5   Time from start (seconds: ERT/VET/UTC/TT per input)'/    SETU437
     & '   6   Time from start (Venus days)'/                           SETU438
     & '   7   Planetocentric longitude of Sun (Ls, degrees)'/          SETU439
     & '   8   Local true solar time (Venus hrs=1/24th Venus day)'/     SETU440
     & '   9   Pressure (mb)'/                                          SETU441
     & '  10   Pressure Height [-log(Pres/PresSurf) = -log(sigma)]'/    SETU442
     & '  11   Sigma coordinate [sigma=Pressure/(Surface Pressure)]'/   SETU443
     & '  12   Solar zenith angle (degrees)'/                           SETU444
     & '  13   Longitude in range -180 to +180 deg. (East or West)'//   SETU445
     & ' Use y-code = 0 for plotable output vs x-code variable only')   SETU446
      IF(NVARX.LT.1.OR.NVARX.GT.13)then                                 SETU447
        Write(iu0,381)                                                  SETU448
  381   Format(' x-code or y-code input error.')                        SETU449
        Write(iu0,380)                                                  SETU450
        Goto 9998                                                       SETU451
      Endif                                                             SETU452
      IF(NVARY.lt.0.or.NVARY.gt.13)then                                 SETU453
        Write(iu0,381)                                                  SETU454
        Write(iu0,380)                                                  SETU455
        Goto 9998                                                       SETU456
      Endif                                                             SETU457
      If (logscale .lt. 0 .or. logscale .gt. 3 )logscale = 0            SETU458
C...  Initialize position data                                          SETU459
      CHGT = FHGT                                                       SETU460
      CLAT = FLAT                                                       SETU461
      CLON = FLON                                                       SETU462
      CSEC = 0.                                                         SETU463
      Write(iu0,*)' Finished Setup_V05 - Starting computations'         SETU464
      Return                                                            SETU465
 9998 Stop ' Error termination! Check the LIST file for messages.'      SETU466
      END                                                               SETU467
C---------------------------------------------------------------------- SETU468
      Subroutine Randinit_V05(J,NR1,RHOd,RHOu,RHOv,iup,iustdout)        RNDI  1
      Implicit None                                                     RNDI  2
      Double Precision RHOd,RHOu,RHOv,RANDOM_V05,PPND_V05               RNDI  3
      Integer J,NR1,iup,iustdout,IX,IY,IZ,L                             RNDI  4
      COMMON /RANDCOM_V05/IX,IY,IZ                                      RNDI  5
C...  Re-initialize NR1, e.g. by reading from a file, or some algorithm RNDI  6
C     from previous NR1 value, or by some computation on index J.       RNDI  7
C     Note that it is not necessary to randomly select each seed value  RNDI  8
C     NR1 in order to get a random sequence of output.  Any regular     RNDI  9
C     progression of selected NR1 values will do for this process.      RNDI 10
      NR1 = NR1 + 11                                                    RNDI 11
      If (NR1.gt.30000)NR1 = Mod(NR1,30000)                             RNDI 12
C...  Write random seed value to list file                              RNDI 13
      If (iup.ne.0)Then                                                 RNDI 14
        Write(iup,10)J,NR1                                              RNDI 15
      Else                                                              RNDI 16
        Write(iustdout,10)J,NR1                                         RNDI 17
      Endif                                                             RNDI 18
  10  Format('   Random seed number',I6,' =',I6)                        RNDI 19
      IX = NR1                                                          RNDI 20
      IY = 172 * Mod(IX, 176) - 35 * (IX / 176)                         RNDI 21
      IZ = 170 * Mod(IX, 178) - 63 * (IX / 178)                         RNDI 22
      If (IY .lt. 0) IY = IY + 30307                                    RNDI 23
      If (IZ .lt. 0) IZ = IZ + 30323                                    RNDI 24
      RHOd = RANDOM_V05(L)                                              RNDI 25
      RHOd = PPND_V05(RHOd,L)                                           RNDI 26
      RHOu = RANDOM_V05(L)                                              RNDI 27
      RHOu = PPND_V05(RHOu,L)                                           RNDI 28
      RHOv = RANDOM_V05(L)                                              RNDI 29
      RHOv = PPND_V05(RHOv,L)                                           RNDI 30
      Return                                                            RNDI 31
      End                                                               RNDI 32
C---------------------------------------------------------------------- RNDI 33
       Subroutine chkdt_V05(MYEAR,month,iday,ihour,minutes,sec,err)     CKDT  1
C                                                                       CKDT  2
C      CHecKs input Date and Time for validity and internal             CKDT  3
C      consistency.  Returns error message(s) and prompts               CKDT  4
C      user to re-enter inputs.                                         CKDT  5
C                                                                       CKDT  6
       Implicit none                                                    CKDT  7
       Integer MYEAR,month,iday,ihour,minutes,err                       CKDT  8
       Double Precision sec                                             CKDT  9
       Logical centyear,leapyear                                        CKDT 10
       err=0                                                            CKDT 11
       centyear=.false.                                                 CKDT 12
       leapyear=.false.                                                 CKDT 13
C...   Convert to 4-digit year, if necessary                            CKDT 14
       If (MYEAR.ge.0.and.MYEAR.le.69)MYEAR = MYEAR + 2000              CKDT 15
       If (MYEAR.ge.70.and.MYEAR.le.99)MYEAR = MYEAR + 1900             CKDT 16
       If (MYEAR.lt.1960.or.MYEAR.gt.2069)Then                          CKDT 17
         Write(*,*)' Year must be 1960-2069'                            CKDT 18
         err=-1                                                         CKDT 19
       Endif                                                            CKDT 20
       If (MYEAR/100.-int(MYEAR/100.).eq.0)centyear=.true.              CKDT 21
       If (MYEAR/4.-int(MYEAR/4.).eq.0)leapyear=.true.                  CKDT 22
       If (centyear)Then                                                CKDT 23
         If (MYEAR/400.-int(MYEAR/400.).gt.0)leapyear=.false.           CKDT 24
       Endif                                                            CKDT 25
       If (month.lt.1.or.month.gt.12)Then                               CKDT 26
C...     Write(*,*)' Month must be 1-12'                                CKDT 27
         err=-2                                                         CKDT 28
       Endif                                                            CKDT 29
       If (month.eq.4.or.month.eq.6.or.month.eq.9.or.month.eq.11)Then   CKDT 30
         If (iday.lt.1.or.iday.gt.30)Then                               CKDT 31
C...       Write(*,*)' Day of month must be 1-30'                       CKDT 32
           err=-3                                                       CKDT 33
         Endif                                                          CKDT 34
       Else If (month.eq.2)Then                                         CKDT 35
         If(leapyear)Then                                               CKDT 36
           If (iday.lt.1.or.iday.gt.29)Then                             CKDT 37
C...         Write(*,*)' Day of month must be 1-29 (Leap Year)'         CKDT 38
             err=-4                                                     CKDT 39
           Endif                                                        CKDT 40
         Else If (iday.lt.1.or.iday.gt.28)Then                          CKDT 41
C...       Write(*,*)' Day of month must be 1-28 (Non-Leap Year)'       CKDT 42
           err=-5                                                       CKDT 43
         Endif                                                          CKDT 44
       Else If (iday.lt.1.or.iday.gt.31)Then                            CKDT 45
C...       Write(*,*)' Day of month must be 1-31'                       CKDT 46
           err=-6                                                       CKDT 47
       Endif                                                            CKDT 48
       If (ihour.lt.0.or.ihour.gt.24)Then                               CKDT 49
C...     Write(*,*)' Hour must be 0-24'                                 CKDT 50
         err=-7                                                         CKDT 51
       Endif                                                            CKDT 52
       If (ihour.eq.24.and.(minutes.ne.0.or.sec.ne.0.0))Then            CKDT 53
C...     Write(*,*)' Hour must be 23 or Time must be 24:00:00'          CKDT 54
         err=-7                                                         CKDT 55
       Endif                                                            CKDT 56
       If (minutes.lt.0.or.minutes.gt.60)Then                           CKDT 57
C...     Write(*,*)' Minutes must be 0-60'                              CKDT 58
         err=-8                                                         CKDT 59
       Endif                                                            CKDT 60
       If (minutes.eq.60.and.sec.ne.0.0)Then                            CKDT 61
C...     Write(*,*)' Minutes must be 59 or Seconds must be 0'           CKDT 62
         err=-8                                                         CKDT 63
       Endif                                                            CKDT 64
       If (sec.lt.0.0.or.sec.gt.60.0)Then                               CKDT 65
C...     Write(*,*)' Seconds must be 0.0-60.0'                          CKDT 66
         err=-9                                                         CKDT 67
       Endif                                                            CKDT 68
       Return                                                           CKDT 69
       End                                                              CKDT 70
C---------------------------------------------------------------------- CKDT 71
      Subroutine ReadVIRA_V05(DATADIR)                                  RDVD  1
C...  Reads VIRA data tables and stores in common block VIRAdata_V05.   RDVD  2
C     Computes reference VIRA data values and stores in common block    RDVD  3
C     VIRAref_V05.  DATADIR is first part of path to VIRA data          RDVD  4
C     directory.                                                        RDVD  5
      Implicit None                                                     RDVD  6
      Integer lendir,i,j                                                RDVD  7
      Character*60 DATADIR                                              RDVD  8
      Character*1 dummy                                                 RDVD  9
      Double Precision zlo,plo,dlo,tlo,zetalo,Rlo,zmd,pmd,dmd,tmd,      RDVD 10
     &  yco2md,yn2md,yomd,ycomd,yhemd,ynmd,zhi,phi,dhi,xlat,sza,xlst,   RDVD 11
     &  thi,yco2hi,yn2hi,yohi,ycohi,yhehi,ynhi,yhhi,z,p,d,t,Rgas,       RDVD 12
     &  zeta,yco2,yn2,yo,yco,yhe,yn,yh,avmw,R0,AvN,zref,pref,           RDVD 13
     &  dref,tref                                                       RDVD 14
C...  Common block for VIRA data arrays                                 RDVD 15
C     Low altitude data (0-100 km) indexed 81 by 5, with 81 heights     RDVD 16
C     and 5 latitudes (30,45,60,75,85).                                 RDVD 17
C     Middle altitude data (100-150 km) indexed 11 by 2, with 11        RDVD 18
C     heights and 2 times (LST = 0, 12).                                RDVD 19
C     High altitude data (150-250 km) indexed 21 by 7, with 21          RDVD 20
C     heights and 7 solar zenith angles (15,34,61,90,119,146,165).      RDVD 21
C     Low, middle, high altitude data designated lo, md, hi, with -     RDVD 22
C     z = height (km)                                                   RDVD 23
C     p = pressure (N/m**2)                                             RDVD 24
C     d = density (kg/m**3)                                             RDVD 25
C     t = temperature (K)                                               RDVD 26
C     R = gas constant (SI units)                                       RDVD 27
C     zeta = compressibility factor [ = p/(d*R*t) ]                     RDVD 28
C     yco2 = CO2 number density (#/m**3)                                RDVD 29
C     yn2 = N2 number density (#/m**3)                                  RDVD 30
C     yo = O number density (#/m**3)                                    RDVD 31
C     yco = CO number density (#/m**3)                                  RDVD 32
C     yhe = He number density (#/m**3)                                  RDVD 33
C     yn = N number density (#/m**3)                                    RDVD 34
C     yh = H number density (#/m**3)                                    RDVD 35
      Common /VIRAdata_V05/zlo(81),plo(81,5),dlo(81,5),tlo(81,5),       RDVD 36
     &  zetalo(81,5),Rlo(81,5),zmd(11),pmd(11,2),dmd(11,2),tmd(11,2),   RDVD 37
     &  yco2md(11,2),yn2md(11,2),yomd(11,2),ycomd(11,2),yhemd(11,2),    RDVD 38
     &  ynmd(11,2),zhi(21),phi(21,7),dhi(21,7),                         RDVD 39
     &  thi(21,7),yco2hi(21,7),yn2hi(21,7),yohi(21,7),ycohi(21,7),      RDVD 40
     &  yhehi(21,7),ynhi(21,7),yhhi(21,7)                               RDVD 41
C...  Common block for VIRA reference data arrays                       RDVD 42
      Common /VIRAref_V05/zref(111),pref(111),dref(111),tref(111)       RDVD 43
C...  Compute character string length of DATADIR path name              RDVD 44
      lendir = Index(DATADIR,' ')-1                                     RDVD 45
      If (lendir.lt.1.or.lendir.gt.60)lendir = 60                       RDVD 46
      Open(26,file=DATADIR(1:lendir)//'VIRALow.txt',status='old')       RDVD 47
      Open(27,file=DATADIR(1:lendir)//'VIRAMid.txt',status='old')       RDVD 48
      Open(28,file=DATADIR(1:lendir)//'VIRAHi.txt',status='old')        RDVD 49
C...  Read and ignore header information                                RDVD 50
      Read(26,10)dummy                                                  RDVD 51
      Read(27,10)dummy                                                  RDVD 52
      Read(28,10)dummy                                                  RDVD 53
  10  Format(A1)                                                        RDVD 54
      R0 = 8314.472d0                                                   RDVD 55
      AvN = 6.02214199d26                                               RDVD 56
C.... Read low altitude (0-100 km) VIRA data and store in Common arrays RDVD 57
      Do 30 j = 1,5                                                     RDVD 58
      Do 20 i = 1,81                                                    RDVD 59
        Read(26,*)z,xlat,d,p,t,zeta,Rgas                                RDVD 60
        avmw = R0/Rgas                                                  RDVD 61
C...    Store low altitude VIRA data in arrays                          RDVD 62
        zlo(i) = z                                                      RDVD 63
        plo(i,j) = zeta*d*Rgas*t                                        RDVD 64
        dlo(i,j) = d                                                    RDVD 65
        tlo(i,j) = t                                                    RDVD 66
        zetalo(i,j) = zeta                                              RDVD 67
        Rlo(i,j) = Rgas                                                 RDVD 68
  20  Enddo                                                             RDVD 69
  30  Enddo                                                             RDVD 70
C.... Read middle altitude (100-150 km) VIRA data and store in Common   RDVD 71
C     arrays                                                            RDVD 72
      Do 50 j = 1,2                                                     RDVD 73
      Do 40 i = 1,11                                                    RDVD 74
        Read(27,*)z,xlst,yco2,yn2,yo,yco,yhe,yn,d,p,t,avmw              RDVD 75
C...    Store middle altitude VIRA data in arrays                       RDVD 76
        zmd(i) = z                                                      RDVD 77
        pmd(i,j) = p                                                    RDVD 78
        dmd(i,j) = d                                                    RDVD 79
        tmd(i,j) = t                                                    RDVD 80
        yco2md(i,j) = yco2                                              RDVD 81
        yn2md(i,j) = yn2                                                RDVD 82
        yomd(i,j) = yo                                                  RDVD 83
        ycomd(i,j) = yco                                                RDVD 84
        yhemd(i,j) = yhe                                                RDVD 85
        ynmd(i,j) = yn                                                  RDVD 86
  40  Enddo                                                             RDVD 87
  50  Enddo                                                             RDVD 88
C.... Read high altitude (150-250 km) VIRA data; store in Common arrays RDVD 89
      Do 70 j = 1,7                                                     RDVD 90
      Do 60 i = 1,21                                                    RDVD 91
        Read(28,*)z,sza,yco2,yn2,yo,yco,yhe,yn,yh,d,p,t,avmw            RDVD 92
C...    Store high altitude VIRA data in arrays                         RDVD 93
        zhi(i) = z                                                      RDVD 94
        phi(i,j) = p                                                    RDVD 95
        dhi(i,j) = d                                                    RDVD 96
        thi(i,j) = t                                                    RDVD 97
        yco2hi(i,j) = yco2                                              RDVD 98
        yn2hi(i,j) = yn2                                                RDVD 99
        yohi(i,j) = yo                                                  RDVD100
        ycohi(i,j) = yco                                                RDVD101
        yhehi(i,j) = yhe                                                RDVD102
        ynhi(i,j) = yn                                                  RDVD103
        yhhi(i,j) = yh                                                  RDVD104
   60 Enddo                                                             RDVD105
   70 Enddo                                                             RDVD106
C...  Close VIRA data files                                             RDVD107
      Close(26)                                                         RDVD108
      Close(27)                                                         RDVD109
      Close(28)                                                         RDVD110
C...  Save reference values of pressure, density, temperature           RDVD111
C...  Reference values for low altitudes (0-98 km) (0-30 lat data)      RDVD112
      Do 80 i = 1,80                                                    RDVD113
        zref(i) = zlo(i)                                                RDVD114
        pref(i) = plo(i,1)                                              RDVD115
        dref(i) = dlo(i,1)                                              RDVD116
        tref(i) = tlo(i,1)                                              RDVD117
  80  Enddo                                                             RDVD118
C...  Reference values for 100 km altitude (average from low and        RDVD119
C     middle altitude data sets at 100 km)                              RDVD120
      zref(81) = zmd(1)                                                 RDVD121
      pref(81) = dSqrt(dSqrt(pmd(1,1)*pmd(1,2))*plo(81,1))              RDVD122
      dref(81) = dSqrt(dSqrt(dmd(1,1)*dmd(1,2))*dlo(81,1))              RDVD123
      tref(81) = 0.5d0*(0.5d0*(tmd(1,1)+tmd(1,2)) + tlo(81,1))          RDVD124
C...  Reference values for middle altitudes (105-145 km) (average       RDVD125
C     of LST = 0 and LST = 12)                                          RDVD126
      Do 90 i = 2,10                                                    RDVD127
        zref(80+i) = zmd(i)                                             RDVD128
        pref(80+i) = dSqrt(pmd(i,1)*pmd(i,2))                           RDVD129
        dref(80+i) = dSqrt(dmd(i,1)*dmd(i,2))                           RDVD130
        tref(80+i) = 0.5d0*(tmd(i,1)+tmd(i,2))                          RDVD131
  90  Enddo                                                             RDVD132
C...  Reference values for 150 km (average of middle and high altitude  RDVD133
C     data sets at 150 km)                                              RDVD134
      zref(91) = zhi(1)                                                 RDVD135
      pref(91) = dSqrt(phi(1,4)*dSqrt(pmd(11,1)*pmd(11,2)))             RDVD136
      dref(91) = dSqrt(dhi(1,4)*dSqrt(dmd(11,1)*dmd(11,2)))             RDVD137
      tref(91) = 0.5d0*(thi(1,4) + 0.5d0*(tmd(11,1)+tmd(11,2)))         RDVD138
C...  Reference values for high altitudes (155-250 km) (solar zenith    RDVD139
C     angle 90 data from high altitude range)                           RDVD140
      Do 100 i = 2,21                                                   RDVD141
        zref(90+i) = zhi(i)                                             RDVD142
        pref(90+i) = phi(i,4)                                           RDVD143
        dref(90+i) = dhi(i,4)                                           RDVD144
        tref(90+i) = thi(i,4)                                           RDVD145
 100  Enddo                                                             RDVD146
      Return                                                            RDVD147
      End                                                               RDVD148
C---------------------------------------------------------------------- RDVD149
