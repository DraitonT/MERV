C...  Program to find date and time for a desired Venus planetocentric  FNDA  1
C     solar longitude (Ls) and date and time for a desired Venus local  FNDA  2
C     true solar time (LTST) at a given Venus site longitude.           FNDA  3
C...................................................................... FNDA  4
      Implicit None                                                     FNDA  5
      Double Precision DAY,DAY0,rate,time,dLs,xLs,yLs,lonout,           FNDA  6
     & latsun,lonsun,radius,lonsite,xLST,yLST,dLST,dayrate,owlt,EOT,    FNDA  7
     & dt,ttsec,sec,latsite,sza                                         FNDA  8
      Integer MYEAR,lonew,iutc,iert,month,iday,ihour,minutes,m,         FNDA  9
     &  icontinue,n,ierr                                                FNDA 10
      Character*3 name,time1                                            FNDA 11
      Character*1 EW                                                    FNDA 12
      Character*2 time2                                                 FNDA 13
C...  Ls rate = Earth days/Ls degree = Venus tropical year (days)/360   FNDA 14
      rate = 2.24695d2/3.6d2                                            FNDA 15
C...  LTST dayrate = Earth hrs/Venus hr = Venus day length (in hrs)/24  FNDA 16
      dayrate = 2802.0d0/24.0d0                                         FNDA 17
C...................................................................... FNDA 18
C                                                                       FNDA 19
C     User provides starting date and time.  This may be an arbitrary   FNDA 20
C     value e.g. January 1, 00:00:00 on a given year, just to set the   FNDA 21
C     year for which Ls and LTST are to be calculated.  The starting    FNDA 22
C     date and time may also be any specific time for which the user    FNDA 23
C     wants to compute the Ls or LTST, using the Venus ephemeris        FNDA 24
C     subroutine.  The year given may be any (4-digit) year 1970-2069   FNDA 25
C                                                                       FNDA 26
C     Note: The program computes the next subsequent occurrence of      FNDA 27
C     desired Ls after the start date.  If the user desires an          FNDA 28
C     occurrence of Ls before the start date used, the program should   FNDA 29
C     be re-run, using an earlier start date.  LTST calculated is       FNDA 30
C     nearest occurrence to the date/time of the desired Ls occurence.  FNDA 31
C                                                                       FNDA 32
C     An option is provided whereby the user can select either West     FNDA 33
C     longitude or East longitude for specifying the Venus site         FNDA 34
C     location for which LTST is desired.                               FNDA 35
C                                                                       FNDA 36
C     This program uses the Alison Venus ephemeris subroutine           FNDA 37
C     to compute Ls and solar longitude.  RMS accuracy of both is about FNDA 38
C     0.004 degrees.  RMS accuracy of LTST is approximately 1 second.   FNDA 39
C                                                                       FNDA 40
C     Options allow time to be expressed in either Terrestrial          FNDA 41
C     Dynamical Time (TT) or Coordinated Universal Time (UTC) and in    FNDA 42
C     either Venus-Event (VE) time or Earth-Receive (ER) time.          FNDA 43
C                                                                       FNDA 44
C...................................................................... FNDA 45
C                                                                       FNDA 46
  1   Write(*,*)' Enter 0 for West Longitude, 1 for East Longitude'     FNDA 47
      Read(*,*)lonew                                                    FNDA 48
      If (lonew.lt.0.or.lonew.gt.1)Then                                 FNDA 49
        Write(*,*)' Must enter 0 or 1'                                  FNDA 50
        Goto 1                                                          FNDA 51
      Endif                                                             FNDA 52
      EW = 'E'                                                          FNDA 53
      If (lonew.eq.0)EW='W'                                             FNDA 54
  4   Write(*,*)' Enter 0 for Terrestrial Dynamical Time, 1 for UTC'    FNDA 55
      Read(*,*)iutc                                                     FNDA 56
      If (iutc.lt.0.or.iutc.gt.1)Then                                   FNDA 57
         Write(*,*)' Must enter 0 or 1'                                 FNDA 58
         Goto 4                                                         FNDA 59
      Endif                                                             FNDA 60
      time1 = ' TT'                                                     FNDA 61
      If (iutc.eq.1)time1 = 'UTC'                                       FNDA 62
  5   Write(*,*)' Enter 0 for Venus Event time, 1 for Earth Receive',   FNDA 63
     &   ' time'                                                        FNDA 64
      Read(*,*)iert                                                     FNDA 65
      If (iert.lt.0.or.iert.gt.1)Then                                   FNDA 66
        Write(*,*)' Must enter 0 or 1'                                  FNDA 67
        Goto 5                                                          FNDA 68
      Endif                                                             FNDA 69
      time2 = 'VE'                                                      FNDA 70
      If (iert.eq.1)time2 = 'ER'                                        FNDA 71
  2   Write(*,3)                                                        FNDA 72
  3   Format(' Enter start year, month, day, hour, minute, seconds,',   FNDA 73
     &  ' and desired Ls (deg)')                                        FNDA 74
      Read(*,*)MYEAR,month,iday,ihour,minutes,sec,xLs                   FNDA 75
C...  Check date and time input                                         FNDA 76
      Call chkdt(MYEAR,month,iday,ihour,minutes,sec,ierr)               FNDA 77
      If (ierr.lt.-6)Then                                               FNDA 78
        Write(*,*)' CHKDT detected TIME error condition'                FNDA 79
      Else If (ierr.lt.0)Then                                           FNDA 80
        Write(*,*)' CHKDT detected DATE error condition'                FNDA 81
      Endif                                                             FNDA 82
C...  Check Ls input                                                    FNDA 83
      If (xLs.lt.0.0.or.xLs.gt.3.6d2)Then                               FNDA 84
        Write(*,*)' Ls must be 0-360'                                   FNDA 85
        ierr = -10                                                      FNDA 86
      Endif                                                             FNDA 87
      If (ierr.lt.0)Goto 2                                              FNDA 88
C...  Get month name from month                                         FNDA 89
      Call namemonth(month,name)                                        FNDA 90
  6   Write(*,7)EW                                                      FNDA 91
  7   Format(' Enter Venus Site Lon (deg',A1,'), Lat (deg),',           FNDA 92
     &  ' and Local True Solar Time (hr)')                              FNDA 93
      Read(*,*)lonsite,latsite,xLST                                     FNDA 94
      If(dAbs(lonsite).gt.3.6d2)Then                                    FNDA 95
        Write(*,*)' Longitude must be -360 to +360'                     FNDA 96
        Goto 6                                                          FNDA 97
      Endif                                                             FNDA 98
      lonout = lonsite                                                  FNDA 99
C...  Convert to East longitude if necessary                            FNDA100
      If (lonew.eq.0)lonsite = 3.6d2 - lonsite                          FNDA101
      If (xLST.lt.0.0.or.xLST.gt.2.4d1)Then                             FNDA102
        Write(*,*)' Local Time must be 0-24'                            FNDA103
        Goto 6                                                          FNDA104
      Endif                                                             FNDA105
C...  Get initial Julian day (DAY0)                                     FNDA106
      Call CaltoJul(MYEAR,month,iday,ihour,minutes,sec,DAY0)            FNDA107
      ttsec = 0.0                                                       FNDA108
      If (iutc.eq.1)Then                                                FNDA109
C...    Get terrestrial dynamical time offset (seconds)                 FNDA110
        dt = (MYEAR - 2000.0d0)/100.0d0                                 FNDA111
C...    Terrestrial time = UTC + ttsec                                  FNDA112
        ttsec = (64.184d0 + 95.0d0*dt + 35.0d0*dt**2)/86400.0d0         FNDA113
      Endif                                                             FNDA114
C...  Use Allison ephemeris routine to determine Ls and solar lon.      FNDA115
C     at DAY0                                                           FNDA116
      Call vensephm(DAY0+ttsec,latsun,lonsun,yLs,radius,owlt,EOT)       FNDA117
      If(iert.eq.1)Call vensephm(DAY0+ttsec-owlt/1440.0d0,latsun,       FNDA118
     &  lonsun,yLs,radius,owlt,EOT)                                     FNDA119
      Call SolZenAng(latsun,lonsun,latsite,lonsite,sza)                 FNDA120
      DAY = DAY0                                                        FNDA121
      Write(*,25)latsun,lonsun,radius,owlt,sza                          FNDA122
 25   Format('  SunLat=',F5.2,' SunLon=',F7.2,'E Rad=',F6.4,            FNDA123
     &  ' OWLT=',F6.2,' SZA=',F7.2,'  @start time')                     FNDA124
      Write(*,30)time1,time2                                            FNDA125
 30   Format('     Ls   Year Month Day  'A3,'_',A2,'time  SiteLon',     FNDA126
     & '  LTST    JulianDay')                                           FNDA127
C...  Compute solar time from site longitude and solar longitude        FNDA128
      yLST = 1.2d1 + (lonsun - lonsite)/1.5d1                           FNDA129
      If (yLST.lt.0.)yLST = yLST + 2.4d1                                FNDA130
      If (yLST.gt.2.4d1)yLST = yLST - 2.4d1                             FNDA131
C...  Write out Ls and local time at starting date, time, and given     FNDA132
C     site longitude                                                    FNDA133
      Write(*,140)yLs,MYEAR,name,iday,ihour,minutes,sec,                FNDA134
     &  lonout,EW,yLST,DAY0,'@start time  '                             FNDA135
C...  Compute difference of Ls from desired value                       FNDA136
      dls = xLs - yLs                                                   FNDA137
      If (dls.lt.-3.0d0)dls = 3.6d2 + dls                               FNDA138
      If (dls.gt.3.57d2)dls = dls - 3.6d2                               FNDA139
C...  Start iteration to find date and time for desired Ls value        FNDA140
      Do 100 m = 1,40                                                   FNDA141
C...    Adjust date based on current Ls difference                      FNDA142
        DAY = DAY + rate*dls                                            FNDA143
        ttsec = 0.0                                                     FNDA144
        If (iutc.eq.1)Then                                              FNDA145
C...      Get terrestrial dynamical time offset (seconds)               FNDA146
          dt = (DAY - 2451545.0d0)/36525.0d0                            FNDA147
C...      Terrestrial time = UTC + ttsec                                FNDA148
          ttsec = (64.184d0 + 95.0d0*dt + 35.0d0*dt**2)/86400.0d0       FNDA149
        Endif                                                           FNDA150
C...    Update to get new Ls and solar longitude                        FNDA151
        Call vensephm(DAY+ttsec,latsun,lonsun,yLs,radius,owlt,EOT)      FNDA152
        If (iert.eq.1)Call vensephm(DAY+ttsec-owlt/1400.0d0,latsun,     FNDA153
     &    lonsun,yLs,radius,owlt,EOT)                                   FNDA154
C...    Update to get new Ls difference from desired value              FNDA155
        dls = xLs - yLs                                                 FNDA156
        If (dls.lt.-1.8d2)dls = 3.6d2 + dls                             FNDA157
        If (dls.gt.1.8d2)dls = dls - 3.6d2                              FNDA158
C...    Terminate iteration if desired accuracy is achieved             FNDA159
        If (dAbs(rate*dls).lt.1.0d-6)Goto 110                           FNDA160
 100  Continue                                                          FNDA161
      Write(*,*)' Ls calculation did not converge, m=',m                FNDA162
 110  Continue                                                          FNDA163
C...  Get calendar date from new Julian day                             FNDA164
      Call JultoCal(DAY,MYEAR,month,iday,time)                          FNDA165
C...  Get month name from month                                         FNDA166
      Call namemonth(month,name)                                        FNDA167
C...  Time of day in hours                                              FNDA168
      time = 2.4d1*time                                                 FNDA169
C...  Get hour, minute, seconds from time                               FNDA170
      ihour = int(time)                                                 FNDA171
      minutes = int(60.*(time-int(time)))                               FNDA172
      sec = (60.*(time-int(time))-minutes)*60.                          FNDA173
C...  Compute solar time from site longitude and solar longitude        FNDA174
      yLST = 1.2d1 + (lonsun - lonsite)/1.5d1                           FNDA175
      If (yLST.lt.0.)yLST =  yLST + 2.4d1                               FNDA176
      If (yLST.gt.2.4d1)yLST = yLST - 2.4d1                             FNDA177
C...  Write date and time for desired Ls, and corresponding LTST        FNDA178
      Write(*,140)xLs,MYEAR,name,iday,ihour,minutes,sec,                FNDA179
     &  lonout,EW,yLST,DAY,'@desired Ls  '                              FNDA180
 140  Format(F9.3,I5,2x,A3,3x,I2,2x,I2,':',I2,':',F4.1,F8.2,A1,F7.3,    FNDA181
     & F14.5,1x,A13)                                                    FNDA182
C...  Find nearest time when LTST = given value at site longitude       FNDA183
C...  Compute deviation of LTST from desired value                      FNDA184
      dLST = xLST - yLST                                                FNDA185
      If (dLST.gt.1.2d1)dLST = dLST - 2.4d1                             FNDA186
      If (dLST.lt.-1.2d1)dLST = dLST + 2.4d1                            FNDA187
C...  Store initial Julian day for LTST iteration                       FNDA188
      DAY0 = DAY - time/2.4d1                                           FNDA189
C...  Start iteration to find date and time for desired LTST            FNDA190
      Do 200 n = 1,10                                                   FNDA191
C...    Update time of day based on LTST difference from desired value  FNDA192
        time = time + dayrate*dLST                                      FNDA193
C...    Update Julian day                                               FNDA194
        DAY = DAY0 + time/2.4d1                                         FNDA195
          ttsec = 0.0                                                   FNDA196
          If (iutc.eq.1)Then                                            FNDA197
C...        Get terrestrial dynamical time offset (seconds)             FNDA198
            dt = (DAY - 2451545.0d0)/36525.0d0                          FNDA199
C...        Terrestrial time = UTC + ttsec                              FNDA200
            ttsec = (64.184d0 + 95.0d0*dt + 35.0d0*dt**2)/86400.0d0     FNDA201
          Endif                                                         FNDA202
C...    Compute new solar longitude and Ls                              FNDA203
        Call vensephm(DAY+ttsec,latsun,lonsun,yLs,radius,owlt,EOT)      FNDA204
        If (iert.eq.1)Call vensephm(DAY+ttsec-owlt/1440.0d0,latsun,     FNDA205
     &    lonsun,yLs,radius,owlt,EOT)                                   FNDA206
C...    Compute Venus solar time from site longitude and solar          FNDA207
C       longitude                                                       FNDA208
        yLST = 1.2d1 + (lonsun - lonsite)/1.5d1                         FNDA209
        If (yLST.lt.0.)yLST =  yLST + 2.4d1                             FNDA210
        If (yLST.gt.2.4d1)yLST = yLST - 2.4d1                           FNDA211
C...    Update deviation from desired local true solar time             FNDA212
        dLST = xLST - yLST                                              FNDA213
        If (dLST.gt.1.2d1)dLST = dLST - 2.4d1                           FNDA214
        If (dLST.lt.-1.2d1)dLST = dLST + 2.4d1                          FNDA215
C...    Terminate iteration if desired accuracy is achieved             FNDA216
        If (dAbs(dLST).lt.1.0d-5)Goto 210                               FNDA217
  200 Enddo                                                             FNDA218
      Write(*,*)' LTST calculation did not converge, n=',n              FNDA219
C...  Write date and time for desired LTST, and corresponding Ls        FNDA220
  210 Continue                                                          FNDA221
C...  Get calendar date from new Julian day                             FNDA222
      Call JultoCal(DAY,MYEAR,month,iday,time)                          FNDA223
C...  Get month name from month                                         FNDA224
      Call namemonth(month,name)                                        FNDA225
C...  Time of day in hours                                              FNDA226
      time = 2.4d1*time                                                 FNDA227
C...  Get hour, minute, seconds from time                               FNDA228
      ihour = int(time)                                                 FNDA229
      minutes = int(60.*(time-int(time)))                               FNDA230
      sec = (60.*(time-int(time))-minutes)*60.                          FNDA231
      Write(*,140)yLs,MYEAR,name,iday,ihour,minutes,sec,                FNDA232
     &  lonout,EW,yLST,DAY,'@desired LTST'                              FNDA233
      Write(*,*)' Enter 0 to stop, or 1 to compute more cases'          FNDA234
      Read(*,*)icontinue                                                FNDA235
      If (icontinue.ne.0)Goto 2                                         FNDA236
      End                                                               FNDA237
C---------------------------------------------------------------------  FNDA238
       Subroutine chkdt(MYEAR,month,iday,ihour,minutes,sec,err)         CKDT  1
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
       If (MYEAR.lt.1957.or.MYEAR.gt.2069)Then                          CKDT 17
         Write(*,*)' Year must be 1957-2069'                            CKDT 18
         err=-1                                                         CKDT 19
       Endif                                                            CKDT 20
       If (MYEAR/100.-int(MYEAR/100.).eq.0)centyear=.true.              CKDT 21
       If (MYEAR/4.-int(MYEAR/4.).eq.0)leapyear=.true.                  CKDT 22
       If (centyear)Then                                                CKDT 23
         If (MYEAR/400.-int(MYEAR/400.).gt.0)leapyear=.false.           CKDT 24
       Endif                                                            CKDT 25
       If (month.lt.1.or.month.gt.12)Then                               CKDT 26
         Write(*,*)' Month must be 1-12'                                CKDT 27
         err=-2                                                         CKDT 28
       Endif                                                            CKDT 29
       If (month.eq.4.or.month.eq.6.or.month.eq.9.or.month.eq.11)Then   CKDT 30
         If (iday.lt.1.or.iday.gt.30)Then                               CKDT 31
           Write(*,*)' Day of month must be 1-30'                       CKDT 32
           err=-3                                                       CKDT 33
         Endif                                                          CKDT 34
       Else If (month.eq.2)Then                                         CKDT 35
         If(leapyear)Then                                               CKDT 36
           If (iday.lt.1.or.iday.gt.29)Then                             CKDT 37
             Write(*,*)' Day of month must be 1-29 (Leap Year)'         CKDT 38
             err=-4                                                     CKDT 39
           Endif                                                        CKDT 40
         Else If (iday.lt.1.or.iday.gt.28)Then                          CKDT 41
           Write(*,*)' Day of month must be 1-28 (Non-Leap Year)'       CKDT 42
           err=-5                                                       CKDT 43
         Endif                                                          CKDT 44
       Else If (iday.lt.1.or.iday.gt.31)Then                            CKDT 45
           Write(*,*)' Day of month must be 1-31'                       CKDT 46
           err=-6                                                       CKDT 47
       Endif                                                            CKDT 48
       If (ihour.lt.0.or.ihour.gt.24)Then                               CKDT 49
         Write(*,*)' Hour must be 0-24'                                 CKDT 50
         err=-7                                                         CKDT 51
       Endif                                                            CKDT 52
       If (ihour.eq.24.and.(minutes.ne.0.or.sec.ne.0.0))Then            CKDT 53
         Write(*,*)' Hour must be 23 or Time must be 24:00:00'          CKDT 54
         err=-7                                                         CKDT 55
       Endif                                                            CKDT 56
       If (minutes.lt.0.or.minutes.gt.60)Then                           CKDT 57
         Write(*,*)' Minutes must be 0-60'                              CKDT 58
         err=-8                                                         CKDT 59
       Endif                                                            CKDT 60
       If (minutes.eq.60.and.sec.ne.0.0)Then                            CKDT 61
         Write(*,*)' Minutes must be 59 or Seconds must be 0'           CKDT 62
         err=-8                                                         CKDT 63
       Endif                                                            CKDT 64
       If (sec.lt.0.0.or.sec.gt.60.0)Then                               CKDT 65
         Write(*,*)' Seconds must be 0.0-60.0'                          CKDT 66
         err=-9                                                         CKDT 67
       Endif                                                            CKDT 68
       Return                                                           CKDT 69
       End                                                              CKDT 70
C---------------------------------------------------------------------- CKDT 71
      Subroutine CaltoJul(iY,iM,iD,ihour,imin,sec,xJD)                  CTOJ  1
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
        Subroutine JultoCal(JD,year,month,day,dayfrac)                  JCAL  1
C...    Compute year, month, day of month, and fraction of day          JCAL  2
C       from Julian day JD.  Algorithm from Meeus, Astronomical         JCAL  3
C       Algorithms, 2nd edition, 1998, page 63                          JCAL  4
        Implicit None                                                   JCAL  5
        Double Precision JD,dayfrac,F,JDp                               JCAL  6
        Integer year,month,day,Z,A,alpha,B,C,D,E                        JCAL  7
C...    Add 0.5 to Julian day                                           JCAL  8
        JDp = JD + 0.5d0                                                JCAL  9
C...    Get integer part and fractional part of JD+0.5                  JCAL 10
        Z = Int(JDp)                                                    JCAL 11
        F = JDp - Z                                                     JCAL 12
C...    Compute parameter A                                             JCAL 13
        If (Z.lt.2299161)Then                                           JCAL 14
          A = Z                                                         JCAL 15
        Else                                                            JCAL 16
          alpha = IDint((Z - 1867216.25d0)/36524.25d0)                  JCAL 17
          A = Z + 1 + alpha - IDint(alpha/4.0d0)                        JCAL 18
        Endif                                                           JCAL 19
C...    Compute parameters B, C, D, and E                               JCAL 20
        B = A + 1524                                                    JCAL 21
        C = IDint((B - 122.1d0)/365.25d0)                               JCAL 22
        D = IDint(365.25d0*C)                                           JCAL 23
        E = IDint((B-D)/30.6001d0)                                      JCAL 24
C...    Get integer day of month and fractional day from parameters     JCAL 25
C         B, D, E, and F                                                JCAL 26
        dayfrac = B - D - IDint(30.6001d0*E) + F                        JCAL 27
        day = IDint(dayfrac)                                            JCAL 28
        dayfrac = dayfrac - day                                         JCAL 29
C...    Get month from parameter E                                      JCAL 30
        If (E.lt.2)Then                                                 JCAL 31
          Stop ' Bad month parameter E: too small'                      JCAL 32
        Else If (E.lt.14)Then                                           JCAL 33
          month = E - 1                                                 JCAL 34
        Else If (E.lt.16)Then                                           JCAL 35
          month = E - 13                                                JCAL 36
        Else                                                            JCAL 37
          Stop ' Bad month parameter E: too large'                      JCAL 38
        Endif                                                           JCAL 39
C...    Get year from parameter C                                       JCAL 40
        If (month.gt.2)Then                                             JCAL 41
          year = C - 4716                                               JCAL 42
        Else                                                            JCAL 43
          year = C - 4715                                               JCAL 44
        Endif                                                           JCAL 45
        Return                                                          JCAL 46
        End                                                             JCAL 47
C---------------------------------------------------------------------- JCAL 48
       Subroutine namemonth(month,monthname)                            NMMN  1
C                                                                       NMMN  2
C      Computes month name from given month number                      NMMN  3
C                                                                       NMMN  4
       Character*3 Months(12),monthname                                 NMMN  5
       Data Months/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',     NMMN  6
     &  'SEP','OCT','NOV','DEC'/                                        NMMN  7
       monthname = Months(month)                                        NMMN  8
       Return                                                           NMMN  9
       End                                                              NMMN 10
C---------------------------------------------------------------------- NMMN 11
      Subroutine Rescale(x)                                             RSCL  1
C...  Puts x into range 0 - 360                                         RSCL  2
      Double precision x                                                RSCL  3
      x = x/360.0d0 - Dint(x/360.0d0) + 1.0d0                           RSCL  4
      x = (x - Dint(x))*360.0d0                                         RSCL  5
      Return                                                            RSCL  6
      End                                                               RSCL  7
C---------------------------------------------------------------------- RSCL  8
      Subroutine Shiftdif(x)                                            SHFD  1
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
      Subroutine vensephm(xday,sunlat,sunlon,sunLsubs,radius,owlt,EOT)  VEPH  1
C...  Computes sunlat, sunlon= latitude and longitude of sub-solar      VEPH  2
C     point on the surface, sunLsubs= planetocentric longitude of Sun   VEPH  3
C     (Ls), radius= current orbital radius from Sun to Venus, heliolon= VEPH  4
C     Venus heliocentric longitude, owlt= Venus-Earth one-way light     VEPH  5
C     time (minutes), and EOT= equation of time (deg), calculated from  VEPH  6
C     Julian day and time, xday.  Notes: input xday is NOT UTC, but     VEPH  7
C     Terrestrial (Dynamical) Venus-Event Time (NOT Earth-Receive Time).VEPH  8
C     Venus Local Mean Solar Time (hrs ) = Local True Solar Time (hrs)  VEPH  9
C     minus EOT (in hrs). Output is for Terrestrial (Dynamical)         VEPH 10
C     Venus Event Time (corresponding to input xday).                   VEPH 11
C                                                                       VEPH 12
C     Equations for "moderately accurate" Venus solar time, seasonal    VEPH 13
C     parameters, and one-way Venus-Earth light time, from Allison and  VEPH 14
C     McEwen, Planet. Space Sci., 48, 215-235 (2000), and Allison       VEPH 15
C     Geophys Res. Lett., 24(16), 1967-1970 (1997).                     VEPH 16
C                                                                       VEPH 17
      Implicit None                                                     VEPH 18
      Double Precision xday,sunlat,sunlon,sunLsubs,radius,pi180,dt,     VEPH 19
     &  anomM,alphFMS,alsrad,helilon,alphs,EOT,pmr,gE,rE,dlat1,         VEPH 20
     &  helonE,Vm,owlt,yranom,anom0,yrtrop,veqlon0,perlon0,ecc,obl,     VEPH 21
     &  dlat2,dlat3,veqlon1,inc,anlon0,siday,rad0,ecc2,ecc3,ecc4,       VEPH 22
     &  ecc5,ecc6,Vm0,Vmday0,xE,yE,xpl,ypl,zpl,eqcenter,argper,         VEPH 23
     &  trueanom,coslat                                                 VEPH 24
      pi180 = Datan(1.0d0)/45.0d0                                       VEPH 25
C...  Days since 2000 January 1.5                                       VEPH 26
      dt = xday - 2451545.0d0                                           VEPH 27
C                                                                       VEPH 28
C.....................................................................  VEPH 29
C                                                                       VEPH 30
C...  Planetary orbit parameters                                        VEPH 31
C                                                                       VEPH 32
C     Semi-major axis (AU) = mean distance from Sun                     VEPH 33
      rad0 = 0.723330d0                                                 VEPH 34
C     Anomalistic year (days, perihelion-to-perihelion)                 VEPH 35
      yranom = 224.7d0                                                  VEPH 36
C     Tropical year (days, for rate of fictitious mean sun)             VEPH 37
      yrtrop = 224.6994d0                                               VEPH 38
C     Mean anomaly for J2000 (degrees)                                  VEPH 39
      anom0 = 50.4084d0                                                 VEPH 40
C     Heliocentric longitude of perihelion at J2000 (deg)               VEPH 41
      perlon0 = 131.5709d0                                              VEPH 42
C     Terms for heliocentric longitude at Ls=0 (deg)                    VEPH 43
      veqlon0 = 237.841d0                                               VEPH 44
      veqlon1 = 9.77d-6                                                 VEPH 45
C     Eccentricity and powers                                           VEPH 46
      ecc = 0.006773d0 - 1.302D-9*dt                                    VEPH 47
      ecc2 = ecc**2                                                     VEPH 48
      ecc3 = ecc2*ecc                                                   VEPH 49
      ecc4 = ecc3*ecc                                                   VEPH 50
      ecc5 = ecc4*ecc                                                   VEPH 51
      ecc6 = ecc5*ecc                                                   VEPH 52
C     Obliquity angle (radians)                                         VEPH 53
      obl = (177.36d0 + 0.0d0*dt)*pi180                                 VEPH 54
C     Inclination (radians)                                             VEPH 55
      inc = (3.3946d0 +2.75D-8*dt)*pi180                                VEPH 56
C     Longitude of ascending node at J2000 (deg)                        VEPH 57
      anlon0 = 76.6799d0                                                VEPH 58
C     Sidereal period of rotation (Earth days)                          VEPH 59
      siday = -243.02011d0                                              VEPH 60
C     Heliocentric lon of prime meridian (deg) at Julian day Vmday0     VEPH 61
      Vm0 = 75.1289d0                                                   VEPH 62
      Vmday0 = 2451545.0d0                                              VEPH 63
C     Difference terms, planetocentric to planetographic lat (deg)      VEPH 64
      dlat1 = -0.002d0                                                  VEPH 65
      dlat2 = 0.002d0                                                   VEPH 66
      dlat3 = 0.0d0                                                     VEPH 67
C                                                                       VEPH 68
C.....................................................................  VEPH 69
C                                                                       VEPH 70
C...  Mean anomaly (radians)                                            VEPH 71
C...  Allison & McEwen (2000) equation (16)                             VEPH 72
      anomM = (anom0 + (360.0d0/yranom)*dt)*pi180                       VEPH 73
C...  Right ascension of fictitious mean sun (deg)                      VEPH 74
C...  Allison & McEwen (2000) equation (17)                             VEPH 75
      alphFMS = perlon0 - veqlon0 + anom0 + (360.0d0/yrtrop)*dt         VEPH 76
C...  Venus equation of center, A&M eqn. (4) (degrees)                  VEPH 77
      eqcenter = ((2.0d0*ecc - 0.25d0*ecc3 +                            VEPH 78
     &  (5.0d0/96.0d0)*ecc5)*Dsin(anomM) +                              VEPH 79
     &  (1.25d0*ecc2 - (11.0d0/24.0d0)*ecc4 +                           VEPH 80
     &  (17.0d0/192.0d0)*ecc6)*Dsin(2.0d0*anomM) +                      VEPH 81
     &  ((13.0d0/12.0d0)*ecc3 - (43.0d0/63.0d0)*ecc5)*                  VEPH 82
     &  Dsin(3.0d0*anomM) + ((103.0d0/96.0d0)*ecc4 -                    VEPH 83
     &  (451.0d0/480.0d0)*ecc6)*Dsin(4.0d0*anomM) +                     VEPH 84
     &  ((1097.0d0/960.0d0)*ecc5)*Dsin(5.0d0*anomM) +                   VEPH 85
     &  ((12323.0d0/960.0d0)*ecc6)*Dsin(6.0d0*anomM))/pi180             VEPH 86
C...  True planetocentric solar longitude (Ls), A&M eqns. (2) and (4)   VEPH 87
      sunLsubs = alphFMS + eqcenter                                     VEPH 88
      Call Rescale(sunLsubs)                                            VEPH 89
C...  Ls angle in radians                                               VEPH 90
      alsrad = sunLsubs*pi180                                           VEPH 91
C...  Sub-solar latitude of sun (planetographic solar declination),     VEPH 92
C     Allison (1997) eqn. (5) with empirical Ls and 3*Ls terms          VEPH 93
      sunlat = DAsin(Sin(obl)*Dsin(alsrad))/pi180 + dlat1*Dsin(alsrad)  VEPH 94
     &  + dlat2*Dcos(alsrad) + dlat3*Dsin(3.0d0*alsrad)                 VEPH 95
C...  Solar right ascension, un-numbered equation, A&M page 217         VEPH 96
      alphs = Datan2(Dcos(obl)*dSin(alsrad),dCos(alsrad))/pi180         VEPH 97
C...  Venus orbital radius, Astronomical Almanac page E4                VEPH 98
      radius = rad0*(1.0d0 - ecc2)/(1.0d0 + ecc*DCos(anomM + alsrad -   VEPH 99
     &  alphFMS*pi180))                                                 VEPH100
C.... Approximate Venus heliocentric longitude, A&M eqn, (11)           VEPH101
      helilon = sunLsubs + veqlon0 - veqlon1*dt-(Dtan(0.5d0*inc)**2)*   VEPH102
     &  Dsin(2.0d0*(alsrad + (veqlon0 - anlon0)*pi180))/pi180           VEPH103
      Call Rescale(helilon)                                             VEPH104
C...  Equation of time (deg)                                            VEPH105
      EOT = alphFMS - alphs                                             VEPH106
      Call Rescale(EOT)                                                 VEPH107
      Call Shiftdif(EOT)                                                VEPH108
      Call Rescale(alphs)                                               VEPH109
C...  Earth heliocentric distance and longitude, Allison eqns (20)-     VEPH110
C     (22)                                                              VEPH111
      gE = (357.528d0 + 0.9856003d0*dt)*pi180                           VEPH112
      rE = 1.00014d0 - 0.01671d0*Dcos(gE) - 0.00014d0*Dcos(2.0d0*gE)    VEPH113
      helonE = 100.472d0 + 0.9856474d0*dt + 1.915d0*Dsin(gE)            VEPH114
     &  + 0.020d0*Dsin(2.0d0*gE)                                        VEPH115
C...  Earth Cartesian coordinates                                       VEPH116
      xE = rE*dCos(helonE*pi180)                                        VEPH117
      yE = rE*dSin(helonE*pi180)                                        VEPH118
C...  Venus true anolmaly (radians)                                     VEPH119
      trueanom = eqcenter*pi180 + anomM                                 VEPH120
C...  Venus argument of perihelion (radians)                            VEPH121
      argper = (54.8910d0 + 1.38374d-5*dt)*pi180                        VEPH122
C...  Venus Cartesian coordinates                                       VEPH123
      zpl = radius*dSin(trueanom + argper)*dSin(inc)                    VEPH124
      coslat = dSqrt(1.0d0 - (zpl/radius)**2)                           VEPH125
      xpl = radius*dCos((helilon+3.82394d-5*dt)*pi180)*coslat           VEPH126
      ypl = radius*dSin((helilon+3.82394d-5*dt)*pi180)*coslat           VEPH127
C...  One-way light time (minutes), Allison eqn.(19)                    VEPH128
      owlt = dSqrt((xpl-xE)**2+(ypl-yE)**2+zpl**2)*499.005d0/60.0d0     VEPH129
C...  Venus (Heliocentric) prime meridian, Allison eqn (11)             VEPH130
      Vm = Vm0 + (360.0d0/dAbs(siday))*(xday - Vmday0)                  VEPH131
C...  Sub-solar longitude from true solar time at prime meridian,       VEPH132
C     A&M page 217                                                      VEPH133
      pmr = (Vm - alphs)/360.0d0                                        VEPH134
      sunlon = (pmr - Dint(pmr))*360.0d0 + 180.0d0                      VEPH135
      Call Rescale(sunlon)                                              VEPH136
      Return                                                            VEPH137
      End                                                               VEPH138
C---------------------------------------------------------------------- VEPH139
      Subroutine SolZenAng(sunlat,sunlon,sitelat,sitelon,sza)           SZAN  1
C...  Solar zenith angle (sza, degrees) from latitude and longitude     SZAN  2
C     of sun and site (degrees)                                         SZAN  3
      Double Precision sunlat,sunlon,sitelat,sitelon,sza,pi180,csza     SZAN  4
      pi180 = dAtan(1.0d0)/45.0d0                                       SZAN  5
C...  Cosine of solar zenith angle                                      SZAN  6
      csza = dSin(sunlat*pi180)*dSin(sitelat*pi180) +                   SZAN  7
     &  dCos(sunlat*pi180)*dCos(sitelat*pi180)*                         SZAN  8
     &  dCos(pi180*(sunlon-sitelon))                                    SZAN  9
C...  Solar zenith angle                                                SZAN 10
      sza = dAcos(csza)/pi180                                           SZAN 11
      Return                                                            SZAN 12
      End                                                               SZAN 13
C---------------------------------------------------------------------- SZAN 14
