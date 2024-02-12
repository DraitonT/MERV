        Real lat1,lat2,lon1,lon2                                        BLDT  1
        Character*60 filename                                           BLDT  2
        filename = 'TRAJDATA.txt'                                       BLDT  3
        Open(21,file=filename)                                          BLDT  4
        Write(*,*)' Enter z1,z2,dz (Real km)'                           BLDT  5
        Read(*,*)z1,z2,dz                                               BLDT  6
        If (dz.eq.0.0)Then                                              BLDT  7
          z2=z1                                                         BLDT  8
          dz = 1.                                                       BLDT  9
        Endif                                                           BLDT 10
        dz = sign(dz,z2-z1)                                             BLDT 11
        numz = Int(1.5+Abs((z2-z1)/dz))                                 BLDT 12
        Write(*,*)' Enter lat1,lat2,dlat (Real deg.)'                   BLDT 13
        Read(*,*)lat1,lat2,dlat                                         BLDT 14
        If (Abs(lat1).gt.90.)lat1 = sign(90.,lat1)                      BLDT 15
        If (Abs(lat2).gt.90.)lat2 = sign(90.,lat2)                      BLDT 16
        If (dlat.eq.0.0)Then                                            BLDT 17
          lat2=lat1                                                     BLDT 18
          dlat = 1.                                                     BLDT 19
        Endif                                                           BLDT 20
        dlat = sign(dlat,lat2-lat1)                                     BLDT 21
        numlat = Int(1.5+Abs((lat2-lat1)/dlat))                         BLDT 22
        Write(*,*)' Enter lon1,lon2,dlon (Real deg.)'                   BLDT 23
        Read(*,*)lon1,lon2,dlon                                         BLDT 24
        If (Abs(lon1).gt.360.)lon1 = sign(360.,lon1)                    BLDT 25
        If (Abs(lon2).gt.360.)lon2 = sign(360.,lon2)                    BLDT 26
        If (dlon.eq.0.0)Then                                            BLDT 27
          lon2=lon1                                                     BLDT 28
          dlon = 1.                                                     BLDT 29
        Endif                                                           BLDT 30
        dlon = sign(dlon,lon2-lon1)                                     BLDT 31
        numlon = Int(1.5+Abs((lon2-lon1)/dlon))                         BLDT 32
        Write(*,*)' Enter time increment (Real sec)'                    BLDT 33
        Read(*,*)dt                                                     BLDT 34
        time = 0.                                                       BLDT 35
        ntime = 0                                                       BLDT 36
        Write(*,9)numz,numlat,numlon,numz*numlat*numlon                 BLDT 37
   9    Format(' Number height, lat, lon, total =',3I5,I7)              BLDT 38
        Do 12 iz = 1,numz                                               BLDT 39
          z = z1 + (iz-1)*dz                                            BLDT 40
        Do 11 ilat = 1,numlat                                           BLDT 41
          xlat = lat1 + (ilat-1)*dlat                                   BLDT 42
        Do 10 ilon = 1,numlon                                           BLDT 43
          xlon = lon1 + (ilon-1)*dlon                                   BLDT 44
          ntime = ntime + 1                                             BLDT 45
          time = ntime*dt                                               BLDT 46
          Write(21,20)time,z,xlat,xlon                                  BLDT 47
  10    Enddo                                                           BLDT 48
  11    Enddo                                                           BLDT 49
  12    Enddo                                                           BLDT 50
  20    Format(4F10.2)                                                  BLDT 51
        Write(*,*)' Data written to file ',filename                     BLDT 52
        End                                                             BLDT 53
