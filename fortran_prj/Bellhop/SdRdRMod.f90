MODULE SdRdRMod
   ! Reads in source depths, receiver depths, and receiver ranges

   IMPLICIT NONE
   SAVE

   INTEGER NSD, NRD, NR
   INTEGER, ALLOCATABLE :: ISD( : ), IRD( : )
   REAL,    ALLOCATABLE ::  SD( : ),  RD( : ), WS( : ), WR( : ), R( : )

CONTAINS

   SUBROUTINE CSDRD( ENVFIL, PRTFIL, ZMIN, ZMAX, c_SD, c_SD_len,&
      c_RD, c_RD_len, c_NSD, c_NRD)

      USE iso_c_binding
      IMPLICIT NONE
      INTEGER ENVFIL, PRTFIL, IS, IR, IAllocStat
      REAL ZMIN, ZMAX
      type(c_ptr), intent(in) :: c_SD, c_RD
      integer(4), intent(in) :: c_NSD, c_NRD, c_SD_len, c_RD_len
      real(4), pointer :: f_SD(:), f_RD(:)

      !     *** Read source depths ***
      NSD = c_NSD
      IF ( NSD <= 0 ) THEN
         WRITE( PRTFIL, * ) ' NSD = ', NSD
         CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Number of sources must be positive'  )
      ENDIF

      IF ( ALLOCATED( SD ) ) DEALLOCATE( SD, WS, ISD )
      ALLOCATE( SD( MAX( 3, NSD ) ), WS( NSD ), ISD( NSD ), Stat = IAllocStat )
      IF ( IAllocStat /= 0 ) THEN
         WRITE( PRTFIL, * ) 'NSD = ', NSD
         CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Too many sources'  )
      END IF

      SD( 3 ) = -999.9
      call c_f_pointer(c_SD, f_SD, [c_SD_len])
      SD(1:c_SD_len) = f_SD(1:c_SD_len)

      CALL SUBTAB( SD, NSD )
      CALL SORT(   SD, NSD )

      WRITE( PRTFIL, * )
      WRITE( PRTFIL, * ) 'Number of sources   = ', NSD
      IF ( NSD >= 1  ) WRITE( PRTFIL, "( 5G14.6 )" ) ( SD( IS ), IS = 1, MIN( NSD, 51 ) )
      IF ( NSD > 51 ) WRITE( PRTFIL, * ) ' ... ', SD( NSD )

      ! *** Read receiver depths ***

      NRD = c_NRD

      IF ( NRD <= 0 ) THEN
         WRITE( PRTFIL, * ) ' NRD = ', NRD
         CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Number of receivers must be positive'  )
      ENDIF

      IF ( ALLOCATED( RD ) ) DEALLOCATE( RD, WR, IRD )
      ALLOCATE( RD( MAX( 3, NRD ) ), WR( NRD ), IRD( NRD ), Stat = IAllocStat  )
      IF ( IAllocStat /= 0 ) THEN
         WRITE( PRTFIL, * ) 'NRD = ', NRD
         CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Too many receivers'  )
      END IF

      RD( 3 ) = -999.9
      call c_f_pointer(c_RD, f_RD, [c_RD_len])
      RD(1:c_RD_len) = f_RD(1:c_RD_len)

      CALL SUBTAB( RD, NRD )
      CALL SORT(   RD, NRD )

      WRITE( PRTFIL, * )
      WRITE( PRTFIL, * ) 'Number of receivers = ', NRD
      IF ( NRD >= 1 ) WRITE( PRTFIL, "( 5G14.6 )" ) ( RD( IR ), IR = 1, MIN( NRD, 51 ) )
      IF ( NRD > 51 ) WRITE( PRTFIL, * ) ' ... ', RD( NRD )

      ! *** Check for SD/RD in upper or lower halfspace ***

      DO IS = 1, NSD
         IF      ( SD( IS ) < ZMIN ) THEN
            SD( IS ) = ZMIN
            CALL ERROUT( PRTFil, 'W', 'SdRdRMod', 'Source above the top bdry has been moved down' )
         ELSE IF ( SD( IS ) > ZMAX ) THEN
            SD( IS ) = ZMAX
            CALL ERROUT( PRTFil, 'W', 'SdRdRMod', 'Source below the bottom bdry has been moved up' )
         ENDIF
      END DO

      DO IR = 1, NRD
         IF      ( RD( IR ) < ZMIN ) THEN
            RD( IR ) = ZMIN
         ELSE IF ( RD( IR ) > ZMAX ) THEN
            RD( IR ) = ZMAX
         ENDIF
      END DO

      RETURN
   END SUBROUTINE CSDRD
!****************************************************************!
   SUBROUTINE SDRD( ENVFIL, PRTFIL, ZMIN, ZMAX )

      IMPLICIT NONE
      INTEGER ENVFIL, PRTFIL, IS, IR, IAllocStat
      REAL ZMIN, ZMAX

      !     *** Read source depths ***

      READ( ENVFIL, * ) NSD

      IF ( NSD <= 0 ) THEN
         WRITE( PRTFIL, * ) ' NSD = ', NSD
         CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Number of sources must be positive'  )
      ENDIF

      IF ( ALLOCATED( SD ) ) DEALLOCATE( SD, WS, ISD )
      ALLOCATE( SD( MAX( 3, NSD ) ), WS( NSD ), ISD( NSD ), Stat = IAllocStat )
      IF ( IAllocStat /= 0 ) THEN
         WRITE( PRTFIL, * ) 'NSD = ', NSD
         CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Too many sources'  )
      END IF

      SD( 3 ) = -999.9
      READ( ENVFIL, * ) ( SD( IS ), IS = 1, NSD )

      CALL SUBTAB( SD, NSD )
      CALL SORT(   SD, NSD )

      WRITE( PRTFIL, * )
      WRITE( PRTFIL, * ) 'Number of sources   = ', NSD
      IF ( NSD >= 1  ) WRITE( PRTFIL, "( 5G14.6 )" ) ( SD( IS ), IS = 1, MIN( NSD, 51 ) )
      IF ( NSD > 51 ) WRITE( PRTFIL, * ) ' ... ', SD( NSD )

      ! *** Read receiver depths ***

      READ( ENVFIL, * ) NRD

      IF ( NRD <= 0 ) THEN
         WRITE( PRTFIL, * ) ' NRD = ', NRD
         CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Number of receivers must be positive'  )
      ENDIF

      IF ( ALLOCATED( RD ) ) DEALLOCATE( RD, WR, IRD )
      ALLOCATE( RD( MAX( 3, NRD ) ), WR( NRD ), IRD( NRD ), Stat = IAllocStat  )
      IF ( IAllocStat /= 0 ) THEN
         WRITE( PRTFIL, * ) 'NRD = ', NRD
         CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Too many receivers'  )
      END IF

      RD( 3 ) = -999.9
      READ( ENVFIL, * ) ( RD( IR ), IR = 1, NRD )

      CALL SUBTAB( RD, NRD )
      CALL SORT(   RD, NRD )

      WRITE( PRTFIL, * )
      WRITE( PRTFIL, * ) 'Number of receivers = ', NRD
      IF ( NRD >= 1 ) WRITE( PRTFIL, "( 5G14.6 )" ) ( RD( IR ), IR = 1, MIN( NRD, 51 ) )
      IF ( NRD > 51 ) WRITE( PRTFIL, * ) ' ... ', RD( NRD )

      ! *** Check for SD/RD in upper or lower halfspace ***

      DO IS = 1, NSD
         IF      ( SD( IS ) < ZMIN ) THEN
            SD( IS ) = ZMIN
            CALL ERROUT( PRTFil, 'W', 'SdRdRMod', 'Source above the top bdry has been moved down' )
         ELSE IF ( SD( IS ) > ZMAX ) THEN
            SD( IS ) = ZMAX
            CALL ERROUT( PRTFil, 'W', 'SdRdRMod', 'Source below the bottom bdry has been moved up' )
         ENDIF
      END DO

      DO IR = 1, NRD
         IF      ( RD( IR ) < ZMIN ) THEN
            RD( IR ) = ZMIN
         ELSE IF ( RD( IR ) > ZMAX ) THEN
            RD( IR ) = ZMAX
         ENDIF
      END DO

      RETURN
   END SUBROUTINE SDRD

   !********************************************************************
   SUBROUTINE CRANGES( ENVFIL, PRTFIL, c_R, c_R_len, c_NR)

      ! *** Read receiver ranges ***
      USE iso_c_binding
      IMPLICIT NONE
      INTEGER ENVFIL, PRTFIL, IR, IAllocStat
      type(c_ptr), intent(in) :: c_R
      integer(4), intent(in) :: c_NR, c_R_len
      real(4), pointer :: f_R(:)

      NR = c_NR
      WRITE( PRTFIL, * ) 'Number of ranges   = ', NR

      IF ( ALLOCATED( R ) ) DEALLOCATE( R )
      ALLOCATE( R( MAX( 3, NR ) ), Stat = IAllocStat )
      IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFIL, 'F', 'RANGES', 'Too many range points' )

      R( 3 ) = -999.9
      call c_f_pointer(c_R, f_R, [c_R_len])
      R(1:c_R_len) = f_R(1:c_R_len)

      CALL SUBTAB( R, NR )
      CALL SORT(   R, NR )

      WRITE( PRTFIL, "( 5G14.6 )" ) ( R( IR ), IR = 1, MIN( NR, 51 ) )
      IF ( NR > 51 ) WRITE( PRTFIL, * ) ' ... ', R( NR )

      R( 1:NR ) = 1000.0 * R( 1:NR )   ! Convert ranges to meters

      ! For a point source can't have receiver at origin
      ! IF ( OPT(1:1) == 'R' .AND. R( 1 ) <= 0.0 )

      !IF ( R( 1 ) <= 0.0 ) R( 1 ) = MIN( 1.0, R( 2 ) )

      RETURN
   END SUBROUTINE Cranges
   !********************************************************************
   SUBROUTINE RANGES( ENVFIL, PRTFIL)

      ! *** Read receiver ranges ***

      IMPLICIT NONE
      INTEGER ENVFIL, PRTFIL, IR, IAllocStat

      READ(  ENVFIL, * ) NR
      WRITE( PRTFIL, * )
      WRITE( PRTFIL, * ) 'Number of ranges   = ', NR

      IF ( ALLOCATED( R ) ) DEALLOCATE( R )
      ALLOCATE( R( MAX( 3, NR ) ), Stat = IAllocStat )
      IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFIL, 'F', 'RANGES', 'Too many range points' )

      R( 3 ) = -999.9
      READ( ENVFIL, * ) ( R( IR ), IR = 1, NR )

      CALL SUBTAB( R, NR )
      CALL SORT(   R, NR )

      WRITE( PRTFIL, "( 5G14.6 )" ) ( R( IR ), IR = 1, MIN( NR, 51 ) )
      IF ( NR > 51 ) WRITE( PRTFIL, * ) ' ... ', R( NR )

      R( 1:NR ) = 1000.0 * R( 1:NR )   ! Convert ranges to meters

      ! For a point source can't have receiver at origin
      ! IF ( OPT(1:1) == 'R' .AND. R( 1 ) <= 0.0 )

      !IF ( R( 1 ) <= 0.0 ) R( 1 ) = MIN( 1.0, R( 2 ) )

      RETURN
   END SUBROUTINE ranges

END MODULE SdRdRMod
