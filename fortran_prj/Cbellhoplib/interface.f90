module interface
    type GrowthDoubleVector
        integer(4) :: eleNum
        real(8), pointer :: vector(:)
    end type
contains
    SUBROUTINE to_c_chars(fchars, c_chars)
        USE iso_c_binding, only: c_null_char, c_char
        IMPLICIT NONE
        character(len=*), intent(in) :: fchars
        character(len=1, kind=c_char), pointer :: c_chars(:)

        integer :: i
        integer :: n

        n = len(fchars)
        allocate(c_chars(n + 1))
        do i = 1, n
            c_chars(i) = fchars(i:i)
        end do
        c_chars(n + 1) = c_null_char
    END SUBROUTINE to_c_chars
    !-----------------------------------------------------------
    SUBROUTINE delete_c_chars(c_chars) bind(C)
        USE iso_c_binding, only: c_null_char, c_char
        IMPLICIT NONE
        character(len=1, kind=c_char), pointer :: c_chars(:)
        deallocate(c_chars)
    END SUBROUTINE delete_c_chars
    !-----------------------------------------------------------
    SUBROUTINE pass_complex_to_c(fcomplex, c_real, c_aimag)
        IMPLICIT NONE
        complex(8), intent(in) :: fcomplex
        real(8), intent(out) :: c_real, c_aimag
        c_real = real(fcomplex)
        c_aimag = aimag(fcomplex)
    END SUBROUTINE pass_complex_to_c
    !-----------------------------------------------------------
    SUBROUTINE readConfig( c_TITLE, c_freq, c_ISINGL, &
               c_NIMAGE, c_IBWIN, c_deltas, c_MaxN, c_zBox, c_rBox, c_EPMULT, c_RLOOP,  &
               c_TopOpt, c_DepthT, CPT_real, CPT_aimag, c_RHOT, c_BotOpt, c_DepthB, &
               CPB_real, CPB_aimag, c_RHOB, c_RunType, c_BeamType) bind(C)
        
        USE iso_c_binding, only: c_null_char, c_char
        USE bellMod
        USE RefCoMod
        USE bdryMod
        USE angleMod
        USE SdRdRMod
        USE ArrMod
        USE BeamPatternMod

        CHARACTER TITLE*80, BotOpt*3, RunType*4, BeamType*3
        character(len=1, kind=c_char), pointer :: c_TITLE(:), c_TopOpt(:), c_BotOpt(:), c_RunType(:), c_BeamType(:)

        INTEGER, intent(out) :: c_ISINGL, c_NIMAGE, c_IBWIN, c_MaxN
        real(4), intent(out) :: c_freq, c_deltas, c_zBox, c_rBox, c_EPMULT, c_RLOOP,&
                                c_DepthT, c_DepthB
        
        real(8), intent(out) :: CPT_real, CPT_aimag, CPB_real, CPB_aimag, c_RHOT, c_RHOB
        
        CALL READIN( TITLE, freq, ISINGL, &
             NIMAGE, IBWIN, deltas, MaxN, zBox, rBox, EPMULT, RLOOP,  &
             TopOpt, DepthT, CPT, RHOT, BotOpt, DepthB, CPB, RHOB, RunType, BeamType)
        
        CALL READATI(  TopOpt(5:5), DepthT, rBox, PRTFil )   	! READ AlTImetry
        CALL READBTY(  BotOpt(2:2), DepthB, rBox, PRTFil )      ! READ BaThYmetrY
        CALL READRC(   BotOpt(1:1), TopOpt(2:2),  PRTFil ) 	! READ Reflection Coefficients (top and bottom)
        CALL READPAT( RunType(3:3),               PRTFil )      ! Read Source Beam Pattern

        c_ISINGL = ISINGL
        c_NIMAGE = NIMAGE
        c_IBWIN = IBWIN
        c_MaxN = MaxN
        c_freq = freq
        c_deltas = deltas
        c_zBox = zBox
        c_rBox = rBox
        c_EPMULT = EPMULT
        c_RLOOP = RLOOP
        c_DepthT = DepthT
        c_DepthB = DepthB

        CALL to_c_chars(TITLE, c_TITLE)
        CALL to_c_chars(TopOpt, c_TopOpt)
        CALL to_c_chars(BotOpt, c_BotOpt)
        CALL to_c_chars(RunType, c_RunType)
        CALL to_c_chars(BeamType, c_BeamType)

        CALL pass_complex_to_c(CPT, CPT_real, CPT_aimag)
        CALL pass_complex_to_c(CPB, CPB_real, CPB_aimag)
    end SUBROUTINE readConfig
    !--------------------------------------------------------------
    SUBROUTINE get_c_chars_len(c_chars, length)
        USE iso_c_binding, only: c_null_char, c_char
        IMPLICIT NONE
        character(len=1, kind=c_char), target, intent(in) :: c_chars(:)
        integer(4), intent(out) :: length
        length = 0
        do
            if(c_chars(length + 1) == c_null_char) exit
            length = length + 1
        end do
    END SUBROUTINE get_c_chars_len
    !--------------------------------------------------------------
    SUBROUTINE pass_c_chars_to_fchars(c_chars, fchars, length)
        USE iso_c_binding, only: c_null_char, c_char
        IMPLICIT NONE
        integer(4), intent(in) :: length
        character(len=1, kind=c_char), target, intent(in) :: c_chars(:)
        character(len=:), allocatable :: fchars
        integer :: i
        allocate(character(len=length) :: fchars)
        do i = 1, length
            fchars(i:i) = c_chars(i)
        end do
    END SUBROUTINE pass_c_chars_to_fchars
    !--------------------------------------------------------------
    FUNCTION create_growth_double_vector(capacity)
        integer(4) :: capacity
        type(GrowthDoubleVector) :: create_growth_double_vector
        create_growth_double_vector%eleNum = 0
        allocate(create_growth_double_vector%vector(capacity))
    END FUNCTION
    !--------------------------------------------------------------
    SUBROUTINE delete_growth_double_vector(vector_ptr) bind(C)
        real(8), pointer :: vector_ptr(:)
        deallocate(vector_ptr)
    END SUBROUTINE
    !--------------------------------------------------------------
    SUBROUTINE append(vector1, vector2, length)
        USE iso_c_binding
        type(GrowthDoubleVector), intent(inout):: vector1
        real(c_double), allocatable, intent(in) :: vector2(:)
        integer(4), intent(in) :: length
        real(c_double), allocatable :: tmp(:)
        do 
            if(vector1%eleNum + size(vector2) < size(vector1%vector)) then
                exit
            else
                if(vector1%eleNum /= 0) then
                    allocate(tmp(vector1%eleNum))
                    tmp(1:vector1%eleNum) = vector1%vector(1:vector1%eleNum)
                    allocate(vector1%vector(2*size(vector1%vector)))
                    vector1%vector(1:vector1%eleNum) = tmp(1:vector1%eleNum)
                else
                    allocate(vector1%vector(2*size(vector1%vector)))
                end if
            end if
        end do
        vector1%vector(vector1%eleNum+1:vector1%eleNum+length) = vector2(1:length)
        vector1%eleNum = vector1%eleNum + length
    END SUBROUTINE append
    !--------------------------------------------------------------
    SUBROUTINE delete_c_line_length(c_line_length) bind(C)
        integer(4), pointer :: c_line_length(:)
        deallocate(c_line_length)
    END SUBROUTINE delete_c_line_length
    !--------------------------------------------------------------
    SUBROUTINE caculate(c_freq, c_ISINGL, c_NIMAGE, c_IBWIN, c_deltas, c_MaxN, c_zBox, c_rBox, &
                        c_EPMULT, c_RLOOP,c_TopOpt, c_DepthT, CPT_real, CPT_aimag, c_RHOT, &
                        c_BotOpt, c_DepthB, CPB_real, CPB_aimag, c_RHOB, c_RunType, c_BeamType,&
                        c_line_length, c_xv_result) bind(C)
        USE iso_c_binding, only: c_null_char, c_char
        USE bellMod
        USE RefCoMod
        USE bdryMod
        USE angleMod
        USE SdRdRMod
        USE ArrMod
        USE BeamPatternMod
        character(len=1, kind=c_char), intent(in) :: c_TopOpt(5), c_BotOpt(3), c_RunType(4), c_BeamType(3)
        real(8) :: CPT_real, CPT_aimag, CPB_real, CPB_aimag
        real(4), intent(in) :: c_freq, c_deltas, c_zBox, c_rBox, c_EPMULT, c_RLOOP, c_DepthT, c_DepthB
        real(8), intent(in) :: c_RHOT, c_RHOB
        integer(4), intent(in) :: c_ISINGL, c_NIMAGE, c_IBWIN, c_MaxN
        COMPLEX,  ALLOCATABLE ::   U( :, : )
        INTEGER, PARAMETER    :: SHDFIL = 25, RAYFIL = 21, ArrivalsStorage = 20000000
        REAL,    PARAMETER    :: DegRad = pi / 180.0
        INTEGER   IBPvec( 1 )
        REAL      xs( 2 ), gradc( 2 )
        COMPLEX   EPS, PICKEPS
        CHARACTER BotOpt*3, RunType*4, BeamType*3
        real(8), allocatable :: xv_1D(:)
        integer(4), pointer, intent(out) :: c_line_length(:)
        type(GrowthDoubleVector) :: xv_result
        real(8), pointer, intent(out) :: c_xv_result(:)
        integer i

        CPT = COMPLEX(CPT_real, CPT_aimag)
        CPB = COMPLEX(CPB_real, CPB_aimag)
        freq = c_freq
        deltas = c_deltas
        zBox = c_zBox
        rBox = c_rBox
        EPMULT = c_EPMULT
        RLOOP = c_RLOOP
        DepthT = c_DepthT
        DepthB = c_DepthB
        RHOT = c_RHOT
        RHOB = c_RHOB
        ISINGL = c_ISINGL
        NIMAGE = c_NIMAGE
        IBWIN = c_IBWIN

        do i = 1, 5
            TopOpt(i:i) = c_TopOpt(i)
        end do
        do i = 1, 3
            BotOpt(i:i) = c_BotOpt(i)
            BeamType(i:i) = c_BeamType(i)
        end do
        do i = 1, 4
            RunType(i:i) = c_RunType(i)
        end do
        ! CALL pass_c_chars_to_fchars(c_TopOpt, TopOpt, 5)
        ! CALL pass_c_chars_to_fchars(c_BotOpt, BotOpt, 3)
        ! CALL pass_c_chars_to_fchars(c_RunType, RunType, 4)
        ! CALL pass_c_chars_to_fchars(c_BeamType, BeamType, 3)
        
        IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) THEN
            ALLOCATE ( U( Nrd, Nr ), Stat = IAllocStat )
            IF ( IAllocStat /= 0 ) &
                CALL ERROUT( PRTFIL, 'F', 'BELLHOP', 'Insufficient memory&
                for TL matrix: reduce Nr * Nrd'  )
        ELSE
            ALLOCATE ( U( 1, 1 ), Stat = IAllocStat )
        ENDIF
       
        IF ( SCAN( 'Aa', RunType(1:1) ) /= 0 ) THEN
            MaxNArr = MAX( ArrivalsStorage / ( Nrd * Nr ), 10 )   ! allow space for at least 10 arrivals
            WRITE( PRTFIL, * )
            WRITE( PRTFIL, * ) '( Maximum # of arrivals = ', MaxNArr, ')'
       
            ALLOCATE ( AArr( Nrd, Nr, MaxNArr ), PhaseArr( Nrd, Nr, MaxNArr ), DelArr( Nrd, Nr, MaxNArr ), &
                 SrcAngArr( Nrd, Nr, MaxNArr ), RcvrAngArr( Nrd, Nr, MaxNArr ), &
                 NArr( Nrd, Nr ), NTopBncArr( Nrd, Nr, MaxNArr ), NBotBncArr( Nrd, Nr, MaxNArr ), Stat = IAllocStat )
            IF ( IAllocStat /= 0 ) &
                 CALL ERROUT( PRTFIL, 'F', 'BELLHOP', &
                 'Insufficient memory to allocate arrivals matrix; reduce &
                 parameter ArrivalsStorage' )
        ELSE
            MaxNArr = 1
            ALLOCATE ( AArr( Nrd, Nr, 1 ), PhaseArr( Nrd, Nr, 1 ), DelArr( Nrd, Nr, 1 ), &
                 SrcAngArr( Nrd, Nr, 1 ), RcvrAngArr( Nrd, Nr, 1 ), &
                 NArr( Nrd, Nr ), NTopBncArr( Nrd, Nr, 1 ), NBotBncArr( Nrd, Nr, 1 ), Stat = IAllocStat )
        END IF
       
        omega  = 2.0 * pi * freq
       
        IF ( Nr > 1 ) THEN
            DeltaR = r( Nr ) - r( Nr - 1 )
        ELSE
            DeltaR = 0.0
        ENDIF
       
        alpha = DegRad * alpha   ! convert to radians
        Dalpha = 0.0
        IF ( NBeams /= 1 ) Dalpha = ( alpha( NBeams ) - alpha( 1 ) ) / ( NBeams - 1 )  ! angular spacing between beams
       
         ! *** Loop over source depths ***
       
        DO IS = 1, Nsd
            xs = (/ 0.0, sd( IS ) /)   ! source coordinate
       
            IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) U = 0.0    ! For a TL run, zero out pressure matrix
            IF ( SCAN( 'Aa',  RunType(1:1) ) /= 0 ) NArr = 0   ! For an arrivals run, zero out arrival matrix
       
            CALL SSP( xs, C, gradc, crr, crz, czz, TopOpt, 'TAB' )
       
            RadMax = 10 * C / freq  ! 10 wavelength max radius
       
            ! Are there enough beams?
            DalphaOpt = SQRT( C / ( 6.0 * freq * r( Nr ) ) )
            NBeamsOpt = 2 + ( alpha( NBeams ) - alpha( 1 ) ) / DalphaOpt
       
            IF ( RunType(1:1) == 'C' .AND. NBeams < NBeamsOpt ) THEN
               CALL ERROUT( PRTFIL, 'W', 'BELLHOP', 'Too few beams' )
               WRITE( PRTFIL, * ) 'NBeams should be at least = ', NBeamsOpt
            ENDIF
       
            ! *** Trace successive beams ***
            allocate(c_line_length(NBeams + 1))
            c_line_length(1) = NBeams
            xv_result = create_growth_double_vector(50000)
            DO ibeam = 1, NBeams
       
       
                IF ( ISINGL == 0 .OR. ibeam == ISINGL ) THEN    ! Single beam run?
       
                    alpha0 = alpha( ibeam ) * 180.0 / pi   ! take-off angle in degrees
                    IBPvec = maxloc( SrcBmPat( :, 1 ), mask = SrcBmPat( :, 1 ) < alpha0 )       ! index of ray angle in beam pattern
                    IBP    = IBPvec( 1 )
                    IBP = MAX( IBP, 1 )               ! don't go before beginning of table
                    IBP = MIN( IBP, NSBPPts - 1 )     ! don't go past end of table
                    ! linear interpolation to get amplitudeIsegBot( CrossBot ) )'
                    s = ( alpha0 - SrcBmPat( IBP, 1 ) ) / ( SrcBmPat( IBP + 1, 1 ) - SrcBmPat( IBP, 1 ) )
                    Amp0 = ( 1 - S ) * SrcBmPat( IBP, 2 ) + S * SrcBmPat( IBP + 1, 2 )
       
                    WRITE( *, * ) 'Tracing beam ', ibeam, alpha0
                    CALL TRACE( deltas, xs, alpha( ibeam ), Amp0, BeamType, zBox, rBox, BotOpt, RunType)   ! *** Trace a ray ***
       
                    IF ( RunType(1:1) == 'R' ) THEN     ! Write the ray trajectory to RAYFIL
                        CALL WRTRAY( alpha0, xv, N2, Trayv, Nsteps, NumTopBnc( Nsteps ), NumBotBnc( Nsteps ), DepthT, DepthB )
                        ! allocate(xv_1D(2*N2))
                        xv_1D = [xv(:,1:N2)]
                        call append(xv_result, xv_1D, 2*N2)
                        c_line_length(ibeam + 1) = 2*N2
                        ! deallocate(xv_1D)
                    ELSE                                ! *** Compute the contribution to the field ***
                        Eps = PICKEPS( BeamType(1:1), omega, C, CZ, alpha( ibeam ), Dalpha, RLOOP, EPMULT ) ! 'optimal' beam constant
                        SELECT CASE ( RunType(2:2) )
                            CASE ( 'R' )
                                IBWIN2 = IBWIN **2
                                CALL INFLUR(   U, DeltaR, Eps, alpha( IBeam ), NImage, IBWin2, RunType, RadMax, BeamType )
                            CASE ( 'C' )
                                IBWIN2 = IBWIN **2
                                CALL INFLUC(   U, DeltaR, Eps, alpha( IBeam ), NImage, IBWin2, RunType, RadMax, BeamType )
                            CASE ( 'S' )
                                CALL INFLUSGB( U,  sd( IS ), alpha( IBeam ), RunType, Dalpha, deltas )
                            CASE ( 'B' )
                                CALL INFLUGRB( U,  sd( IS ), alpha( IBeam ), RunType, Dalpha )
                            CASE DEFAULT
                                CALL INFLUG(   U,  sd( IS ), alpha( IBeam ), RunType, Dalpha )
                        END SELECT
                    END IF
               END IF
            END DO ! Next beam

            c_xv_result => xv_result%vector
       
            ! *** write results to disk ***
       
            IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) THEN   ! TL calculation
                CALL SCALEP( Dalpha, cV( 1 ), R, U, Nrd, Nr, RunType, TopOpt, freq )
                IRec  = 6 + Nrd * ( IS - 1 )
                DO I = 1, Nrd
                    IRec = IRec + 1
                    WRITE( SHDFil, REC = IRec ) ( U( I, J ), J = 1, Nr )
                END DO
       
            ELSE IF ( RunType(1:1) == 'A' ) THEN   ! arrivals calculation, ascii
               CALL WRTARRASC( R, Nrd, Nr, TopOpt, freq, RunType(4:4) )
            ELSE IF ( RunType(1:1) == 'a' ) THEN   ! arrivals calculation, binary
               CALL WRTARRBIN( R, Nrd, Nr, TopOpt, freq, RunType(4:4) )
            END IF
       
        END DO    ! Next source depth
       
         ! close all files
       
        IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) THEN   ! TL calculation
            CLOSE( SHDFIL )
        ELSE IF ( RunType(1:1) == 'A' ) THEN   ! arrivals calculation, ascii
            CLOSE( ARRFIL )
        ELSE IF ( RunType(1:1) == 'a' ) THEN   ! arrivals calculation, binary
            CLOSE( ARRFIL )
        ELSE IF ( RunType(1:1) == 'R' ) THEN
            CLOSE( RAYFIL )
        END IF
       
         ! Display run time
       
        CALL CPU_TIME( Tstop )
        WRITE( PRTFIL, "( /, ' CPU Time = ', G15.3 )" ) Tstop - Tstart
    END SUBROUTINE caculate
end module interface