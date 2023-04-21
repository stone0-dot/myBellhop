module interface
    USE bellMod
    USE RefCoMod
    USE bdryMod
    USE angleMod
    USE SdRdRMod
    USE ArrMod
    USE BeamPatternMod
contains
    FUNCTION to_c_chars(fchars) result(c_chars)
        USE iso_c_binding, only: c_null_char, c_char
        IMPLICIT NONE
        character(len=*), intent(in) :: fchars
        character(len=1, kind=c_char), allocatable :: c_chars(:)

        integer :: i
        integer :: n

        n = len(fchars)
        allocate(c_chars(n + 1))
        do i = 1, n
            c_chars(i) = fchars(i:i)
        end do
        c_chars(n + 1) = c_null_char
    END FUNCTION to_c_chars
    !-----------------------------------------------------------
    SUBROUTINE delete_c_chars(c_chars) bind(C)
        USE iso_c_binding, only: c_null_char, c_char
        IMPLICIT NONE
        character(len=1, kind=c_char), allocatable :: c_chars(:)
        deallocate(c_chars)
    END SUBROUTINE delete_c_chars
    !-----------------------------------------------------------
    SUBROUTINE pass_complex_to_c(fcomplex, c_real, c_aimag)
        IMPLICIT NONE
        complex(8), intent(in) :: fcomplex
        real(4), intent(out) :: c_real, c_aimag
        c_real = real(fcomplex)
        c_aimag = aimag(fcomplex)
    END SUBROUTINE pass_complex_to_c
    !-----------------------------------------------------------
    SUBROUTINE readConfig( c_TITLE, freq, ISINGL, &
               NIMAGE, IBWIN, deltas, MaxN, zBox, rBox, EPMULT, RLOOP,  &
               c_TopOpt, DepthT, CPT_real, CPT_aimag, RHOT, c_BotOpt, DepthB, &
               CPB_real, CPB_aimag, RHOB, c_RunType, c_BeamType) bind(C)
        USE iso_c_binding, only: c_null_char, c_char
        IMPLICIT REAL (KIND=4) ( A-H, O-Z )
        character(len=1, kind=c_char), allocatable :: c_TITLE(:), c_TopOpt(:), c_BotOpt(:), c_RunType(:), c_BeamType(:)

        INTEGER, PARAMETER :: PRTFil = 6
        INTEGER :: ISINGL
        
        COMPLEX (KIND=8) ::   CPT, CPB
        real(4), intent(out) :: CPT_real, CPT_aimag, CPB_real, CPB_aimag
        CHARACTER    TITLE*80, TopOpt*5, BotOpt*3, RunType*4, BeamType*3
        
        CALL READIN( TITLE, freq, ISINGL, &
             NIMAGE, IBWIN, deltas, MaxN, zBox, rBox, EPMULT, RLOOP,  &
             TopOpt, DepthT, CPT, RHOT, BotOpt, DepthB, CPB, RHOB, RunType, BeamType )
        
        CALL READATI(  TopOpt(5:5), DepthT, rBox, PRTFil )   	! READ AlTImetry
        CALL READBTY(  BotOpt(2:2), DepthB, rBox, PRTFil )      ! READ BaThYmetrY
        CALL READRC(   BotOpt(1:1), TopOpt(2:2),  PRTFil ) 	! READ Reflection Coefficients (top and bottom)
        CALL READPAT( RunType(3:3),               PRTFil )      ! Read Source Beam Pattern

        c_TITLE = to_c_chars(TITLE)
        c_TopOpt = to_c_chars(TopOpt)
        c_BotOpt = to_c_chars(BotOpt)
        c_RunType = to_c_chars(RunType)
        c_BeamType = to_c_chars(BeamType)

        CALL pass_complex_to_c(CPT, CPT_real, CPT_aimag)
        CALL pass_complex_to_c(CPB, CPB_real, CPB_aimag)
    end SUBROUTINE readConfig
end module interface