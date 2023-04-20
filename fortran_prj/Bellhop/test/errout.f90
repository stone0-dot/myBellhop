!     Last change:  MBP  23 Jun 2002    2:44 pm
 SUBROUTINE ERROUT( PRTFIL, SEVRTY, WHERE, ErrMsg )

!     Outputs an error message

!     PRTFIL unit number for print-out
!     SEVRTY:
!        'W' Warning
!        'F' Fatal
!     WHERE  in which program or subroutine
!     ErrMsg error message

 IMPLICIT NONE
 INTEGER PRTFIL, Last
 CHARACTER SEVRTY *1, WHERE *(*), ErrMsg *(*)

 WRITE( PRTFIL, * )

 SELECT CASE ( SEVRTY )  !     *** Severity ***
  CASE ( 'W' )
     WRITE( PRTFIL, * ) '*** WARNING ***'
  CASE ( 'F' )
     WRITE( PRTFIL, * ) '*** FATAL ERROR ***'
  CASE DEFAULT
     WRITE( PRTFIL, * ) '*** FATAL ERROR ***'
     WRITE( PRTFIL, * ) 'Error handler (ERROUT) called with unknown severity level: ', SEVRTY
 END SELECT

 Last = LEN( WHERE )
 WRITE( PRTFIL, * ) 'Generated by program or subroutine: ', WHERE( 1:Last )

 Last = LEN( ErrMsg )
 WRITE( PRTFIL, * ) ErrMsg(1:Last)
 WRITE( PRTFIL, * )

! return or stop depending on severity

IF ( SEVRTY == 'W' ) THEN
 RETURN
ELSE
 STOP 'Fatal Error: See print file for details'
ENDIF

END
