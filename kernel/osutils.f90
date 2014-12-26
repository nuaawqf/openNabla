!--------------------------*-  OPENNABLA  -*--------------------------------!
!    //\                  |OPENNABLA: A FORTRAN90 TOOLKIT FOR COMPUTATIONAL !
!   //  \   C OMPUTATIONAL|           CONTINUOUS MECHANISM (V 1.0)          !
!  //    \  C ONTINUOUS   |COPYRIGHT (C) : LIUHUAFEI                        !
! //      \ M ECHANISM    |EMAIL  :   LIUHUAFEI@HOTMAIL.COM OR              !
! =========               |           LIUHUAFEI@BAOSTEEL.COM                !
!---------------------------------------------------------------------------!
!LICENSE                                                                    !
!  THIS FILE IS PART OF OPENNABLA.                                          !
!                                                                           !
!  OPENNABLA IS FREE SOFTWARE: YOU CAN REDISTRIBUTE IT AND/OR MODIFY IT     !
!  UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY        !
!  THE FREE SOFTWARE FOUNDATION, EITHER VERSION 3 OF THE LICENSE, OR        !
!  (AT YOUR OPTION) ANY LATER VERSION.                                      !
!                                                                           !
!  OPENNABLA IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT WITHOUT !
!  ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF MERCHANTABILITY OR    !
!  FITNESS FOR A PARTICULAR PURPOSE.  SEE THE GNU GENERAL PUBLIC LICENSE    !
!  FOR MORE DETAILS.                                                        !
!                                                                           !
!  YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE        !
!  ALONG WITH OPENNABLA.  IF NOT, SEE <HTTP://WWW.GNU.ORG/LICENSES/>.       !
!                                                                           !
!  YOU MAY MODIFIED THE CODE AND GIVE IT TO THE THIRD PARTIES PROVIDED      !
!  THAT AN ACKNOWLEDGEMENT TO THE SOURCE OF THE ORIGINAL VERSION IS         !
!  RETAINED                                                                 !
!                                                                           !
!---------------------------------------------------------------------------! 

!> \file
!! Contains subroutines getroot(),read_rootcase(),clock_start(),clock_end()
!<

!>This subroutine parses the root dirction
!<
!###########################################################################
subroutine getroot(givenfil, rootname )
!###########################################################################
! parse the root file name from the name of the given file. Count everything
! after the last period as the extension.
!===========================================================================
implicit none
character(*), intent(in)  :: givenfil ! the name of the given file.
character(*), intent(out) :: rootname ! the parsed root name of the given file.

! local declarations.
integer                   :: i
!character(len=1)          :: ch
!
! deal with invalid directory
!
!do i=1,len_trim(givenfil)
!   ch = givenfil(i:i)
!   if(.not.isspace(ch)) then
!     if(ch=="/" .or. ch=="\") then
!	   call fatal("getroot","can't open the directory")
!     else
!	   exit
!	 endif
!   endif
!end do

! deal with a couple of special cases.
!if ( trim( givenfil ) == "." ) then
!   call curdir(rootname)
!   return
!end if

!if(  trim( givenfil ) == ".." )  then
!   call curdir(rootname)
!   i=index(rootname,'\',back=.true.) 
!   rootname=rootname(:i-1)
!   return
!end if
!
! more-normal cases.
!
do i=len_trim( givenfil ),1,-1
   if ( givenfil(i:i) == '.' )  then
      ! make sure the index i is okay
	  rootname = givenfil(:i-1)
      if (i< len_trim( givenfil ) ) then           
         ! make sure we don't have the rootname in a different directory
         if ( index( '\/', givenfil(i+1:i+1)) == 0 ) then
            rootname = givenfil(:i-1)
         else
            rootname = givenfil           ! this does not have a file extension
         end if
      else
         rootname = givenfil(:i-1)
      end if
      return
   end if
end do ! i
rootname =  givenfil
return
end subroutine getroot ! ( givenfil, rootname )

!>This subroutine reads the direction of root and case name
!<
!###########################################################################
subroutine read_rootcase
!###########################################################################
! log:
!     checked - 2013-9-2
!===========================================================================
use precision,only:delim
use scheme,only:casename,casedir
implicit none
character :: rdir*256,dir_root*256

write(*,'(a)') "please input root directory [eg.  . .. c:\case]"
read(*,'(a)')  rdir
call getroot(rdir,dir_root)

write(*,'(a)') "please input case name [eg.  cavity ]"
read(*,'(a)') casename

casedir = trim(dir_root)//delim//trim(casename) 
end subroutine read_rootcase

!>This subroutine starts the clock
!<
!###########################################################################
subroutine  clock_start (start) 
!###########################################################################
! Exercise DATE_AND_TIME and SYSTEM_CLOCK functions to start timing a CPU process
! Warning: reporting rate may be crude, e.g. 0.01 sec
! ==========================================================================
implicit none
integer,intent(out) :: start
integer             :: rate
character* 8        :: the_date
character*10        :: the_time
!
! Call the system clock to start an execution timer.
!
call system_clock ( COUNT = start )
!
call date_and_time ( DATE = the_date )
call date_and_time ( TIME = the_time )

call system_clock ( COUNT_RATE  =  rate )
end subroutine clock_start

!>This subroutine ends the clock
!<
!###########################################################################
subroutine clock_end (seconds,start)
!###########################################################################
! Use start (from cpu_clock_start) and SYSTEM_CLOCK 
! function to find elapsed CPU run time
! Warning: reporting rate may be crude, e.g. 0.01 sec
!===========================================================================
use precision,only:dp
implicit none
integer,intent(in)   :: start
real(dp),intent(out) :: seconds

integer :: finish, rate
!
! Stop the execution timer and report execution time.
call system_clock ( COUNT       = finish )
call system_clock ( COUNT_RATE  =  rate )
if ( finish >= start ) then
  seconds = float( finish - start ) / float( rate )
else
  seconds = 0.0
end if
end subroutine clock_end ! cpu_clock_time 

