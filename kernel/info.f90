!--------------------------*-  OPENNABLA  -*--------------------------------!
!    //\                  |OPENNABLA: A FORTRAN90 TOOLKIT FOR COMPUTATIONAL !
!   //  \   C OMPUTATIONAL|           CONTINUOUS MECHANISM (V 1.0)          !
!  //    \  C ONTINUOUS   |COPYRIGHT (C) : LIUHUAFEI                        !
! //      \ M ECHANISM    |EMAIL  :   LIUHUAFEI@HOTMAIL.COM OR              !
! =========               |           LIUHUAFEI@BAOSTEEL.COM                 !
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
!! Contains subroutines info(),uinfo(),disp(),banner()
!<

!> This routine handle error and warning 
!<
!###########################################################################
subroutine info(caller,tag,imsg)
!###########################################################################
!  error and warning handing routine
!===========================================================================
!>@param[in]  caller -- calling subroutine name
!>@param[in]  tag    -- 'I'(info) 'W'(warning) 'E'(error)
!>@param[in]  imsg   -- message index
!<
!===========================================================================
use scheme
implicit none
character(len=*),intent(in)  :: caller
character(len=1),intent(in)  :: tag
integer,intent(in)           :: imsg

character :: ans*1,msg*256

select case(imsg)
!
!
!------ MEMORY ERROR----------------
!
  case(err_mem_maxvar)
    msg ='INSUFFICIENT VAIABLE ARRAY, NEED INCEASE THE MAXVAR IN MEM.INC AND RECOMPILE'
  case(err_mem_varnoex)
    msg ='VARIABLE NOT EXISIED'
!
!------ FIEL  ERROR----------------
!
  case(err_file_nounit)
    msg ='NO FILE UNIT AVAILABLE'
  case(err_file_noexist)
    msg ='FILE NOT EXSIT'
  case(err_file_cantread)
    msg ='FILE NOT READABLE'
  case(err_file_cantwrite)
    msg ='FILE NOT WRITABLE'
  case(err_file_end)
    msg ='REACH END OF FILE,NO LINE READ'
  case(err_file_realarr)
    msg ='READING REAL ARRAY'
  case(err_file_intarr)
    msg ='READING INTEGER ARRAY'
  case(err_file_listarr)
    msg ='READING LIST ARRAY'
  case(err_file_keyword)
    msg ='READING KEYWORD'
  case(err_file_char)
    msg ='RAEDING CHARACTER SUCH AS {} ()'
!
!------ STRING  ERROR----------------
!
  case(err_str_toint)
    msg ='CONVERSE STRING TO INTEGER' 
  case(err_str_toreal)
    msg ='CONVERSE STRING TO REAL'
  case(err_str_tonum)
    msg ='CONVERSE STRING TO REAL OR INTEGER'
!
!------ MESH ERROR
!    
  case(err_msh_vrtfacnum)
    msg ='INVALID MESH VERTEX/FACE/OWN/NEI NUMBER'
  case(err_msh_badvect)
    msg ='INVALID VECTOR OR TENSOR VALUE IN KEYWORD VALUE'
!
!-----  FVM DICRETIZATION ERR
!
  case(err_fvm_badddt)
    msg = 'WRONG INPUT PARAMTERS FOR DDT SCHEME'
  case(err_fvm_badddtback)
    msg = 'WRONG INPUT PARAMTERS FOR DDT_BACKWARD SCHEME'
!
!-----  BC DICRETIZATION ERR
!
end select

select case(tag)
  case( "I","i" ) ! info message
    write(*,'(a)') trim(caller)//") INFO: "//trim(msg)
  case( "W","w" ) ! Warning message
    write(*,'(a)') trim(caller)//") WARNING: "//trim(msg)
    pause
  case( "E","e" ) ! ERROR message
    write(*,'(a)') trim(caller)//") ERROR: "//trim(msg)
    write(*,'(a)') 'TERMINATE PROGRAM ? (Y/N)'
    read(*,'(a)') ans
    if(ans=='Y'.or.ans=='y') stop    
  case default
    write(*,'(a)') trim(caller)//") ERROR: "//'UNKNOWN ERROR'
    pause
    stop
end select
end subroutine info

!> This routine handle user defined error and warning 
!<
!###########################################################################
subroutine uinfo(caller,tag,msg )
!###########################################################################
!  user defined error and warning handing routine
!===========================================================================
!>@param[in]  caller -- calling subroutine name
!>@param[in]  tag    -- 'I'(info) 'W'(warning) 'E'(error)
!>@param[in]  msg    -- message string
!<
!===========================================================================
implicit none
character(len=*),intent(in)  :: caller
character(len=1),intent(in)  :: tag
character(len=*),intent(in)  :: msg

character*1 :: ans

select case(tag)
  case( "I","i" ) ! info message
    write(*,'(a)') trim(caller)//") INFO: "//trim(msg)
  case( "W","w" ) ! Warning message
    write(*,'(a)') trim(caller)//") WARNING: "//trim(msg)
    pause
  case( "E","e" ) ! ERROR message
    write(*,'(a)') trim(caller)//") ERROR: "//trim(msg)
    write(*,'(a)') 'TERMINATE PROGRAM ? (Y/N)'
    read(*,'(a)') ans
    if(ans=='Y'.or.ans=='y') stop    
  case default
    write(*,'(a)') trim(caller)//") ERROR: "//'UNKNOWN ERROR'
    pause
    stop
end select
end subroutine uinfo

!> This routine displays message 
!<
!###########################################################################
subroutine disp(msg)
!###########################################################################
!  this routine displays a message on the screen.
!==========================================================================
!>@param[in]  msg    -- message string
!<
!===========================================================================

implicit none
character(len=*),intent(in)  :: msg
!
write(*,'(a)') trim(msg)
end subroutine disp

!> This routine displays openNabla's banner
!<
!############################################################################
subroutine banner(msg)
!############################################################################
! print the banner of openNABLA
! msg:    message defined by user
! log:    checked - 2013-9-2
!============================================================================
!>@param[in]  msg    -- message string
!<
!===========================================================================
implicit none
character(len=*),intent(in) :: msg

write(*,'(a)')      ''
write(*,'(a75)')    "++*---------------------------*- FORTRAN 90 -*--------------------------*++"
write(*,'(a75)')    "+   //\                  |OPENNABLA: A FORTRAN90 LIBRARY FOR COMPUTATIONAL+"
write(*,'(a75)')    "+  //  \   C OMPUTATIONAL|           CONTINUOUS MECHANISM                 +"
write(*,'(a75)')    "+ //    \  C ONTINUOUS   |VERSION:   1.000                                +"
write(*,'(a75)')    "+//      \ M ECHANISM    |EMAIL  :   liuhuafei@baosteel.com               +"
write(*,'(a75)')    "+=========               |           liuhuafei@hotmail.com                +"
write(*,'(a75)')    "++*---------------------------------------------------------------------*++"
write(*,'(t37,a)')  trim(msg)
write(*,'(a75)')    "++*---------------------------------------------------------------------*++"
end subroutine banner

!############################################################################
subroutine strcmpt(text)
!############################################################################
! compac takes a text string and converts it to a left-justified
! string with no embedded blanks.
!============================================================================
implicit none
character(len=*),intent(inout) :: text

integer  :: ilen,i,j,ii
character:: c

c    = achar(32)
ilen = len(text)

i = 1
j = 1
10 continue
if(text(i:i)==c) then
   j = i + 1
20   continue
   if(text(j:j)/=c) then
     text(i:i) = text(j:j)
     i = i + 1
     j = j + 1
   else
     j = j + 1
   endif
   if(j<=ilen) go to 20
   go to 30
 endif
 
 i = i + 1
 j = j + 1
 if(i<ilen) go to 10
!
30 continue
!
! -- blank-fill remainder of string
!
if(j/=i) then
  do ii = i,ilen
    text(ii:ii) = c
  end do
endif
return
end subroutine strcmpt