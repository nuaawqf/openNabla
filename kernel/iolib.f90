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
!! Contains field input & ouput subroutines: getval(),wrtval(),
!! getcpar(),wrtcpar(),getrpar(),wrtrpar(),getipar(),wrtipar()
!<

!> This routine gets a real array according to keyword
!>@param[in]   ifile    -- file unit
!>@param[in]   key      -- keyword for real array
!>@param[in]   m        -- dimension of val(m,n)
!>@param[in]   n        -- dimension of val(m,n)
!>@param[in]   val      -- real array val(m,n)
!>@param[in]   ist      -- starting index of val(m,ist:ien)
!>@param[in]   ien      -- ending index of val(m,ist:ien)
!>@param[in]   usercode -- if keyword contains 'usercode'
!>@param[out]  ierr     -- return error code
!<
!##########################################################
subroutine getval(ifile,key,m,n,val,ist,ien,usercode,ierr)
!##########################################################
! get value from file
!==========================================================
use precision,only:dp
use scheme,only:err_str_toreal
implicit none
integer,intent(in)         :: ifile
character(len=*),intent(in):: key
integer,intent(in)         :: m,n
real(dp),intent(inout)      :: val(m,n)
integer,intent(in)         :: ist,ien
logical,intent(out)        :: usercode
integer,intent(out)        :: ierr

logical,external :: strcmp

character :: line*1024,str*1024
integer   :: nm,nn,i
real(dp)  :: valc(m)
character :: fun*16='GETVAL' 

call findkey(ifile,trim(key),line,ierr)
if(ierr/=0) return

call sreadstr(line,str)

usercode = .false.

if(strcmp(str,'UNIFORM')) then
  read(line,*,err=100) valc

  do i=ist,ien
    val(:,i) = valc
  enddo
elseif(strcmp(str,'NONUNIFORM')) then
  call getline(ifile,str,ierr)
  if(ierr/=0) return
  
  read(str,*,err=100) nm,nn
  if(nn/=(ien-ist+1)) goto 100

  call findc(ifile,'(',.true.,ierr)
  if(ierr/=0) return
  call getreal(ifile,m,n,val,ist,ien,.false.,ierr)
  if(ierr/=0) return
  call findc(ifile,')',.true.,ierr)
  if(ierr/=0) return
elseif(strcmp(str,'USERCODE')) then
  usercode = .true.
else
  call uinfo(trim(fun),'E','READ VALUE ERROR')     
endif
return

100 continue
ierr =err_str_toreal
return
end subroutine getval

!> This routine writes a real array  including keyword
!>@param[in]   ifile    -- file unit
!>@param[in]   key      -- keyword for real array
!>@param[in]   m        -- dimension of val(m,n)
!>@param[in]   n        -- dimension of val(m,n)
!>@param[in]   val      -- real array val(m,n)
!>@param[in]   ist      -- starting index of val(m,ist:ien)
!>@param[in]   ien      -- ending index of val(m,ist:ien)
!>@param[in]   usercode -- if keyword contains 'usercode'
!<
!##########################################################
subroutine wrtval(ifile,key,m,n,val,ist,ien,usercode)
!##########################################################
! write value to file
!==========================================================
use precision,only:dp
implicit none
integer,intent(in)          :: ifile
character(len=*),intent(in) :: key
integer,intent(in)          :: m,n
real(dp),intent(in)          :: val(m,n)
integer,intent(in)          :: ist,ien
logical,intent(in)          :: usercode

logical   :: uniform = .true.
character :: str*1024,fmt*64

if((ien-ist)>0) uniform = .false.

if(usercode) then
  str='  '//trim(key)//' USERCODE'
else
  if(uniform) then
    write(fmt,'(a,i0,a)')   '(2a,',m,'(es10.3,1x))'
    write(str,fmt=fmt)      trim(key),' UNIFORM ',val(m,ist)
  else
    write(str,'(3x,a,1x,a)') trim(key),' NONUNIFORM '
  endif
endif
call wrtline(ifile,trim(str))

write(str,'(i0,1x,i0)') m,ien-ist+1
call wrtline(ifile,str)
call wrtline(ifile,'(')
call wrtreal(ifile,m,n,val,ist,ien)
call wrtline(ifile,')')

end subroutine wrtval

!> This routine gets string paramters from file
!>@param[in]   ifile    -- file unit
!>@param[in]   key      -- keyword for string parameters
!>@param[in]   m        -- dimension of string array val(m)
!>@param[in]   val      -- string array val(m)
!>@param[out]  ierr     -- return error code
!<
!###########################################################################
subroutine getcpar(ifile,key,m,val,ierr)
!###########################################################################
! get string parameter from file
!==========================================================
use precision,only:dp
implicit none
integer,intent(in)           :: ifile
character(len=*),intent(in)  :: key
integer,intent(in)           :: m
character(len=*),intent(out) :: val(m)
integer,intent(out)          :: ierr

character :: line*1024
integer   :: i

call findkey(ifile,trim(key),line,ierr)
if(ierr/=0) return

do i=1,m
  call sreadstr(line,val(i))
end do  
end subroutine getcpar

!> This routine writes string paramters into file
!>@param[in]   ifile    -- file unit
!>@param[in]   key      -- keyword for string parameters
!>@param[in]   m        -- dimension of string array val(m)
!>@param[in]   val      -- string array val(m)
!<
!##########################################################
subroutine wrtcpar(ifile,key,m,val)
!##########################################################
! write string parameter from file
!==========================================================
implicit none
integer,intent(in)          :: ifile
character(len=*),intent(in) :: key
integer,intent(in)          :: m
character(len=*),intent(in) :: val(m)

integer   :: i
character :: line*1024

line='  '//trim(key)
do i=1,m
  call strcat(line,' '//trim(val(i)))
end do
call wrtline(ifile,trim(line))
end subroutine wrtcpar

!> This routine gets real paramters from file
!>@param[in]   ifile    -- file unit
!>@param[in]   key      -- keyword for real array
!>@param[in]   m        -- dimension of real array val(m)
!>@param[in]   val      -- real array val(m)
!>@param[out]  ierr     -- return error code
!<
!###########################################################################
subroutine getrpar(ifile,key,m,val,ierr)
!###########################################################################
! get real*8 parameter from file
!==========================================================
use precision,only:dp
use scheme,only:err_str_toreal
implicit none
integer,intent(in)          :: ifile
character(len=*),intent(in) :: key
integer,intent(in)          :: m
real(dp),intent(out)        :: val(m)
integer,intent(out)         :: ierr

character :: line*1024
 
call findkey(ifile,trim(key),line,ierr)
if(ierr/=0) return

read(line,*,err=100) val(1:m)
return

100 continue
ierr =err_str_toreal
return
end subroutine getrpar

!> This routine writes real paramters into file
!>@param[in]   ifile    -- file unit
!>@param[in]   key      -- keyword for real array
!>@param[in]   m        -- dimension of real array val(m)
!>@param[in]   val      -- real array val(m)
!<
!##########################################################
subroutine wrtrpar(ifile,key,m,val)
!##########################################################
! write real*8 parameter to file
!==========================================================
use precision,only:dp
implicit none
integer,intent(in)          :: ifile
character(len=*),intent(in) :: key
integer,intent(in)          :: m
real(dp),intent(in)         :: val(m)

character :: str*1024,fmt*64

write(fmt,'(a,i0,a)') '(',m,'(es10.3,1x))'
write(str,fmt=trim(fmt)) val
call wrtline(ifile,'  '//trim(key)//' '//trim(str))
end subroutine wrtrpar

!> This routine gets integer paramters from file
!>@param[in]   ifile    -- file unit
!>@param[in]   key      -- keyword for integer array
!>@param[in]   m        -- dimension of integer array val(m)
!>@param[in]   val      -- integer array val(m)
!>@param[out]  ierr     -- return error code
!<
!###########################################################################
subroutine getipar(ifile,key,m,val,ierr)
!###########################################################################
! get integer parameter from file
!==========================================================
use scheme,only:err_str_toint
implicit none
integer,intent(in)          :: ifile
character(len=*),intent(in) :: key
integer,intent(in)          :: m
integer,intent(out)         :: val(m)
integer,intent(out)         :: ierr

character :: line*1024
 
call findkey(ifile,trim(key),line,ierr)
if(ierr/=0) return

read(line,*,err=100) val(1:m)
return

100 continue
ierr =err_str_toint
return
end subroutine getipar

!> This routine writes integer paramters into file
!>@param[in]   ifile    -- file unit
!>@param[in]   key      -- keyword for integer array
!>@param[in]   m        -- dimension of integer array val(m)
!>@param[in]   val      -- integer array val(m)
!<
!##########################################################
subroutine wrtipar(ifile,key,m,val)
!##########################################################
! write integer parameter to file
!==========================================================
implicit none
integer,intent(in)          :: ifile
character(len=*),intent(in) :: key
integer,intent(in)          :: m
integer,intent(in)          :: val(m)

character :: str*1024,fmt*64

write(fmt,'(a,i0,a)') '(',m,'(i0,1x))'
write(str,fmt=trim(fmt)) val
call wrtline(ifile,'  '//trim(key)//' '//trim(str))
end subroutine wrtipar
