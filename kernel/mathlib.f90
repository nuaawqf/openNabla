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
!! Contains subroutines stabilise(),limit(),pos(),sign_(),limitface(),isfrac(),
!! little_endian()
!<

!>This subroutine stabilizes around zero for division
!<
!###########################################################################
function stabilise(s, smal) result(res)
!###########################################################################
!// stabilisation around zero for division
!=================================================
use precision,only:dp
implicit none
real(dp),intent(in) :: s,smal
real(dp)            :: res
if(s>=0.0d0) then  
  res=s + smal
else
  res=s - smal
endif
endfunction stabilise

!###########################################################################
function limit(s1, s2) result(res)
!###########################################################################
!=================================================
use precision,only:dp
implicit none
real(dp),intent(in) :: s1,s2
real(dp)            :: res
if(abs(s1)<abs(s2)) then
  res=s1
else
  res=0.0d0
endif
endfunction limit

!###########################################################################
function pos(x) result(res)
!###########################################################################
use precision,only:dp
implicit none
real(dp),intent(in) :: x
real(dp)            :: res
if(x>=0.0d0) then
  res=1.0d0
else
  res=0.0d0
endif
endfunction pos

!###########################################################################
function sign_(x) result(res)
!###########################################################################
use precision,only:dp
implicit none
real(dp),intent(in) :: x
real(dp)            :: res
if(x>=0.0d0) then
  res=1.0d0
else
  res=-1.0d0
endif
endfunction sign_


!###########################################################################
subroutine limitface(limiter_,maxdelta_,mindelta_,extrapolate_)
!###########################################################################
use precision,only:dp,vsmall
implicit none
real(dp),intent(inout) :: limiter_
real(dp),intent(in)    :: maxdelta_,mindelta_,extrapolate_

if(extrapolate_> (maxdelta_+vsmall)) then
   limiter_ = min(limiter_, maxdelta_/extrapolate_)
else if(extrapolate_ < (mindelta_ - vsmall)) then
   limiter_ = min(limiter_, mindelta_/extrapolate_)
endif
end subroutine limitface

!###########################################################################
function isfrac (x) result (y)
!###########################################################################
!  isfrac  return .true. 
! if the argument has a fractional part, and .false. if the argument is an integer.
!=================================================================================
use precision,only:dp
implicit none

real(dp),intent(in) :: x
logical             :: y

double precision, parameter :: eps = 1.0d-8

if ((abs(x)-int(abs(x))) .gt. eps) then
  y = .true.
else
  y = .false.
end if
end function isfrac

!###########################################################################
function little_endian () result (flag)
!###########################################################################
!  return .true. for a little-endian computer, .false. for big-endian.
!===========================================================================
implicit none
logical :: flag

integer(kind=1), dimension(2) :: iarr       ! array of two 1-byte integers
integer(kind=2)               :: s          ! scalar 2-byte integer

s = 4660                                    ! set scalar to hex 1234
iarr = transfer (s,iarr)                    ! transfer to iarr

if(iarr(1) .eq. 52) then                    ! if element 1 of iarr is hex 34..
  flag = .true.                             ! ..then little endian
else
  flag = .false.                            ! else big endian
end if
return
end function little_endian
