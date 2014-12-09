!--------------------------*-  OpenNabla  -*--------------------------------!
!    //\                  |OpenNabla: A Fortran90 toolkit for Computational !
!   //  \   C omputational|           Continuous Mechanism (v 1.0)          !
!  //    \  C ontinuous   |Copyright (C) : liuhuafei                        !
! //      \ M echanism    |Email  :   liuhuafei@hotmail.com or              !
! =========               |           LIUHUAFEI@BAOSTEEL.COM                 !
!---------------------------------------------------------------------------!
!License                                                                    !
!  This file is part of OpenNabla.                                          !
!                                                                           !
!   OpenNabla is free software: you can redistribute it and/or modify it    !
!   under the terms of the GNU General Public License as published by       !
!   the Free Software Foundation, either version 3 of the License, or       !
!   (at your option) any later version.                                     !
!                                                                           !
!   OpenNabla is distributed in the hope that it will be useful, but WITHOUT!
!   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or   !
!   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License   !
!   for more details.                                                       !
!                                                                           !
!   You should have received a copy of the GNU General Public License       !
!   along with OpenNabla.  If not, see <http://www.gnu.org/licenses/>.      !
!                                                                           !
!   You may modified the code and give it to the third parties provided     !
!   that an acknowledgement to the source of the original version is        !
!   retained                                                                !
!                                                                           !
!---------------------------------------------------------------------------! 

!> \file
!! Contains Module precision, which defines basic data type
!! and holds some global variables.
!<

!> Contains basic data type in openNabla
!<
!############################################################################
module precision
!############################################################################
!  definition of simple data type precision
!============================================================================
use iso_c_binding,only:c_float,c_double,c_intptr_t
implicit none
save
integer,parameter  :: byt32   = 1
integer,parameter  :: sp      = c_float           !< defaule opennabla float
integer,parameter  :: dp      = c_double          !< defaule opennabla double  
integer,parameter  :: ptr     = c_intptr_t        !< defaule opennabla array or function address
!
! c_intptr_t is equal to c_long,c_size_t
!

integer,parameter  :: sp64    = 2*sp
integer,parameter  :: dp64    = 2*dp

type list                                         !<list type for face definition
 integer,allocatable :: ind(:)
endtype list

real(dp),parameter :: small   = 1.e-30            !< small numeric
real(dp),parameter :: large   = 1.e+30            !< large numeric
real(dp),parameter :: vsmall  = tiny(1.0)         !< tiny  numeric
real(dp),parameter :: vlarge  = huge(1.0)         !< huge  numeric
real(dp),parameter :: pi      = 3.1415926535898d0 !
                                 
character,parameter:: delim*1 = achar(92)         !  \               
integer,parameter  :: ndim    = 3                 ! default three dimension

end module