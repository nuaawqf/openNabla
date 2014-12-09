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
!! Contains main program of Mesh Conversion
!<

!> converse Fluent   binary case files to openNabla binary mesh\n
!! converse OpenFOAM ascii  case files to openNabla binary mesh\n
!<

!############################################################################
program main
!############################################################################
! Description:
!                                              author: Liu Huafei
!                                              Updated : 2014/6/24
!                                              Email : liuhuafei@baosteel.com
!============================================================================
! usage:
! dname ----  the directory name which fluent case file locates
! itype ----  mesh type (1 fluent binary case 2 openFOAM ascii case)
!             fluent binary case is a .cas file
!             openFOAM ascii case include points,faces,owner,neighbour,bounday files
! axisymm---  if the mesh is 2D, axissymm = T for axissymmetrical, = F for plane
!
! example:  
! In c:\tmp, fluent case file is 3d.cas 
! dname = c:\tmp   itype = 1 axisymm = F
! openFOAM case files exist in In directory c:\tmp2 
! dname = c:\tmp2  itype = 2 axisymm = F
!============================================================================
implicit none
!
character :: dname*256,cname*32  
logical   :: axisymm
integer   :: itype

call disp('PLEASE INPUT DIRECTORY WHERE FILES LOCATE (EXAMPLE: c:\tut or /usr/tut)')
read(*,'(a)') dname

call disp('PLEASE MESH TYPE (EXAMPLE: 1--FLUENT 2--OpenFOAM)')
read(*,*)     itype

call disp('IF 2D MESH, IS IT AXISSYMMETRICAL (T/F) ?')
read(*,*)     axisymm

call msh_imp(dname,itype,axisymm)
pause
end program main









