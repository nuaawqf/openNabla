!--------------------------*-  OPENNABLA  -*--------------------------------!
!    //\                  |OPENNABLA: A FORTRAN90 TOOLKIT FOR COMPUTATIONAL !
!   //  \   C OMPUTATIONAL|           CONTINUOUS MECHANISM (V 1.0)          !
!  //    \  C ONTINUOUS   |COPYRIGHT (C) : LIUHUAFEI                        !
! //      \ M ECHANISM    |EMAIL  :   LIUHUAFEI@HOTMAIL.COM OR              !
! =========               |           XINGGAILAN@ECUST.EDU.CN               !
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

!############################################################################
program main
!############################################################################
! Description:
!  Steady-state solver for incompressible,laminal flow of non-Newtonian fluids.
!                                              Author: Liu Huafei
!                                              Email : liuhuafei@baosteel.com
!============================================================================
use precision
use scheme
use comm0
implicit none
!
integer  :: mfile,bfile,ierr
character:: file*256,mode*3
!
!--------------------------Print CCM Banner--------------------------
!
call banner('MESHIO')
!
!--------------------------Set Case root-----------------------------
!
call read_rootcase

binary = .true.
mode = 'r'
if(binary) mode = 'rb'

call fopen(mfile,trim(casedir)//delim//'mesh',mode,ierr)
call fopen(bfile,trim(casedir)//delim//'boundary','r',ierr)
!
!
call grd_get(mfile,bfile)
call pst_msh

call grd_del
call banner('MESHIO')
pause

end program main



