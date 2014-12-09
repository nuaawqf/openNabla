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
!! Contains Mesh Manipulation Subroutines msh_dealloc(),msh_rotate(),
!! msh_scale(),msh_translate(),msh_write(),msh_imp()
!<

!> delete all the dynamic memory used in the meshbase
!<
!############################################################################
subroutine msh_dealloc
!############################################################################
!============================================================================
use meshbase
implicit none

call memdel(cpoint_)
call memdel(cshape_)
call memdel(cface_)
call memdel(pcell_)
call memdel(point_)
call memdel(cell_)
call memdel(patch_)
call memdel(face_)
call memdel(own_)
call memdel(nei_)
call memdel(cycle_)

contains
subroutine memdel(d_)
implicit none
type(data_set) :: d_

integer :: i,j,ierr

if(allocated(d_%rd)) deallocate(d_%rd)
if(allocated(d_%id)) deallocate(d_%id)
if(allocated(d_%sd)) deallocate(d_%sd)

if(allocated(d_%ld)) then
  do i=1,d_%nsize
    if(allocated(d_%ld(i)%ind)) deallocate(d_%ld(i)%ind,stat=ierr)
  end do
  deallocate(d_%ld,stat=ierr)
endif  
d_%nsize = 0
d_%ndim  = 0
end subroutine memdel
end subroutine msh_dealloc

!> rotate a import mesh from direction n1 to direction n2
!>@param[in]   n1 -- the origin direction vector
!>@param[in]   n2 -- the target direction vector
!<
!#######################################################################
subroutine msh_rotate(n1,n2)
!#######################################################################
use precision,only:dp
use vec3dlib,only:norm,vrot,transform
use meshbase,only:point_,nvrt_
implicit none
real(dp),intent(in) :: n1(3),n2(3)

real(dp) :: n11(3),n22(3),t(9)
integer  :: i

n11 = norm(n1)
n22 = norm(n2)

t   = vrot(n11,n22)
!
! rotate mesh
!
call disp('ROTATING MESH ...')

do i=1,nvrt_
  point_%rd(:,i) = transform(t,point_%rd(:,i))
end do
end subroutine msh_rotate

!> scale a import mesh with scale factor 
!!the grid coordinate (x,y,z) will change to (x*s,x,y*s.y,z*s.z)\n
!>@param[in]   s -- the scale factor
!<
!#######################################################################
subroutine msh_scale(s)
!#######################################################################
use precision,only:dp
use meshbase,only:point_,nvrt_
implicit none
real(dp),intent(in) :: s(3)

integer  :: i
!
! scale mesh
!
call disp('SCALING MESH ...')

do i=1,nvrt_
  point_%rd(:,i) = point_%rd(:,i)*s
end do
end subroutine msh_scale


!>translate a import mesh with translation vector.
!!the grid coordinate (x,y,z) will change to (x+t.x,y+t.y,z+t.z)
!>@param[in]   t -- the translation vector
!<
!#######################################################################
subroutine msh_translate(t)
!#######################################################################
use precision,only:dp
use meshbase,only:point_,nvrt_
implicit none
real(dp),intent(in) :: t(3)

integer  :: i
!
! scale mesh
!
call disp('SCALING MESH ...')

do i=1,nvrt_
  point_%rd(:,i) = point_%rd(:,i)+t
end do
end subroutine msh_translate

!>write a imported mesh into openNable mesh
!>@param[in]   mfile -- mesh file unit
!>@param[in]   bfile -- boundary file unit
!<
!############################################################################
subroutine msh_write(mfile,bfile)
!############################################################################
use precision,only:delim
use fluentbase
use meshbase
use listlib,only:list_len
implicit none
integer,intent(in) :: mfile,bfile

integer   :: i,ierr
character :: str*64
!
call disp('WRITING POINTS.....')

call wrtline(mfile,'# POINT COORDINATES')
write(str,'(i0,1x,i0)') ndim_,nvrt_
call wrtline(mfile,str)
call wrtline(mfile,'(')
call wrtreal(mfile,ndim_,nvrt_,point_%rd,1,nvrt_)
call wrtline(mfile,')')
!
call disp('WRITING FACE BASED TOPOLOGY......')

ntvfac_ = 0
do i=1,nface_+nbfac_
  ntvfac_ = ntvfac_ + list_len(face_%ld(i))
end do
call wrtline(mfile,'# FACES BASE TOPOLOGY')
write(str,'(i0,1x,i0)') ntvfac_,nface_+nbfac_
call wrtline(mfile,str)
call wrtline(mfile,'(')
call wrtface(mfile,nface_+nbfac_,face_%ld(:),own_%id(1,:),nei_%id(1,:),1,nface_+nbfac_)
call wrtline(mfile,')')

call disp('WRITING INTERFACE AND CYCLIC PAIRS.....')

call wrtline(mfile,'# CYCLIC AND INTERFACE PAIRS')
write(str,'(i0,1x,i0)') ncyc_,ninter_
call wrtline(mfile,str)

close(mfile)
!
call disp('WRITING CELLS AND BOUNDARIES......')


call wrtline(bfile,'# CELLS')
write(str,'(i0)') nczon_
call wrtline(bfile,str)

call wrtline(bfile,'(')
do i=1,nczon_
  call wrtline(bfile,adjustl(cell_%sd(1,i)))
  call wrtline(bfile,'{')

  write(str,'(t5,a)')    'TYPE '//trim(cell_%sd(2,i))
  call wrtline(bfile,str)

  write(str,'(t5,a,i0)') 'NCELLS ',cell_%id(2,i)-cell_%id(1,i)+1
  call wrtline(bfile,str)
  
  write(str,'(t5,a,i0)')   'STARTCELL ',cell_%id(1,i)
  call wrtline(bfile,str)

  call wrtline(bfile,'}')
end do
call wrtline(bfile,')')

call wrtline(bfile,'# BOUNDARIES')

write(str,'(i0)') nreg_
call wrtline(bfile,str)
call wrtline(bfile,'(')

do i=1,nreg_
  call wrtline(bfile,adjustl(patch_%sd(1,i)))
  call wrtline(bfile,'{')
  
  write(str,'(t5,a)')  'TYPE '//trim(patch_%sd(2,i))
  call wrtline(bfile,str)
  
  write(str,'(t5,a,i0)')   'NFACES ',patch_%id(2,i)-patch_%id(1,i)+1
  call wrtline(bfile,str)

  write(str,'(t5,a,i0)')   'STARTFACE ',patch_%id(1,i)
  call wrtline(bfile,str)
  
  call wrtline(bfile,'}')
end do
call wrtline(bfile,')')

close(bfile)
return

end subroutine msh_write

!>import a mesh to openNable mesh
!>@param[in]   dname   -- directory name
!>@param[in]   itype   -- mesh type
!>@param[in]   axisymm -- axisymmetrical
!<
!#######################################################################
subroutine msh_imp(dname,itype,axisymm)
!#######################################################################
use precision,only:delim
implicit none
character(len=*),intent(in) :: dname
integer,intent(in)          :: itype
logical,intent(in)          :: axisymm

integer   :: mfile,bfile,ifile,pfile,ffile,ofile,nfile,ierr
character :: fname*256,cfile*16
 
select case(itype)
  case(1)
    call disp('PLEASE INPUT BINARY CASE FILENAME (EXAMPLE: mesh3d ---NO SUFFIX NEEDED)')
    read(*,'(a)') cfile

    fname = trim(dname)//delim//trim(cfile)//'.cas'
  
    call fopen(mfile,fname,'rb',ierr)
    if(ierr/=0) goto 100
    
    call xf_msh_get(mfile,axisymm)
    close(mfile)

    call fopen(mfile,trim(dname)//delim//'mesh','wb',ierr)
    call fopen(bfile,trim(dname)//delim//'boundary','w',ierr)

    call xf_msh_wrt(mfile,bfile)
  case(2)
    call fopen(pfile,trim(dname)//delim//'points','r',ierr)
    if(ierr/=0) goto 100
    call fopen(ffile,trim(dname)//delim//'faces','r',ierr)
    if(ierr/=0) goto 100
    call fopen(ofile,trim(dname)//delim//'owner','r',ierr)
    if(ierr/=0) goto 100
    call fopen(nfile,trim(dname)//delim//'neighbour','r',ierr)
    if(ierr/=0) goto 100
    call fopen(bfile,trim(dname)//delim//'boundary','r',ierr)
    if(ierr/=0) goto 100
  
    call of_msh_get(pfile,ffile,ofile,nfile,bfile)

    call fopen(mfile,trim(dname)//delim//delim//'mesh','wb',ierr)
    call fopen(bfile,trim(dname)//delim//delim//'boundary','w',ierr)

    call msh_write(mfile,bfile)
end select

call msh_dealloc
return

100 continue
call info('MESH_IMP','E',ierr)
return
end subroutine msh_imp

