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
!! Contains Module meshbase,which defines basic data type for mesh conversion 
!! and manipulation
!<

!> Contains basic data type and global variables for mesh conversion
!! Subroutines include extrude2d()
!<
!############################################################################
module meshbase
!############################################################################
!  three dimensional mesh module
!============================================================================
use precision,only:dp,list
implicit none
!
! data of related to common mesh utility
!
type data_set
  character(len=16)             :: name        ! name of dataset    {-} 
  integer                       :: ndim, nsize ! dimension and size {-}
  integer,allocatable           :: id(:,:)     ! integer data       {-}
  type(list),allocatable        :: ld(:)       ! list data          {-}
  real(dp),allocatable          :: rd(:,:)     ! double data        {-}
  character(len=16),allocatable :: sd(:,:)     ! character data     {-}
end type data_set 

! cpoint_ --  points related to a cell
! cshape_ --  cell shape
! cface_  --  cell faces
! pcell_  --  cell indexs related to a cell
! point_  --  point coordinates
! cell_   --  cell definition
! patch_  --  patch definition
! face_   --  face definition
! own_    --  cell owner 
! nei_    --  cell neighbour
! cycle_  --  cyclic pair
! inter_  --  interface pair 

type(data_set) :: cpoint_,cshape_,cface_,pcell_
type(data_set) :: point_,cell_,patch_,face_,own_,nei_,cycle_,inter_


integer  :: ndim_               !< number of dimensions {-}
integer  :: nvrt_               !< number of vertexs  {-}
integer  :: ncel_               !< number of cells  {-}
integer  :: nreg0_              !< number of original regions {-}
integer  :: nreg_               !< number of boundary regions {-}
integer  :: ncyc_ =0            !< number of cyclic   regions {-}
integer  :: ninter_=0           !< number of interface regions{-}
integer  :: ntvfac_             !< number of vertex on faces {-}
integer  :: nface_              !< number of internal faces  {-}
integer  :: nbfac_              !< number of boundary faces  {-}
integer  :: nczon_              !< number of cell zone {-}
integer  :: nface0_  = 0        !< number of non-conformed faces {-}           
logical  :: axisymm_ =.false.   !< axissymmetrical 

contains

!>This subroutine extrude two-dimensional mesh to three dimension.
!!Two-dimension mesh must be defined on x-y coordinates
!!The extrusion will be in z direction. For plane mesh,a empty patch
!!will be added. For axissymmetrical mesh, two wedge patches are added
!<
!#######################################################################
subroutine extrude2d
!#######################################################################
use precision,only:large,pi
use listlib,only:list_create,list_clone,list_del,list_setsize
implicit none

real(dp),parameter     :: fact = 0.001
real(dp)               :: zoff,xmin,xmax,ymin,ymax,facsin,faccos
real(dp),allocatable   :: oldp(:,:)
type(list),allocatable :: oldface(:),cellface(:)
integer,allocatable    :: oldown(:),oldnei(:),nfaceincell(:)
integer                :: istat,i,ip,in,ifoff,iff,iv,iv1,iv2,ifind,iface
logical,allocatable    :: needfound(:)

ndim_ = point_%ndim
if(ndim_==3) return

call disp("EXTRUDING 2D GRID TO 3D")

allocate(oldp(ndim_,nvrt_),stat=istat)
oldp = point_%rd

xmin = large
ymin = large
xmax = -large
ymax = -large
do i=1,nvrt_
  if(xmin>oldp(1,i)) xmin=oldp(1,i)
  if(ymin>oldp(2,i)) ymin=oldp(2,i)
  if(xmax<oldp(1,i)) xmax=oldp(1,i)
  if(ymax<oldp(2,i)) ymax=oldp(2,i)
end do
zoff = fact*sqrt((xmax-xmin)**2+(ymax-ymin)**2)

deallocate(point_%rd,stat=istat)
allocate(point_%rd(3,2*nvrt_),stat=istat)
point_%ndim  = 3
point_%nsize = 2*nvrt_
!
! two-dimensional grid. extrude in z-direction
! note: in order for the owner-neighbour rules to be right, the
! points given by fluent need to represent the front plane of the
! geometry. therefore, the extrusion will be in -z direction
!
if(axisymm_) then
  faccos = cos(2.5/180.0*pi)
  facsin = sin(2.5/180.0*pi)
  do i=1,nvrt_
    point_%rd(1,i)         = oldp(1,i)
    point_%rd(2,i)         = oldp(2,i)*faccos
    point_%rd(3,i)         = -oldp(2,i)*facsin
    point_%rd(1:2,i+nvrt_) = point_%rd(1:2,i) 
    point_%rd(3,i+nvrt_)   = -point_%rd(3,i)
  end do
else
  do i=1,nvrt_
    point_%rd(1:2,i)       = oldp(1:2,i) 
    point_%rd(3,i)         = -zoff  
    point_%rd(1:2,i+nvrt_) = oldp(1:2,i) 
    point_%rd(3,i+nvrt_)   = zoff
  end do
endif
ndim_ = 3  
nvrt_ = 2*nvrt_

deallocate(oldp,stat=istat)
!
!  count the faces of cell
!
allocate(nfaceincell(ncel_),stat=istat)
allocate(cellface(ncel_),stat=istat)
nfaceincell = 0
do iff=nface0_+1,nface0_+nface_+nbfac_
  ip = own_%id(1,iff) 
  in = nei_%id(1,iff)
  
  nfaceincell(ip) = nfaceincell(ip) + 1
  if(in>0) nfaceincell(in) = nfaceincell(in) +1
end do

do i=1,ncel_
  call list_setsize(cellface(i),nfaceincell(i))
end do

nfaceincell = 0
do iff=nface0_+1,nface0_+nface_+nbfac_
  ip = own_%id(1,iff) 
  nfaceincell(ip) = nfaceincell(ip) + 1
  cellface(ip)%ind(nfaceincell(ip)) = iff
end do

do iff=nface0_+1,nface0_+nface_+nbfac_
  in = nei_%id(1,iff)
  
  if(in>0) then
    nfaceincell(in) = nfaceincell(in) +1
    cellface(in)%ind(nfaceincell(in)) = iff
  endif
end do
!
! save own,nei,facepoly
!
allocate(oldface(nface0_+nface_+nbfac_),oldown(nface0_+nface_+nbfac_),oldnei(nface0_+nface_+nbfac_),stat=istat)
do i=1,nface0_+nface_+nbfac_
  call list_clone(oldface(i),face_%ld(i))
  call list_del(face_%ld(i))
  
  oldown(i) = own_%id(1,i)
  oldnei(i) = nei_%id(1,i)
end do

deallocate(face_%ld,own_%id,nei_%id,stat=istat)
allocate(face_%ld(2*ncel_+nface0_+nface_+nbfac_),&
         nei_%id(1,2*ncel_+nface0_+nface_+nbfac_),&
         own_%id(1,2*ncel_+nface0_+nface_+nbfac_),&
         stat=istat)
face_%ndim  = 1         
face_%nsize = 2*ncel_+nface0_+nface_+nbfac_
own_%ndim   = 1
own_%nsize  = 2*ncel_+nface0_+nface_+nbfac_
nei_%ndim   = 1
nei_%nsize  = 2*ncel_+nface0_+nface_+nbfac_          

do i=nface0_+1,nface0_+nface_+nbfac_
  call list_create(face_%ld(i),4)
  face_%ld(i)%ind(1:2) = oldface(i)%ind(1:2)
  face_%ld(i)%ind(3)   = oldface(i)%ind(2) + nvrt_/2
  face_%ld(i)%ind(4)   = oldface(i)%ind(1) + nvrt_/2
  
  own_%id(1,i) =oldown(i) 
  nei_%id(1,i) =oldnei(i)  
end do
deallocate(oldown,oldnei,stat=istat)

ifoff= nface0_+nface_+nbfac_
do i=1,ncel_
  call list_create(face_%ld(i+ifoff),nfaceincell(i))
  call list_create(face_%ld(i+ifoff+ncel_),nfaceincell(i))
end do

do i=1,ncel_
  iff = cellface(i)%ind(1)
  iv1 = oldface(iff)%ind(1)
  iv2 = oldface(iff)%ind(2)
   
  face_%ld(ifoff+i)%ind(1) = iv2
  face_%ld(ifoff+i)%ind(2) = iv1
  ifind = iv1
  
  allocate(needfound(2:nfaceincell(i)),stat=istat)
  needfound=.true.
  
  do iv = 3,nfaceincell(i)
    do iface = 2,nfaceincell(i)
      if(.not.needfound(iface)) cycle
      
      iff = cellface(i)%ind(iface)
      iv1 = oldface(iff)%ind(1)
      iv2 = oldface(iff)%ind(2)
      
      if(iv1==ifind) then
        needfound(iface) = .false.
        face_%ld(ifoff+i)%ind(iv) = iv2
        ifind  = iv2
        exit
      elseif(iv2==ifind) then
        needfound(iface) = .false.
        face_%ld(ifoff+i)%ind(iv) = iv1
        ifind  = iv1
        exit
      endif
    end do  
  end do
  deallocate(needfound)
  
  do iv=1,nfaceincell(i)
    face_%ld(ifoff+ncel_+i)%ind(iv) = face_%ld(ifoff+i)%ind(nfaceincell(i)-iv+1) + nvrt_/2
  end do
  
  own_%id(1,ifoff+i) = i
  nei_%id(1,ifoff+i) = 0
  own_%id(1,ifoff+ncel_+i) = i
  nei_%id(1,ifoff+ncel_+i) = 0
end do

do i=1,nface0_+nface_+nbfac_
  call list_del(oldface(i))
end do
do i=1,ncel_
  call list_del(cellface(i))
end do  
deallocate(oldface,cellface,nfaceincell,stat=istat)

nreg0_ = nreg0_ +1
nreg_  = nreg_  +1

if(axisymm_) then
  patch_%id(1,nreg0_) = -1 
  patch_%id(2,nreg0_) = -2
  patch_%id(3,nreg0_) = nface0_+nface_+nbfac_+1
  patch_%id(4,nreg0_) = nface0_+nface_+nbfac_+ncel_
  patch_%sd(1,nreg0_) = 'back'
  patch_%sd(2,nreg0_) = 'wedge'
  
  nreg_  = nreg_  +1
  nreg0_ = nreg0_ +1
  patch_%id(1,nreg0_) = -1 
  patch_%id(2,nreg0_) = -2
  patch_%id(3,nreg0_) = nface0_+nface_+nbfac_+ncel_+1
  patch_%id(4,nreg0_) = nface0_+nface_+nbfac_+2*ncel_
  patch_%sd(1,nreg0_) = 'front'
  patch_%sd(2,nreg0_) = 'wedge'

  patch_%nsize        = nreg0_
else
  patch_%id(1,nreg0_) = -1 
  patch_%id(2,nreg0_) = -1
  patch_%id(3,nreg0_) = nface0_+nface_+nbfac_+1
  patch_%id(4,nreg0_) = nface0_+nface_+nbfac_+2*ncel_
  patch_%sd(1,nreg0_) = 'frontandback'
  patch_%sd(2,nreg0_) = 'empty'
  patch_%nsize        = nreg0_
endif  

nbfac_ = nbfac_ + ncel_*2
end subroutine extrude2d

end module meshbase





