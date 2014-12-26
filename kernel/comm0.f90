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
!! Contains Module comm0,which defines mesh-related data And these data are
!! used to describe geomtrical descretization the solution domain and function
!! and subroutine whichPatch(),memlog(). 
!<

!> Contains vertex coordinates,cell center,internal and boundary faces,boundary
!! type and some common data.
!<
!############################################################################
module comm0
!############################################################################
! common data for calculation
!============================================================================
use precision,only:list,dp
implicit none
public
save

!!!dir$ attributes align:64 :: x,xo,ncp,xc,vol,volo,voloo,own,nei
!!!dir$ attributes align:64 :: swvol,swvolb,sf,sb,magsf,magsb,xf,xb
!!!dir$ attributes align:64 :: xfo,xbo,fx,fxb,dr,drb,corr,corrb
!!!dir$ attributes align:64 :: lsp,lspb,lsn,lsnb,lfpl,nfpu
!
!===============================================================
! solution dimension & geometrical dimension
!===============================================================
!
integer               :: nsold,ngeomd          !<number of solution dimensions
integer               :: sold(3),geomd(3)      !<solution flag vector,geometrical flag vector 
!
!===============================================================
! the scheme of grid motion and deformation
!===============================================================
!
logical               :: moving,changing       !<moving flag and geometry changing flag
!
!===============================================================
! data related to different materials or equations
!===============================================================
!
integer                :: ndo =1           ! the number of domains             {-}
integer                :: nph =1           ! the number of fluid phases        {-}

integer,allocatable   :: nsc(:),nec(:)         ! starting and ending number of cells (ndo)    {-}
integer,allocatable   :: cityp(:)              ! domain type index (ndo)
character,allocatable :: ctype(:)*32           ! domain type name (ndo)
character,allocatable :: cname(:)*32           ! domain name (ndo)

integer,allocatable   :: nsf(:),nef(:)         ! starting and ending number of faces (ndo)    {-}
integer,allocatable   :: nsb(:),neb(:)         ! starting and ending number of boundaries(ndo){-}

!
!===============================================================
! vertex section
!===============================================================
!
integer               :: nvrt =0               ! vertice number                           {-}
real(dp),allocatable  :: x(:,:)                ! array of vertice coordinates     [3,nvrt]{m}
real(dp),allocatable  :: xo(:,:)               ! array of old vertice coordinates [3,nvrt]{m}
real(dp),allocatable  :: cpwt(:)               ! cell to point interpolation weight{-}
!
!===============================================================
! edge section
!===============================================================
!
integer               :: nedg =0               ! edge  number                           {-}
!
!===============================================================
! cell section
!===============================================================
!
integer               :: ncell= 0             ! number of cell {-}
integer,allocatable  :: cface(:),cface_p(:)   ! face indexs defining cell (ncell)
!
real(dp),allocatable  :: xc(:,:)              ! cell centre (3,ncell)      {m}
real(dp),allocatable  :: vol(:)               ! cell volume (ncell)        {m3}
real(dp),allocatable  :: volo(:)              ! old cell volume (ncell)    {m3}
real(dp),allocatable  :: voloo(:)             ! old old cell volume (ncell){m3}
!
!===============================================================
! face section
!===============================================================
!
integer              :: ntface=0        ! total of face number              {-}
integer              :: ntbfac=0        ! total of boundary number          {-}
integer              :: ntvfac=0        ! total of vertex on faces          {-)
integer              :: nface=0         ! internal face number              {-}
integer              :: nbfac=0         ! boundary faces excluding empty    {-}
integer              :: nemfac=0        ! num. of faces on empty patches    {-}
integer,allocatable  :: own(:)      ! owner of face / cell p  (ntface)          {-}
integer,allocatable  :: nei(:)      ! neighbour of face / cell n (ntface)        {-}
integer,allocatable  :: lb(:)       ! owner cell of boundary faces (ntbfac)
integer,allocatable  :: ln(:)       ! neighbor cell of coupled boundary face (ntbfac)

integer,allocatable   :: face(:),face_p(:)   ! face data defined by vertex index (ntface) {-}


real(dp),allocatable  :: swvol(:),swvolb(:)   ! face moving volume  (nface)                {m3}

real(dp),allocatable  :: sf(:,:),sb(:,:)      ! surface area vector                  (3,nface)  {m2} 
real(dp),allocatable  :: magsf(:),magsb(:)    ! magnitude of surface vector          (ntface)   {m2}
real(dp),allocatable  :: xf(:,:),xb(:,:)      ! surface center                       (3,nface)  {m}
real(dp),allocatable  :: xfo(:,:),xbo(:,:)    ! old surface center                   (3,ntface) {m}

real(dp),allocatable  :: fx(:),fxb(:)         ! interpolation factor                 (nface)    {-}
real(dp),allocatable  :: dr(:),drb(:)         ! differencing factors                 (nface)    {1/m}

logical               :: nonorth               ! non-orthogonal
real(dp),allocatable  :: corr(:,:),corrb(:,:) ! non-orthogonal correction vector (3,nface)
!
real(dp),allocatable  :: lsp(:,:),lsn(:,:)    ! least square gradient (3,nface)
real(dp),allocatable  :: lspb(:,:),lsnb(:,:)  ! least square gradient (3,nbfac)!
!===============================================================
! coupled face sectiom
!===============================================================
!
integer               :: ncycle =0            ! number of cyclic coupling patch-pair
integer               :: ninter =0            ! number of interface coupling patch-pair
integer,allocatable   :: nspair(:),nepair(:)  ! starting and ending index of face 
integer,allocatable   :: bpair(:,:)           ! coupling face index
!
!===============================================================
! patch section
!===============================================================
!
integer               :: npatch =0            ! number of patches
integer               :: nempatch=0           ! number of empty patches

integer,allocatable   :: nspem(:),nepem(:)    ! empty patch starting and ending index {-}                 

integer,allocatable   :: nsp(:),nep(:)        ! nsp,nep ...boundary face starting and ending index {-}                     
logical,allocatable   :: bcoup(:)             ! coupling flag (npatch)
logical,allocatable   :: bstrain(:)           ! boundary constrained flag (npatch)
logical,allocatable   :: btrans(:)            ! boundary transformation flag (npatch)
integer,allocatable   :: bityp(:)             ! boundary type index (npatch)
character,allocatable :: btype(:)*32          ! boundary type name (npatch)
character,allocatable :: bname(:)*32          ! boundary name (npatch)

real(dp),allocatable  :: brough(:)            ! rough value if wall boundary (npatch)
integer,allocatable   :: bgmot(:)             ! if geometrical moving for wall (npatch) 
integer,allocatable   :: bvmot(:)             ! if effeced by velocity for wall (npatch)

real(dp),allocatable  :: forwardt(:,:)        ! farward transformation matrix (9,npatch)
real(dp),allocatable  :: reverset(:,:)        ! reverse transformation matrix (9,npatch)

real(dp),allocatable  :: sepaxis(:,:)         ! rotation axis or separation (3,npatch)
real(dp),allocatable  :: rotcen(:,:)          ! rotation centre (3,npatch)

real(dp),allocatable  :: cellt(:,:)           ! neighbour-cell transformation tensor for wedge boundary (npatch)
real(dp),allocatable  :: facet(:,:)           ! face transformation tensor (npatch)
!
! cell relation
!
integer,allocatable   :: ccell(:),ccell_p(:)   ! cell-cells  connectivity ccell[ncell] list
integer,allocatable   :: cpoint(:),cpoint_p(:) ! cell-vertexs connectivity cpoint[ncell] list
integer,allocatable   :: pcell(:),pcell_p(:)   ! vertex-cells connectivity pcell[nvrt] list
integer,allocatable   :: pface(:),pface_p(:)   ! vertex-faces connectivity pcell(nvrt) list
!
! edge relation
!
type(list),allocatable :: edges(:)    ! 
type(list),allocatable :: pedge(:)    !
!
!
! ldu relation
!
integer,allocatable    :: lfpl(:)      ! face index collecting lower part of matrix (nface)
integer,allocatable    :: nfpu(:)      ! face index collecting upper part of matrix (ncell+1)
!
!face weight for amg
!
real(dp),allocatable   :: facewt(:)
type(list),allocatable :: restr(:)
!
! zones data
!
end module comm0

!>This function gives the boundary patch which the face locates
!>@param[in]   iface  -- face index
!<
!##############################################################
function whichPatch(iface)
!##############################################################
use comm0,only:nPatch,nface,nsp,nep,nempatch
implicit none
integer,intent(in) :: iface
integer            :: whichPatch

integer :: ir

whichPatch=0
do ir=1,npatch+nempatch
  if(iface>=nface+nsp(ir) .and. iface<=nface+nep(ir)) then
    whichPatch = ir
	exit
  endif
end do
end function

!>This subroutine logs a variable
!>@param[in]   name -- variable name
!>@param[in]   loca -- variable address
!<
!############################################################################
subroutine memlog(name,loca)
!############################################################################
!   name   - name of array          
!   loc    - address of storage
!   n      - number
!============================================================================
use precision,only:ptr
use scheme,only:mxvar,nvar,var_name,var_loc
implicit  none
character(len=*),intent(in) :: name
integer(ptr),intent(in)     :: loca

nvar = nvar + 1

if(nvar>mxvar) return

var_name(nvar) = trim(name)
var_loc(nvar)  = loca
end subroutine memlog

!###########################################################################
subroutine patch(ir,btyname)
!###########################################################################
! ir     : boundary patch index
! btyname: boundary patch type name
! val    : geometrical values related to boundary patch  
!===========================================================================
! At the boundary patch(ir), set the boundary patch type (btyname).
! At the same time some geometrical data or other data relative to
! the boundary patch are also provided.  
!===========================================================================
! patch type         patch type name  additional data
!===========================================================================
! bc_empty          'EMPTY'            n                       
! bc_wedge          'WEDGE'            n                       
! bc_symmetry       'SYMMETRY'         n                       
! bc_cyclic         'CYCLIC'           n                       
! bc_interface      'INTERFACE'        n                       
! bc_wall           'WALL'             motion type 1,motion type 1,rough                       
! bc_inlet          'INLET'            n   (need 
! bc_outlet         'OUTLET'           n
!===========================================================================
! For the following patch, users need set the boundary type for the 
! corresponding partial differential equation.
! bc_wall
! bc_inlet
! bc_outlet
!===========================================================================
use precision,only:dp
use comm0,only:npatch,nempatch,btype,bityp,bcoup,bstrain
use scheme,only:npchtype,btypnam,bc_empty,bc_wedge,bc_symmetry,&
                bc_cyclic,bc_interface, bc_wall,bc_inlet, bc_outlet
implicit none
integer,intent(in)          :: ir
character(len=*),intent(in) :: btyname

integer,external      :: stridx
character*16,external :: itoc

character*16 :: fun ='PATCH'

if(ir<0.or.ir>npatch+nempatch) goto 100

btype(ir)  = trim(btyname)
bityp(ir)  = stridx(btype(ir),npchtype,btypnam)
bcoup(ir)  = .false.
bstrain(ir)= .false.

if(bityp(ir)==0) goto 200

select case(bityp(ir))
  case(bc_wall,&
       bc_inlet,&
       bc_outlet)
    call disp('REGION '//trim(itoc(ir))//&
              ' NEED FURTHER BOUNDARY TYPE FOR DIFFERNTIAL EQUATION') 
end select
              
select case(bityp(ir))
  case(bc_cyclic,&
       bc_interface)
    bcoup(ir) = .true.
end select

select case(bityp(ir))
  case(bc_empty,&
       bc_symmetry,&
       bc_wedge,&
       bc_interface)
    bstrain(ir)=.true.
end select  

return

100 continue
call uinfo(trim(fun),'W','PATCH '//trim(itoc(ir))//' NOT EXISTED')
return

200 continue
call uinfo(trim(fun),'W','PATCH TYPE '//trim(btyname)//' NOT A VALID NAME')
return
end subroutine patch
