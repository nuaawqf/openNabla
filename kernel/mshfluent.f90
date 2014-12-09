!--------------------------*-  OpenNabla  -*--------------------------------!
!    //\                  |OpenNabla: A Fortran90 toolkit for Computational !
!   //  \   C omputational|           Continuous Mechanism (v 1.0)          !
!  //    \  C ontinuous   |Copyright (C) : liuhuafei                        !
! //      \ M echanism    |Email  :   liuhuafei@hotmail.com or              !
! =========               |           LIUHUAFEI@BAOSTEEL.COM                !
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
!! Contains Module fluentbase,which defines data and subroutine to handle binary 
!! fluent case to openNabla mesh. Subroutines include findblock(),xf_msh_get(),
!! xf_msh_wrt()
!<

!> Contains global variables for fluent case conversion
!! Subroutines include readhex()\n
!! To import Fluent .cas files into openNabla, remember
!! Only mesh data is imported. No boundary conditions, model selections, 
!! solver parameters or solution fields are imported.\n
!! All the 2D or 3D mesh of Fluent will be imported as 3d mesh in openNabla.\n
!! All the 2D mesh will be extruded to 3D mesh.\n
!!
!! The Fluent importer in openNabla reads the following sections:\n
!! FLUENT_HEADER_SECTION = 1\n
!! FLUENT_DIMENSION_SECTION = 2\n
!! FLUENT_NODE_SECTION = 10\n
!! FLUENT_CELL_SECTION = 12\n
!! FLUENT_FACE_SECTION = 13\n
!! FLUENT_PERIODIC_SECTION = 18\n
!! FLUENT_INTERFACE_SECTION = 18\n
!! FLUENT_ZONE_SECTION = 39\n
!! FLUENT_NAME_SECTION = 45\n
!!\n
!!The following region, boundary, and interface types are recognized:\n
!!FLUENT_FLUID = 1\n
!!FLUENT_SOLID = 17\n
!!FLUENT_INTERIOR = 2\n
!!FLUENT_WALL = 3\n
!!FLUENT_PRESSURE_INLET = 4\n
!!FLUENT_PRESSURE_OUTLET = 5\n
!!FLUENT_SYMMETRY = 7\n
!!FLUENT_PERIODIC_SHADOW = 8\n
!!FLUENT_PERIODIC = 12\n
!!FLUENT_PRESSURE_FAR_FIELD = 9\n
!!FLUENT_VELOCITY_INLET = 10\n
!!FLUENT_MASS_FLOW_INLET = 20\n
!!FLUENT_INTERFACE = 24\n
!!FLUENT_OUTFLOW = 36\n
!!FLUENT_AXIS = 37\n
!!Boundaries with those types are assigned to equivalent boundary types
!<

!
!############################################################################
module fluentbase
!############################################################################
! a module handing fluent case file
! note : at this time ,boundary axis may not be handled?????
!        need attention   2014/11/14
!============================================================================
implicit none
integer,parameter  :: xf_dpoff     = 3000          ! double precision
integer,parameter  :: xf_spoff     = 2000          ! single precision 
integer,parameter  :: xf_mxpatch   = 100           ! max patch number

integer,parameter  :: xf_mxcel     = 100           ! max cell zones
integer,parameter  :: xf_mxcyc     = 100           ! max cyclic patches
integer,parameter  :: xf_mxint     = 100           ! max interface patches
!
! fluent blocks
!
integer,parameter  :: xf_comment   = 0             ! comment
integer,parameter  :: xf_header    = 1             ! header
integer,parameter  :: xf_dim       = 2             ! dimension
integer,parameter  :: xf_node      = 10            ! node
integer,parameter  :: xf_cell      = 12            ! cell
integer,parameter  :: xf_face      = 13            ! face

integer,parameter  :: xf_periodisd = 18            ! periodisd
integer,parameter  :: xf_zone1     = 39            ! zone (fluent)
integer,parameter  :: xf_zone2     = 45            ! zone (gambit)
integer,parameter  :: xf_celtree   = 58            ! cell tree
integer,parameter  :: xf_factree   = 59            ! face tree
integer,parameter  :: xf_interpair = 61            ! interface's parent face
!
! fluent boundary type
!
! in fluent, periodic pair is divides into two face zone
! one is period, the other is periodsd
! at last, use xf_periodisd explicitly declared

integer,parameter  :: xf_bc_empty              = -1   ! empty
integer,parameter  :: xf_bc_wedge              = -2   ! wedge
integer,parameter  :: xf_bc_interior           = 2    ! interior faces
integer,parameter  :: xf_bc_wall               = 3    ! wall
integer,parameter  :: xf_bc_pressure_inlet     = 4    ! pressure-inlet
integer,parameter  :: xf_bc_pressure_outlet    = 5    ! pressure-outlet
integer,parameter  :: xf_bc_symmetry           = 7    ! symmetry
integer,parameter  :: xf_bc_periodsd           = 8    ! period shadow
integer,parameter  :: xf_bc_pressure_far_field = 9    ! pressure far field
integer,parameter  :: xf_bc_velocity_inlet     = 10   ! velocity inlet
integer,parameter  :: xf_bc_period             = 12   ! period
integer,parameter  :: xf_bc_fan                = 14   ! fan
integer,parameter  :: xf_bc_mass_flow_inlet    = 20   ! mass flow inlet
integer,parameter  :: xf_bc_interface          = 24   ! interface
integer,parameter  :: xf_bc_parent             = 31   ! parent
integer,parameter  :: xf_bc_outflow            = 36   ! outflow
integer,parameter  :: xf_bc_axis               = 37   ! axis
!
! fluent face type
!
integer,parameter  :: xf_fac_mixed    = 0   
integer,parameter  :: xf_fac_line     = 2
integer,parameter  :: xf_fac_tri      = 3
integer,parameter  :: xf_fac_quad     = 4
integer,parameter  :: xf_fac_polygon  = 5
!
! fluent cell type
!
integer,parameter  :: xf_cel_mixed    = 0
integer,parameter  :: xf_cel_tri      = 1
integer,parameter  :: xf_cel_tet      = 2    
integer,parameter  :: xf_cel_quad     = 3
integer,parameter  :: xf_cel_hex      = 4
integer,parameter  :: xf_cel_pyr      = 5
integer,parameter  :: xf_cel_wedge    = 6
integer,parameter  :: xf_cel_polyhed  = 7

integer,parameter  :: xf_hanging      = 3

integer            :: xf_regid(xf_mxpatch)
logical            :: xf_inter(xf_mxpatch)
logical            :: xf_cyc(xf_mxpatch)

character(len=16)  :: xf_ftype(0:5)
character(len=16)  :: xf_ctype(0:7)

character(len=16)  :: xf_btype(-2:37)

data xf_btype /'WEDGE','EMPTY',      '',   &
               '',     'INTERIOR',   'WALL',  'INLET',     'OUTLET',&
               '',     'SYMMETRY',   'CYCLIC','INLET',     'INLET',&
               '',     'CYCLIC',     '',      'INLET',     '',&
               '',     '',           '',      '',          'INLET',& 
               '',     '',           '',      'INTERFACE', '',&
               '',     '',           '',      '',          '',&
               '',     '',           '',      '',          '',&
               'OUTLET','EMPTY'/           

data xf_ftype /'MIXED','','LINEAR','TRIANGULAR','QUADRILATERAL','POLYGONAL'/
data xf_ctype /'MIXED','TRIANGULAR','TETRAHEDRAL','QUADRILATERAL','HEXAHEDRAL',&
               'PYAMID','WEDGE','POLYHEDRAL'/               
contains

!>This subroutine reads hex from a string
!>@param[in]    str  -- input string
!>@param[out]   ival -- integer array 
!>@param[out]   ino  -- number of integer values which are read 
!<
!#############################################################################
subroutine readhex(str,ival,ino)
!#############################################################################
! read hex integer from str
! ival :: integer array 
! ino  :: number of integer
!=============================================================================
implicit none
character(len=*),intent(in) :: str
integer,intent(out)         :: ival(:)
integer,intent(out)         :: ino

integer            :: ipos1,ipos2,ilen
character(len=256) :: hexstr

ipos1 = 1
ilen  = len_trim(str)

if(ilen<ipos1) return

ival  = 0
ino   = 0
do while(.true.)
   ipos2 = index(str(ipos1:ilen),' ')+ipos1-1
   if(ipos2<ipos1) ipos2=ilen

   hexstr= str(ipos1:ipos2)
   ino   = ino+1

   read(hexstr,fmt='(z16)') ival(ino)

   ipos1 = ipos2+1 

   if(ipos2==ilen) exit
end do
end subroutine readhex
end module fluentbase

               
!>This subroutine finds a section header from fluent case file
!>@param[in]    ifile  -- case file unit
!>@param[out]   id     -- section id 
!>@param[out]   str    -- a string excluding id
!>@param[out]   ierr   -- return error code 
!<
!############################################################################
subroutine findblock(ifile,id,str,ierr)
!############################################################################
! find section header for fluent case and mesh file, get id and value 
!============================================================================
implicit none
integer,intent(in)    :: ifile
integer,intent(out)   :: id
character,intent(out) :: str*256
integer,intent(out)   :: ierr

integer   :: ipos1,ipos2,i1,i2,i
character :: cdummy*256

integer,parameter :: nvalid         = 20
integer,parameter :: validid(nvalid)= (/0,1,2,10,12,13,18,39,45,61,&
                                        2012,2013,2010,2018,2061,&
                                        3010,3012,3013,3018,3061/)
!    0,   1,   2,   39,  45,
!    10,  12,  13,  18  
!  2010,2012,2013,2018 single precsion
!  3010,3012,3013,3018 double precision

do 
  call getline(ifile,cdummy,ierr)
  
  if(ierr/=0) exit
  
  ipos1=index(cdummy,"(")
  ipos2=index(cdummy," ")
  
  if((ipos2-1)<(ipos1+1)) cycle
  
  read(cdummy(ipos1+1:ipos2-1),'(i10)',iostat=ierr) id
  if(ierr/=0) cycle
  
  do i=1,nvalid
    if(id==validid(i)) exit
  end do
  if(i>nvalid) cycle
    
  select case(id)
    case(0,1)
      i1=index(cdummy,'"')
      i2=index(cdummy,'"',back=.true.)
    case(2)
      i1=ipos2
      i2=index(cdummy,')')
    case default
      i1=index(cdummy(ipos2:),'(')+ipos2-1
      i2=index(cdummy,')')
  end select
  str = cdummy(i1+1:i2-1)
  exit
end do  
  
end subroutine findblock
               

!>This subroutine reads fluent case file 
!>@param[in]    ifile   -- case file unit
!>@param[in]    axisymm -- if mesh is axisymm 
!<
!############################################################################
subroutine xf_msh_get(ifile,axisymm)
!############################################################################
use listlib,only:list_create,list_reverse
use fluentbase
use meshbase
implicit none
integer,intent(in) :: ifile
logical,intent(in) :: axisymm

integer   :: isect,ierr,i,ii,nn,itemp,ival(20),nval,if1,if2,&
             id,ir,ist,ien,idim,ibtype,iftype,ietype,iperz,ipersdz
logical   :: found,bsingle
character :: str*256,stemp1*64,stemp2*64

axisymm_ = axisymm

do while(.true.)
  call findblock(ifile,isect,str,ierr)
  if(ierr/=0) exit

  select case(isect)
!
!....  comment:      
!....  ex.      (0 "comment text")
!....           (0 "variables:")
!
     case(xf_comment)     
	   write(*,'(2(a))') 'COMMENT: ',trim(str)
!
!   header:       
!... ex.        (1 "tgrid 2.1.1")
!	 
     case(xf_header)
	   write(*,'(2(a))') 'HEADER: ',trim(str)
!
!   dimensions:    
!... ex.        (2 nd)
!
     case(xf_dim)
       read(str,'(i16)',iostat=ierr) ndim_
       if(ierr/=0) goto 100
	   write(*,'(a,i0)') 'DIMENSIONS OF GRID: ',ndim_
!
!   nodes    :
!... ex.    (10 (zone-id first-index last-index type nd)(
!              x1 y1 z1
!              x2 y2 z2
!              .
!            ))
!
! if zone-id is zero, this provides the total number of nodes in the grid.
! first-index will then be one, last-index will be the total number of nodes in hexadecimal, 
! type is equal to 1, ndis the dimensionality of the grid, and there are no coordinates 
! following (the parentheses for the coordinates are omitted as well).
!
!... ex.    (10 (0 1 2d5 1 2))
!           zone-id = 0   
!           last-index---- number of nodes
!           type=0
!           nd  =dimension
!
! if zone-id is greater than zero, it indicates the zone to which the nodes belong. 
! first-index and last-index are the indices of the nodes in the zone, in hexadecimal. 
! the values of last-index in each zone must be less than or equal to the value in
! the declaration section. type is always equal to 1. 
! nd is an optional argument that indicates the dimensionality of the node data, 
! where nd is 2 or 3.
! zone-id and type are also in hexadecimal
!... ex.  
!           (10 (zone-id first-index last-index type nd)(
!               x1 y1 z1
!               x2 y2 z2
!               .  .  .
!           ))
!           type always is 1; nd is 2 or 3
!
     case(xf_node)
		call readhex(str,ival,nval)
		
		id   = ival(1)
		ist  = ival(2);		ien  = ival(3)
		idim = ival(5)
 	    
		if(id==0) then
		   point_%name = 'GRID COORDINATES'
		   point_%ndim = idim
		   point_%nsize= ien-ist+1
		   allocate(point_%rd(idim,ist:ien),stat=ierr)
  	       write(*,'(a,i0)') 'NUMBER OF POINTS: ',point_%nsize
  	       
  	       ndim_ = idim
  	       nvrt_ = point_%nsize
        endif
  	 case(xf_node+xf_dpoff,xf_node+xf_spoff)
		call readhex(str,ival,nval)
		id   = ival(1)
		ist  = ival(2);		ien  = ival(3)
		idim = ival(5);
		
		bsingle = .false.
		if(isect==(xf_node+xf_spoff)) bsingle = .true.
		  	    
  	    write(*,'(a,i0,a,i0,1x,i0,a)') 'READING ',ist,' to ',ien,idim,'D POINT COORDINATES'

        call findc(ifile,'(',.false.,ierr)
        if(ierr/=0) goto 200
        
        call getreal(ifile,idim,nvrt_,point_%rd,ist,ien,bsingle,ierr)

        call findc(ifile,')',.false.,ierr)
        if(ierr/=0) goto 200
!
!...   faces 
!...   ex.  (13 (zone-id first-index last-index bc-type face-type))
!...       zone-id = zone id of the face section
!...       first-index = index of the first face in the list
!...       last-index = index of the last face in the list
!...       bc-type = id of the boundary condition represented by the face section
!...       face-type = id of the type(s) of face(s) in the section
!
!  bc-type description
!   2 interior                                   !   3 wall
!   4 pressure-inlet, inlet-vent, intake-fan
!   5 pressure-outlet, exhaust-fan, outlet-vent
!   7 symmetry                                  !   8 periodic-shadow
!   9 pressure-far-field                        !  10 velocity-inlet
!  12 periodic                                  !  14 fan, porous-jump, radiator
!  20 mass-flow-inlet                           !  24 interface
!  31 parent (hanging node)                     !  36 outflow
!  37 axis
! face-type description nodes/face
!  0 mixed                 !  2 linear 2
!  3 triangular 3          !  4 quadrilateral 4
!  5 polygonal nn
!
      case(xf_face)
	    call readhex(str,ival,nval)
		
		id     = ival(1)
		ist    = ival(2);  		ien    = ival(3)
		ibtype = ival(4);		iftype = ival(5)
		
		if(id==0) then
		  face_%name = 'FACE DEFINITION'
		  face_%ndim = 1
		  face_%nsize= ien-ist+1
		  allocate(face_%ld(ist:ien),stat=ierr)

		  own_%name = 'OWN'
		  own_%ndim = 1
		  own_%nsize= ien-ist+1
		  allocate(own_%id(1,ist:ien),stat=ierr)

		  nei_%name = 'NEI'
		  nei_%ndim = 1
		  nei_%nsize= ien-ist+1
		  allocate(nei_%id(1,ist:ien),stat=ierr)
		  
		  patch_%name = 'PATCH'
		  patch_%ndim = 1
		  patch_%nsize= xf_mxpatch
		  allocate(patch_%id(6,xf_mxpatch),stat=ierr)
          allocate(patch_%sd(2,xf_mxpatch),stat=ierr)
          
          nreg0_ = 0
          nface_ = 0
          nbfac_ = 0
  	      write(*,'(a,i0)') 'NUMBER OF FACES: ',ien-ist+1
  	    endif
      case(xf_face+xf_dpoff,xf_face+xf_spoff)
	    call readhex(str,ival,nval)
		
		id     = ival(1)
		ist    = ival(2);  		ien    = ival(3)
		ibtype = ival(4);		iftype = ival(5)
		!
		!ibtype>1000 the faces will be generated by non-conformed interface 
		!
		if(ibtype>1000) then
    	  write(*,'(a,i0,1x,i0,1x,a)') 'READING ',ist,ien,'NON-CONFORMED FACES,WILL BE NEGECTED'
		  nface0_ = nface0_ + (ien-ist+1)
 	      call findc(ifile,'(',.false.,ierr)
          if(ierr/=0) goto 200
 	      call findc(ifile,')',.false.,ierr)
          if(ierr/=0) goto 200		
		  cycle
        endif
        
        select case(ibtype)
	      case(xf_bc_interior)
	        nface_ = nface_ + (ien-ist+1)
	      case(xf_bc_wall,xf_bc_pressure_inlet,xf_bc_pressure_outlet,&
	           xf_bc_symmetry,xf_bc_periodsd,xf_bc_pressure_far_field,&
	           xf_bc_velocity_inlet,xf_bc_period,xf_bc_fan,&
	           xf_bc_mass_flow_inlet,xf_bc_interface,&
	           xf_bc_outflow,xf_bc_axis)
            nbfac_ = nbfac_ + (ien-ist+1)
	        nreg_  = nreg_ + 1
	      case(xf_bc_parent)
	        cycle
	    end select  
	    
        nreg0_              = nreg0_  +1
	    patch_%id(1,nreg0_) = id
	    patch_%id(2,nreg0_) = ibtype
        patch_%id(3,nreg0_) = ist
	    patch_%id(4,nreg0_) = ien
	    
  	    write(*,'(a,i0,1x,i0,1x,2a)') 'READING ',ist,ien,trim(xf_ftype(iftype)),' FACES'
!
        select case(iftype)
		  case(xf_fac_line)
		    nn = 2
	      case(xf_fac_tri)
	        nn = 3
	      case(xf_fac_quad)
	        nn = 4
	      case(xf_fac_polygon,xf_fac_mixed)
	        nn = 0   
	    end select
	    
	    call findc(ifile,'(',.false.,ierr)
        if(ierr/=0) goto 200
!
! for fluent polyhedral mesh, nei comes first
!
	    call getface(ifile,face_%nsize,&
		             face_%ld(:),nei_%id(1,:),own_%id(1,:),&
		             ist,ien,nn,ierr)
		
        call findc(ifile,')',.false.,ierr)
        if(ierr/=0) goto 200
        
!
!This section indicates the pairings of periodic faces on periodic boundaries.
!Grids without periodic boundaries do not have sections of this type.
!
!(18 (first-index last-index periodic-zone shadow-zone)(
!f00 f01
!f10 f11
!f20 f21
!.
!))
!where
! first-index = index of the first periodic face pair in the list
! last-index = index of the last periodic face pair in the list
! periodic-zone = zone id of the periodic face zone
! shadow-zone   = zone id of the corresponding shadow face
!
! these are in hexadecimal format
!(18 (1 2b a c) (
!12 1f
!13 21
!.
!))
      case(xf_periodisd+xf_dpoff,xf_periodisd+xf_spoff)
	    call readhex(str,ival,nval)
		
		ist  = ival(1);  		ien     = ival(2)
		iperz = ival(3);		ipersdz = ival(4)
		
		if(cycle_%nsize==0) then
		  cycle_%name = 'CYCLIC BOUNDARY'
		  cycle_%ndim = 2
		  cycle_%nsize= xf_mxcyc
		  allocate(cycle_%ld(xf_mxcyc),stat=ierr)
		  allocate(cycle_%id(1,xf_mxcyc),stat=ierr)
		  ncyc_=0		   
		endif

        ncyc_=ncyc_+1

        nn   =ien-ist+1

        cycle_%id(1,ncyc_) = nn
                
        call list_create(cycle_%ld(ncyc_),2*nn)
        
        call findc(ifile,'(',.false.,ierr)
        if(ierr/=0) goto 200
        
        call getint(ifile,2,nn,cycle_%ld(ncyc_)%ind,1,nn,ierr)        

        call findc(ifile,')',.false.,ierr)
        if(ierr/=0) goto 200
      case(xf_interpair+xf_dpoff,xf_interpair+xf_spoff)
	    call readhex(str,ival,nval)
		
		ist  = ival(1);  		ien     = ival(2)

		if(inter_%nsize==0) then
		  inter_%name = 'INTERFACE BOUNDARY'
		  inter_%ndim = 2
		  inter_%nsize= xf_mxint
		  allocate(inter_%ld(xf_mxint),stat=ierr)
		  allocate(inter_%id(1,xf_mxint),stat=ierr)
		  ninter_=0		   
		endif

        ninter_=ninter_+1

        nn   =ien-ist+1

        inter_%id(1,ninter_) = nn
                
        call list_create(inter_%ld(ninter_),2*nn)
        
        call findc(ifile,'(',.false.,ierr)
        if(ierr/=0) goto 200
        
        call getint(ifile,2,nn,inter_%ld(ninter_)%ind,1,nn,ierr)
        
        call findc(ifile,')',.false.,ierr)
        if(ierr/=0) goto 200        
!
!  cells
!        (12 (zone-id first-index last-index type element-type))
!        (12 (0 1 3e3 0))
!        (12 (9 1 3d 0 0)(
!             1 1 1 3 3 1 1 3 1
!             .
!           ))
! 
! zone-id is zero to indicate that it is a declaration of the total number 
! of cells. if last-index is zero, then there are no cells in the grid.
! type indicates whether the cell zone is an active zone (solid or fluid), 
! or inactive zone
! element-type description nodes/cell faces/cell
!     0         mixed
!     1         triangular    3        3
!     2         tetrahedral   4        4
!     3         quadrilateral 4        4
!     4         hexahedral    8        6
!     5         pyramid       5        5
!     6         wedge         6        5
!     7         polyhedral    nn       nf
!
!if a zone is of mixed type
!(element-type=0), it will have a body that lists the element-type of each cell.
!example:
!(12 (9 1 3d 0 0)(
!1 1 1 3 3 1 1 3 1
!.
!))
      case(xf_cell)
	    call readhex(str,ival,nval)
		
		id  = ival(1)
		ist = ival(2);     ien   = ival(3)
		ibtype = ival(4);  ietype= ival(5) 
		
		if(id==0) then  ! a declaration of the total number of cells
  	       write(*,'(a,i0)') 'NUMBER OF CELLS: ',ien-ist+1
  	       cell_%name = 'CELL'
  	       allocate(cell_%id(3,xf_mxcel),stat=ierr)
  	       allocate(cell_%sd(2,xf_mxcel),stat=ierr)
  	       ncel_       = ien-ist+1
  	       cell_%ndim  = 1  
  	       cell_%nsize = ncel_
  	       
  	       nczon_ = 0  	       
  	    endif
      case(xf_cell+xf_dpoff,xf_cell+xf_spoff)
  	    call readhex(str,ival,nval)
		
		id  = ival(1)
		ist = ival(2);     ien   = ival(3)
		ibtype = ival(4);  ietype= ival(5)
		
	    nczon_ = nczon_ +1
		cell_%id(1,nczon_) = id
		cell_%id(2,nczon_) = ist
		cell_%id(3,nczon_) = ien	
  	    write(*,'(a,i0,1x,i0,1x,2a)') 'READING ',ist,ien,trim(xf_ctype(ietype)),' CELLS'
!
!(39 (4 velocity-inlet innerwall 1)(
!
      case(xf_zone1,xf_zone2)
        read(str,*) id,stemp1,stemp2
        found = .false.
        do i=1,nreg0_
          if(patch_%id(1,i)==id) then
            patch_%sd(1,i) =stemp2
            patch_%sd(2,i) =stemp1
            found = .true.
            exit
          endif
        end do
        if(.not.found) then
          do i=1,nczon_
            if(cell_%id(1,i)==id) then
              cell_%sd(1,i) =stemp2
              cell_%sd(2,i) =stemp1
              exit
            endif
          end do
        endif
        
  end select
end do

close(ifile)

!
!  flip face
!    
do i=nface0_+1,nface0_+nface_+nbfac_
  if(own_%id(1,i)==0 .or. (nei_%id(1,i)/=0 .and. own_%id(1,i)>nei_%id(1,i))) then
    itemp        = nei_%id(1,i)
    nei_%id(1,i) = own_%id(1,i)
    own_%id(1,i) = itemp
	call list_reverse(face_%ld(i))
  endif
enddo
!
! extruding the 2d to 3d
!
call extrude2d    
!
! ordering the internal and boundary face patch
!
ir = 0

do i=1,nreg0_          ! 1st: interior faces
  if(patch_%id(2,i)/=xf_bc_interior) cycle
  ir        = ir + 1
  xf_regid(ir)  = i
end do

do i=1,nreg0_          ! 2nd: common boundary faces
  select case(patch_%id(2,i))
	case(xf_bc_wall,xf_bc_pressure_inlet,xf_bc_pressure_outlet,&
	     xf_bc_symmetry,xf_bc_pressure_far_field,&
	     xf_bc_velocity_inlet,xf_bc_mass_flow_inlet,&
	     xf_bc_outflow,xf_bc_axis)
     ir        = ir +1
     xf_regid(ir)  = i
  end select 
end do  

do i=1,nreg0_          ! 3rd: wedge boundary faces
  select case(patch_%id(2,i))
	case(xf_bc_wedge)
     ir        = ir +1
     xf_regid(ir) = i
  end select 
end do  

xf_inter = .false.
do i=1,nreg0_          ! 4th: interface boudary faces
  select case(patch_%id(2,i))
	case(xf_bc_interface)
     ir        = ir +1
     xf_regid(ir)  = i
     xf_inter(i)   = .true.
  end select 
end do  

xf_cyc = .false.
do i=1,nreg0_          ! 5th: cyclic region
  select case(patch_%id(2,i))
	case(xf_bc_period)
     ir        = ir +1
     xf_regid(ir)  = i
     xf_cyc(i)     = .true.
  end select 
end do  
do i=1,nreg0_          ! 5th: cyclic shadow region
  select case(patch_%id(2,i))
	case(xf_bc_periodsd)
     ir        = ir +1
     xf_regid(ir)  = i
     xf_cyc(i)     = .true.
  end select 
end do  

do i=1,nreg0_          ! 6th : empty boundary faces
  select case(patch_%id(2,i))
	case(xf_bc_empty)
     ir        = ir +1
     xf_regid(ir) = i
  end select 
end do
return

100  continue
close(ifile)
call uinfo('XF_MSH_GET','E','DIMENSION ERROR')
return

200 continue
close(ifile)
call info('XF_MSH_GET','E',ierr)
return
end subroutine xf_msh_get

!>This subroutine write fluent case into a openNabla mesh file
!>@param[in]    mfile -- mesh file unit
!>@param[in]    bfile -- boundary file unit 
!<
!############################################################################
subroutine xf_msh_wrt(mfile,bfile)
!############################################################################
use precision,only:delim
use fluentbase
use meshbase
use listlib,only:list_len
implicit none
integer,intent(in) :: mfile
integer,intent(in) :: bfile

integer   :: i,j,ii,jj,ist,ien,ir,nst,if1,if2,nn,ierr,ip1,ip2
character :: str*256
!
call disp('WRITING POINTS.....')

call wrtline(mfile,'# POINT COORDINATES')
write(str,'(i10,1x,i10)') ndim_,nvrt_
call wrtline(mfile,str)
call wrtline(mfile,'(')
call wrtreal(mfile,ndim_,nvrt_,point_%rd,1,nvrt_)
call wrtline(mfile,')')
!
call disp('WRITING FACE BASED TOPOLOGY......')

ntvfac_ = 0
do i=nface0_+1,nface0_+nface_+nbfac_
  ntvfac_ = ntvfac_ + list_len(face_%ld(i))
end do
call wrtline(mfile,'# FACES BASE TOPOLOGY')
write(str,'(i10,1x,i10)') ntvfac_,nface_+nbfac_
call wrtline(mfile,str)
call wrtline(mfile,'(')

nst = 0
do i=1,nreg0_
  ir  = xf_regid(i)
  
  ist = patch_%id(3,ir);   ien = patch_%id(4,ir)
  
  call wrtface(mfile,nface0_+nface_+nbfac_,face_%ld(:),own_%id(1,:),nei_%id(1,:),ist,ien)
  
  patch_%id(5,ir) = nst+1; patch_%id(6,ir) = nst+ien-ist+1
  nst             = nst+ien-ist+1
end do
call wrtline(mfile,')')

!
! -----  write cell and boundary data
!
call disp('WRITING CELLS AND BOUNDARIES ......')

call wrtline(bfile,'# CELLS')
write(str,'(i0)') nczon_
call wrtline(bfile,str)

call wrtline(bfile,'(')
do i=1,nczon_
  call wrtline(bfile,adjustl(cell_%sd(1,i)))

  call wrtline(bfile,'{')

  write(str,'(t5,a)')    'TYPE '//trim(cell_%sd(2,i))
  call wrtline(bfile,str)

  write(str,'(t5,a,i0)') 'NCELLS ',cell_%id(3,i)-cell_%id(2,i)+1
  call wrtline(bfile,str)
  
  write(str,'(t5,a,i0)')   'STARTCELL ',cell_%id(2,i)
  call wrtline(bfile,str)

  call wrtline(bfile,'}')
end do
call wrtline(bfile,')')

call wrtline(bfile,'# BOUNDARIES')
write(str,'(i0)') nreg_
call wrtline(bfile,str)
call wrtline(bfile,'(')

do i=nreg0_-nreg_+1,nreg0_
  ir  = xf_regid(i)
  
  ist = patch_%id(5,ir);  ien = patch_%id(6,ir)
  
  call wrtline(bfile,adjustl(patch_%sd(1,ir)))
  
  call wrtline(bfile,'{')
  
  write(str,'(t5,a)')      'TYPE '//trim(xf_btype(patch_%id(2,ir)))
  call wrtline(bfile,str)
  
  write(str,'(t5,a,i0)')   'NFACES ',   ien-ist+1
  call wrtline(bfile,str)

  write(str,'(t5,a,i0)')   'STARTFACE ',ist
  call wrtline(bfile,str)
  
  call wrtline(bfile,'}')
end do

call wrtline(bfile,')')

close(bfile)


call disp('WRITING INTERFACE AND CYCLIC PAIRS.....')

call wrtline(mfile,'# CYCLIC AND INTERFACE PAIRS')
write(str,'(i0,1x,i0)') ncyc_,ninter_
call wrtline(mfile,str)

if(ncyc_<=0.and.ninter_<=0) then
  close(mfile)
  return
endif  

do i=1,ncyc_
  write(str,'(i0,1x,i0)') 2,cycle_%id(1,i)
  call wrtline(mfile,str)
  
  ip1 = cycle_%ld(i)%ind(1)
  loopA1: do if1=1,nreg0_
     if(.not. xf_cyc(if1)) cycle
     if(ip1>=patch_%id(3,if1) .and. ip1<=patch_%id(4,if1)) then
       exit loopA1
     endif
  end do loopA1

  ip2 = cycle_%ld(i)%ind(2)
  loopB1: do if2=1,nreg0_
     if(.not. xf_cyc(if2)) cycle
     if(ip2>=patch_%id(3,if2) .and. ip2<=patch_%id(4,if2)) then
       exit loopB1
     endif
  end do loopB1

  do j=1,cycle_%id(1,i)
    ist = patch_%id(3,if1); ien = patch_%id(5,if1)
    cycle_%ld(i)%ind(2*j-1) = cycle_%ld(i)%ind(2*j-1)+ien-ist

    ist = patch_%id(3,if2); ien = patch_%id(5,if2)
    cycle_%ld(i)%ind(2*j)   = cycle_%ld(i)%ind(2*j)  +ien-ist
  end do
  
  call wrtline(mfile,'(')
  call wrtint(mfile,2,cycle_%id(1,i),cycle_%ld(i)%ind,1,cycle_%id(1,i))
  call wrtline(mfile,')')
enddo

do i=1,ninter_
  write(str,'(i0,1x,i0)') 2,inter_%id(1,i)
  call wrtline(mfile,str)
  
  ip1 = inter_%ld(i)%ind(1)
  loopA2: do if1=1,nreg0_
     if(.not. xf_inter(if1)) cycle
     if(ip1>=patch_%id(3,if1) .and. ip1<=patch_%id(4,if1)) then
       exit loopA2
     endif
  end do loopA2

  ip2 = inter_%ld(i)%ind(2)
  loopB2: do if2=1,nreg0_
     if(.not. xf_inter(if2)) cycle
     if(ip2>=patch_%id(3,if2) .and. ip2<=patch_%id(4,if2)) then
       exit loopB2
     endif
  end do loopB2

  do j=1,inter_%id(1,i)
    ist = patch_%id(3,if1); ien = patch_%id(5,if1)
    inter_%ld(i)%ind(2*j-1) = inter_%ld(i)%ind(2*j-1)+ien-ist

    ist = patch_%id(3,if2); ien = patch_%id(5,if2)
    inter_%ld(i)%ind(2*j)   = inter_%ld(i)%ind(2*j)  +ien-ist
  end do
  
  call wrtline(mfile,'(')
  call wrtint(mfile,2,inter_%id(1,i),inter_%ld(i)%ind,1,inter_%id(1,i))
  call wrtline(mfile,')')
enddo
close(mfile)

return
end subroutine xf_msh_wrt
