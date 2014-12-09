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
!! Contains Module scheme, which defines basic parameters to numeric calculation 
!<

!############################################################################
module scheme
!############################################################################
!  definition of common used parameters or variales
!============================================================================
use precision,only:dp,ptr
implicit none
public
save
!
real(dp),parameter  :: boltzmann  = 5.67e-8  ! W/m2.K4
!
! max points    for a cell
! max points    for a face
! max cells     for a point
! max neighbour for a cell
integer,parameter   :: maxpcel     = 12      !< max. points per cell
integer,parameter   :: maxfpnt     = 96      !< max. points per face
integer,parameter   :: maxcpnt     = 36      !< max. cells per point
integer,parameter   :: maxcnei     = 100	 !< max. neighbous per cell 
!
!
! ccm version
!
character           :: ccm_file*8 = 'NABLA'  
character           :: ccm_ver*32 = '(v0.01.00, 16-07-2013)'
!
!
! directory and file type
!
character           :: casedir*256           !< case directory
character           :: casename*32           !< case name
character           :: application*32        !< application name
logical             :: binary     = .false.  !< file format
integer             :: file_ilog  = 1000     !< log file unit
character           :: file_log*3 = 'log'    !< log file name
!
! model parameters.
!
logical             :: restart   =.false.    !< if restart calculation
!
! time option parameters.
!
integer, parameter  :: time_origin  =1
integer, parameter  :: time_previous=2
integer, parameter  :: time_current =3
!
! grid connectivity related parameters.
!
integer, parameter  :: cell_tri     = 1
integer, parameter  :: cell_quad    = 2
integer, parameter  :: cell_tet     = 3
integer, parameter  :: cell_pyramid = 4
integer, parameter  :: cell_prism   = 5
integer, parameter  :: cell_hex     = 6
integer, parameter  :: cell_poly    = 7
!
! face type
!
integer, parameter  :: face_interior = 1
integer, parameter  :: face_interface= 2
integer, parameter  :: face_boundary = 3 
!
! face shape
!
integer, parameter  :: face_line = 1
integer, parameter  :: face_tri  = 2
integer, parameter  :: face_quad = 3
integer, parameter  :: face_poly = 4
!
! variable manager
!
integer,parameter   :: mxvar  = 100          !<max. variables    
integer             :: nvar   = 0            !<number of variables
character*16        :: var_name(mxvar)       !<variable names
integer(ptr)        :: var_loc(mxvar)        !<variable address

!
! the scheme of general boundary condition
!
integer, parameter  :: npchtype    = 10      !<number of patch types
integer, parameter  :: nbctype     = 23      !<number of boundary types
integer, parameter  :: nvctype     = 4       !<number of vc types
!
! general volume conditions
!
integer, parameter  :: vc_fluid         = 1  !<fluid vc
integer, parameter  :: vc_solid         = 2  !<solid vc
integer, parameter  :: vc_gas           = 3  !<gas vc
integer, parameter  :: vc_porosity      = 4  !<porosity vc

character,parameter :: ctypnam(nvctype)*16 =[&
                       'FLUID           ','SOLID           ',&
                       'GAS             ','POROSITY        ' ]
!
! general boundary conditions
!
integer, parameter  :: bc_empty         = 1  !<empty patch
integer, parameter  :: bc_wedge         = 2  !<wedge patch
integer, parameter  :: bc_symmetry      = 3  !<symmetry patch
integer, parameter  :: bc_cyclic        = 4  !<cyclic patch
integer, parameter  :: bc_interface     = 5  !<interface patch
integer, parameter  :: bc_wall          = 6  !<wall patch
integer, parameter  :: bc_inlet         = 7  !<inlet patch
integer, parameter  :: bc_outlet        = 8  !<outlet patch
!
! boundary condition for pde
!
integer, parameter  :: bc_calculated    = 9  !<calculated boundary       
integer, parameter  :: bc_default       = 10 !<default boundary
integer, parameter  :: bc_zerograd      = 11 !<zero gradient boundary      
integer, parameter  :: bc_fixvalue      = 12 !<fixe value boundary
integer, parameter  :: bc_fixflux       = 13 !<fixec flux boundary
integer, parameter  :: bc_tottemp       = 14 !<total temperature boundary
integer, parameter  :: bc_totpres       = 15 !<total pressure boundary
integer, parameter  :: bc_fixmass       = 16 !<fixed mass boundary
integer, parameter  :: bc_backflow      = 17 !<back flow boundary
integer, parameter  :: bc_newton        = 18 !<Newton convection boundary
integer, parameter  :: bc_extrad        = 19 !<external radiation boundary
integer, parameter  :: bc_mixed         = 20 !<convection and radiation boundary
integer, parameter  :: bc_userbc1       = 21
integer, parameter  :: bc_userbc2       = 22
integer, parameter  :: bc_userbc3       = 23

character,parameter :: btypnam(nbctype)*16 =[&
                       'EMPTY           ','WEDGE           ',&
                       'SYMMETRY        ','CYCLIC          ',&
                       'INTERFACE       ','WALL            ',&
                       'INLET           ','OUTLET          ',&
                       'CALCULATED      ','DEFAULT         ',&
                       'ZEROGRAD        ','FIXVALUE        ',&
                       'FIXFLUX         ','TOTTEMP         ',&
                       'TOTPRES         ','FIXMASS         ',&
                       'BACKFLOW        ','NEWTON          ',&
                       'EXTRAD          ','MIXED           ',&
                       'USERBC1         ','USERBC2         ',&
                       'USERBC3         ' ]

!
! mesh quality checking
!
real(dp),parameter  :: chk_closed   = 1.0e-6       !< if mesh is closed
real(dp),parameter  :: chk_aspect   = 1000.0d0     !< mesh aspect
real(dp),parameter  :: chk_nonorth  = 70.0d0       !< mesh non-orthogonality
real(dp),parameter  :: chk_skew     = 4.0d0        !< mesh skewness
real(dp),parameter  :: chk_nonconvex= 10.0d0       !< mesh non-convex
real(dp),parameter  :: chk_flatness = 0.5d0        !< mesh flatness
logical             :: chk_renumber = .false.      !< if mesh need renumber to reduce bandwidth
logical             :: chk_export   = .false.	   !					
!
! the scheme of gradient calculation
!
integer, parameter  :: grad_gauss      = 1         !<gauss gradient method
integer, parameter  :: grad_least      = 2         !<least square gradient method
!
! the scheme of interpolation of cell to face
!
integer, parameter  :: interp_linear   = 1         !<linear interpolation
integer, parameter  :: interp_midpoint = 2         !<middle point interpolation
integer, parameter  :: interp_harmonic = 3         !<harmonic interpolation
!
! field type
!
integer, parameter  :: field_vol     = 1
integer, parameter  :: field_surface = 2
integer, parameter  :: field_point   = 3
!
! the time scheme
!
integer             :: starttime     = 0           !<starting time
integer             :: endtime       = 0           !<ending time
integer             :: iter          = 0           !<iterations
real(dp)            :: time          = 0.0         !<time
real(dp)            :: time_test(100)= 0.0          
real(dp)            :: dt,dt0                      !<delta time,old delta time
real(dp)            :: dtmax                       !<max. delta time
logical             :: adjustdt     = .false.      !<if adjust delta time
real(dp)            :: maxco        = 0.5          !<max corant number
integer             :: writeinterval= 1            !<write interval
!
! transient schemes
!
integer,parameter   :: ddt_euler    = 1            !<euler scheme for d/dt
integer,parameter   :: ddt_backward = 2            !<2nd backward scheme for d/dt
integer,parameter   :: ddt_steady   = 3            !<steady
integer,parameter   :: d2dt2_euler  = 1            !<euler scheme for d/dt2
integer,parameter   :: d2dt2_steady = 2            !<steady
!
! the nonorthogonal correction
!
integer             :: nonorth_ncorr= 1            !<number of non-orthogonal correction iterations
real(dp)            :: sngrad_fac   = 0.5          
!
! the scheme of solution to linear algrebraic equation
!
integer, parameter  :: solv_iccg     = 1           !<ICCG solver
integer, parameter  :: solv_bicg     = 2           !<BICG solver
integer, parameter  :: solv_gmres    = 3           !<GMRES solver
integer, parameter  :: solv_gamg     = 4           !<GAMG solver

integer, parameter  :: prec_dilu     = 1           !<DILU preconditioner
integer, parameter  :: prec_gamg     = 2           !<GAMG preconditioner

real(dp)            :: solv_conv     = 0.001       !<convergence certerior
integer             :: solv_iter     = 0           !<iteration
!
! the schemes of convective flux
!
integer, parameter  :: div_upwind    = 1           !<UPWIND scheme
integer, parameter  :: div_cds       = 2           !<CDS scheme
integer, parameter  :: div_blend     = 3           !<blend scheme
!
! the schemes of convective flux (tvd)
!
integer, parameter  :: div_minmod    = 4           !<MINMOD TVD scheme
integer, parameter  :: div_muscle    = 5           !<MUSCLE TVD scheme
integer, parameter  :: div_umist     = 6           !<UMIST TVD scheme
integer, parameter  :: div_ospre     = 7           !<OSPRE TVD scheme
integer, parameter  :: div_vanalbada = 8           !<VANALBADA TVD scheme
integer, parameter  :: div_superbee  = 9           !<SUPERBEE TVD scheme
integer, parameter  :: div_vanleer   = 10          !<VANLEER TVD scheme
integer, parameter  :: div_osher     = 11          !<OSHER TVD scheme
integer, parameter  :: div_smart     = 12          !<SMART TVD scheme
integer, parameter  :: div_mc        = 13          !<MC TVD scheme
!
! the schemes of turbulent mode
!
integer, parameter  :: turb_lam      = 0           !<laminal flow
integer, parameter  :: turb_kestd    = 1           !<K-E turbulent model
integer, parameter  :: turb_kerng    = 2           !<K-E RNG turbulent model
integer, parameter  :: turb_kereal   = 3           !<K-E REALIABLE turbulent model

!
! component name
!
character,parameter :: cmpt_vec(3)*1 =['X','Y','Z']
character,parameter :: cmpt_symt(6)*2=['XX','XY','XZ','YY','YZ','ZZ']
character,parameter :: cmpt_tens(9)*2=['XX','XY','XZ','YX','YY','YZ','ZX','ZY','ZZ']
!
! error code
!
integer,parameter   :: err_error	=	-1
integer,parameter   :: err_ok 		=	 0

integer,parameter   :: err_mem_maxvar =10
integer,parameter   :: err_mem_varnoex=11

integer,parameter   :: err_file_nounit = 20
integer,parameter   :: err_file_noexist = 21
integer,parameter   :: err_file_cantread = 22
integer,parameter   :: err_file_cantwrite  = 23
integer,parameter   :: err_file_end  = 24
integer,parameter   :: err_file_realarr= 25
integer,parameter   :: err_file_intarr= 26
integer,parameter   :: err_file_listarr= 27
integer,parameter   :: err_file_keyword= 28
integer,parameter   :: err_file_char   =29

integer,parameter   :: err_str_toint = 40
integer,parameter   :: err_str_toreal = 41
integer,parameter   :: err_str_tonum  = 43

integer,parameter   :: err_msh_vrtfacnum  =50
integer,parameter   :: err_msh_badvect =51

integer,parameter   :: err_fvm_badddt   =100
integer,parameter   :: err_fvm_badddtback=101

contains
!############################################################################
integer function ranks(m) 
!############################################################################
implicit none
integer,intent(in) :: m

select case(m)
  case(1)
    ranks = 0
  case(2,3)
    ranks = 1
  case(6,9)
    ranks = 2
  case default
    ranks = 0
end select
end function ranks


!############################################################################
logical function bfixed(ibt,nm) 
!############################################################################
! Does the field need a reference level for solution
!============================================================================
implicit none
integer,intent(in) :: ibt,nm

bfixed = .false.

select case(ibt)
   case(bc_default)
     if(nm>1) bfixed = .true.
   case(bc_calculated,bc_fixvalue,bc_tottemp,bc_totpres,bc_fixmass,&
        bc_backflow,bc_newton,bc_extrad,bc_mixed)
     bfixed = .true.
end select
end function bfixed

!############################################################################
logical function bneedval(ibt) 
!############################################################################
implicit none
integer,intent(in) :: ibt

bneedval = .false.

select case(ibt)
   case(bc_fixvalue,bc_fixflux,bc_tottemp,bc_totpres,bc_fixmass,&
        bc_backflow,bc_newton,bc_extrad,bc_mixed)
     bneedval = .true.
end select
end function bneedval

!############################################################################
subroutine bvalid(ibt,bok) 
!############################################################################
implicit none
integer,intent(in) :: ibt
logical,intent(out):: bok(nbctype)

bok = .false.

select case(ibt)
  case(bc_calculated,bc_default,bc_empty,bc_wedge,&
       bc_cyclic,bc_interface,bc_symmetry)
    bok(ibt) = .true.
  case(bc_inlet)
    bok(bc_fixvalue) =.true.
    bok(bc_zerograd) =.true.
    bok(bc_fixmass)  =.true.
    bok(bc_tottemp)   =.true.
    bok(bc_totpres)   =.true.
  case(bc_outlet)
    bok(bc_fixvalue) =.true.
    bok(bc_zerograd) =.true.
    bok(bc_backflow) =.true.
  case(bc_wall)
    bok(bc_fixvalue) =.true. 
    bok(bc_zerograd) =.true. 
    bok(bc_fixflux)  =.true. 
    bok(bc_newton)   =.true. 
    bok(bc_extrad)   =.true. 
    bok(bc_mixed)    =.true.
end select
end subroutine bvalid

!############################################################################
subroutine bnpar(ibt,val) 
!############################################################################
! val(1): number of value
! val(2): number of real*8   coefficeints
! val(3): number of additional field specified by field name
! val(4): if need default boundary value, integer flag
!============================================================================
implicit none
integer,intent(in) :: ibt
integer,intent(out):: val(4)

select case(ibt)
  case(bc_calculated,&
       bc_fixvalue)
    val = [1,0,0,0]
  case(bc_default,&
       bc_empty,&
       bc_wedge,&
       bc_symmetry,&
       bc_cyclic,&
       bc_interface,&
       bc_zerograd)
    val = [0,0,0,0]
  case(bc_fixmass,&
       bc_backflow)
    val = [1,0,1,1]
  case(bc_tottemp,&
       bc_totpres,&
       bc_fixflux)
    val = [1,0,1,0]
  case(bc_newton)
    val = [0,2,1,0]
  case(bc_extrad)
    val = [0,2,1,1]
  case(bc_mixed)
    val = [0,4,1,1]
end select
end subroutine bnpar
end module scheme




!data varname(1:mxvar) /&
!'x',    'xc',    'xf',    'vol',   'sf',   'dr',&
!'own',  'nei',&
!------------------------------------------------------------------------------
!     mesh arrays
!------------------------------------------------------------------------------
!         'x    ',          !    01: nodal coordinates
!         'xc   ',          !    02: cell  coordinates
!         'xf   ',          !    03: face center coordinates
!         'vol  ',          !    04: cell volume
!         'sf   ',          !    05: surface vector
!         'dr   ',          !    06: distance of cell p & n
!         'own  ',          !    07: nodal load/displacement, current
!         'nei  ',          !    08: nodal load/displacement, current
                                        
!------------------------------------------------------------------------------
!     solution arrays
!------------------------------------------------------------------------------
!'u',     'p',    'mf',    't',     'h',     'k',&
!'ep',    'e',    'd',     'den',   'vismix','vis',&
!'cpmix', 'cp',   'cond',  'econd'/ 
!         'u',              !     1: fluid velocity
!         'p',              !     2: static pressure
!         't',              !     3: temperature for heat,flow and stress
!         'h',              !     4: enthalpy
!         'k',              !     5: turbulent kinetic energy

!         'ep',             !     6: turbulent dissipation rate
!         'elpot',          !     7: electric potential ( real part)
!         'elpoti',         !     7: electric potential ( imaginary part)
!         'eden',           !     7: electron number density
!         'eperm',          !    15: electric relative permittivity

!         'vis',            !     8: effective viscosity
!         'visl',           !     9: laminal dynamic viscosity
!         'vist',           !    10: turbulent dynamic viscosity
!         'cond',           !    11: laminal dynamic conductivity
!         'den',            !    12: density
!         'cp',             !    13: specific heat
!         'econd',          !    14: electric conductivity
  ! global bc types.
!  integer(int_p), parameter :: bc_type_inlet= 1, &
!                               bc_type_wall = 2, &
!                               bc_type_exit = 3, &
!                               bc_type_interface = 4, &
!                               bc_type_f_f_interface = 5, &
!                               bc_type_s_s_interface = 6, &
!                               bc_type_s_f_interface = 7, &
!                               bc_type_f_b_interface = 8, &
!                               bc_type_b_b_interface = 9, &
!                               bc_type_s_b_interface = 10, &
!                               bc_type_symm = 11, &
!                               bc_type_cyclic = 12, &
!                               bc_type_thinwall = 13

  ! heat transfer bc subtypes
!  integer(int_p), parameter :: bc_heat_inout = 1, &
!                               bc_heat_symm = 2, &
!                               bc_heat_isothermal = 3, &
!                               bc_heat_adiabatic = 4, &
!                               bc_heat_fix_q = 5, &
!                               bc_heat_newton = 6, &
!                               bc_heat_ext_radiation = 7, &
!                               bc_heat_conjugate_interface = 8, &
!                               bc_heat_thinwall = 9, &
!                               bc_heat_cyclic = 10, &
!                               bc_heat_interface = 11, &
!                               bc_heat_mixing_plane = 12, &
!                               bc_heat_couple = 13, &
!                               bc_heat_chimera = 14

  ! flow bc subtypes                               
!  integer(int_p), parameter :: bc_flow_fixm_inlet = 1, &
!                               bc_flow_fixp_outlet = 2, &
!                               bc_flow_wall = 3, &
!                               bc_flow_symm = 4, &
!                               bc_flow_fixp_extrapolat_outlet = 5, &
!                               bc_flow_fixpt_inlet = 6, &
!                               bc_flow_fixp_inlet = 7, &
!                               bc_flow_cyclic = 8, &
!                               bc_flow_interface = 9, &
!                               bc_flow_mixing_plane = 10, &
!                               bc_flow_couple = 11, &
!                               bc_flow_chimera = 12
  ! electric bc subtypes.
!  integer(int_p), parameter :: bc_electric_fix_potential = 1, &
!                               bc_electric_fix_flux = 2, &
!                               bc_electric_symm = 3, &
!                               bc_electric_cyclic = 4, &
!                               bc_electric_diel_diel = 5, &
!                               bc_electric_fix_charge = 6, &
!                               bc_electric_ignore = 7, &
!                               bc_electric_zero_current = 8, &
!                               bc_electric_thin_wall = 9, &
!                               bc_electric_interface = 10


