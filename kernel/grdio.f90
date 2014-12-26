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
!! Contains grd_del(),grd_set(),grd_get(),grd_wrt(),couple_get(),couple_wrt(),vcbnd_get(),
!! vcbnd_wrt(),bnd_reorder(),bnd_resetind()
!<

!############################################################################
!subroutine mesh_exp(lbinary)
!############################################################################
!use scheme,only:delim,dir_constant,caseName
!use fluentbase,only:xf_mesh_Exp
!use fileutils,only:fopen
!implicit none
!logical,intent(in) :: lbinary

!integer          :: ifile
!character(len=3) :: mode

!mode = 'w'
!if(lbinary) mode = 'wb'

!ifile =fopen(trim(dir_constant)//delim//trim(caseName)//".cas",mode)

!call xf_mesh_Exp(ifile)

!close(ifile)
!end subroutine mesh_exp

!>This subroutine delete all the dynamic allocated data
!<
!############################################################################
subroutine grd_del
!############################################################################
use comm0
implicit none

integer :: ierr

if(allocated(nsc))      deallocate(nsc,stat=ierr)
if(allocated(nec))      deallocate(nec,stat=ierr)
if(allocated(cityp))    deallocate(cityp,stat=ierr)
if(allocated(ctype))    deallocate(ctype,stat=ierr)
if(allocated(cname))    deallocate(cname,stat=ierr)

if(allocated(nsf))      deallocate(nsf,stat=ierr)
if(allocated(nef))      deallocate(nef,stat=ierr)
if(allocated(nsb))      deallocate(nsb,stat=ierr)
if(allocated(neb))      deallocate(neb,stat=ierr)

if(allocated(x))        deallocate(x,stat=ierr)
if(allocated(xo))       deallocate(xo,stat=ierr)

if(allocated(xc))       deallocate(xc,stat=ierr)
if(allocated(vol))      deallocate(vol,stat=ierr)
if(allocated(volo))     deallocate(volo,stat=ierr)
if(allocated(voloo))    deallocate(voloo,stat=ierr)

if(allocated(own))      deallocate(own,stat=ierr)
if(allocated(nei))      deallocate(nei,stat=ierr)

if(allocated(swvol))    deallocate(swvol,stat=ierr)
if(allocated(sf))       deallocate(sf,stat=ierr)
if(allocated(magsf))    deallocate(magsf,stat=ierr)
if(allocated(xf))       deallocate(xf,stat=ierr)
if(allocated(xfo))      deallocate(xfo,stat=ierr)
if(allocated(fx))       deallocate(fx,stat=ierr)
if(allocated(dr))       deallocate(dr,stat=ierr)
if(allocated(corr))     deallocate(corr,stat=ierr)
if(allocated(lsp))      deallocate(lsp,stat=ierr)
if(allocated(lsn))      deallocate(lsn,stat=ierr)

if(allocated(nspair))   deallocate(nspair,stat=ierr)
if(allocated(nepair))   deallocate(nepair,stat=ierr)
if(allocated(bpair))    deallocate(bpair,stat=ierr)

if(allocated(nsp))      deallocate(nsp,stat=ierr)
if(allocated(nep))      deallocate(nep,stat=ierr)
if(allocated(nspem))    deallocate(nspem,stat=ierr)
if(allocated(nepem))    deallocate(nepem,stat=ierr)
                                     
if(allocated(bcoup))    deallocate(bcoup,stat=ierr)
if(allocated(bstrain))  deallocate(bstrain,stat=ierr)
if(allocated(btrans))   deallocate(btrans,stat=ierr)
if(allocated(bityp))    deallocate(bityp,stat=ierr)
if(allocated(btype))    deallocate(btype,stat=ierr)
if(allocated(bname))    deallocate(bname,stat=ierr)
if(allocated(brough))   deallocate(brough,stat=ierr)
if(allocated(bgmot))    deallocate(bgmot,stat=ierr)
if(allocated(bvmot))    deallocate(bvmot,stat=ierr)

if(allocated(forwardt)) deallocate(forwardt,stat=ierr)
if(allocated(reverset)) deallocate(reverset,stat=ierr)

if(allocated(sepaxis))  deallocate(sepaxis,stat=ierr)
if(allocated(rotcen))   deallocate(rotcen,stat=ierr)
if(allocated(cellt))    deallocate(cellt,stat=ierr)
if(allocated(facet))    deallocate(facet,stat=ierr)

if(allocated(cpwt))     deallocate(cpwt)

if(allocated(ccell))    deallocate(ccell,stat=ierr)
if(allocated(ccell_p))  deallocate(ccell_p,stat=ierr)
if(allocated(cpoint))   deallocate(cpoint,stat=ierr)
if(allocated(cpoint_p)) deallocate(cpoint_p,stat=ierr)
if(allocated(pcell))    deallocate(pcell,stat=ierr)
if(allocated(pcell_p))  deallocate(pcell_p,stat=ierr)
if(allocated(pface))    deallocate(pface,stat=ierr)
if(allocated(pface_p))  deallocate(pface_p,stat=ierr)
if(allocated(face))     deallocate(face,stat=ierr)
if(allocated(face_p))   deallocate(face_p,stat=ierr)
if(allocated(cface))    deallocate(cface,stat=ierr)
if(allocated(cface_p))  deallocate(cface_p,stat=ierr)

end subroutine grd_del

!############################################################################
subroutine grd_set(ndim_,nvrt_,x_,&
                    ntface_,ntvfac_,nvface_,face_,own_,nei_,&
                    ndo_,   cname_,ctype_,cstart_,cncel_,&
                    npatch_,bname_,btype_,bstart_,bnfac_)
!############################################################################
! directly set mesh data
!============================================================================
use comm0
use scheme
implicit none
integer,intent(in)          :: ndim_,nvrt_
real(dp),intent(in)          :: x_(ndim_,nvrt_)
integer,intent(in)          :: ntface_,ntvfac_
integer,intent(in)          :: nvface_(ntface_)
integer,intent(in)          :: face_(ntvfac_)
integer,intent(in)          :: own_(ntface_),nei_(ntface_)
integer,intent(in)          :: ndo_
character(len=*),intent(in) :: cname_(ndo_)
character(len=*),intent(in) :: ctype_(ndo_)
integer,intent(in)          :: cstart_(ndo_)
integer,intent(in)          :: cncel_(ndo_)
integer,intent(in)          :: npatch_
character(len=*),intent(in) :: bname_(npatch_)
character(len=*),intent(in) :: btype_(npatch_)
integer,intent(in)          :: bstart_(npatch_)
integer,intent(in)          :: bnfac_(npatch_)

integer,external :: stridx

integer  :: nm,i,j,ifs,ife,ierr
character:: fun='GRD_SET'
!
! set vertex
!
nm   = ndim_
nvrt = nvrt_

if(nvrt<=0) then
  ierr = err_grd_vrtfacnum ; goto 100
endif

if(allocated(x)) deallocate(x,stat=ierr)
allocate(x(3,nvrt),stat=ierr)
call memlog('X',loc(x))

do i=1,nvrt
  x(1:nm,i) =x_(1:nm,i)
end do
!
! set faces,owner and nei
!
ntface =ntface_
nface  =ntface
ntvfac =ntvfac_
if(ntface<=0 .or. ntvfac<=0) then
  ierr = err_grd_vrtfacnum;  goto 100
endif

if(allocated(face_p)) deallocate(face_p,stat=ierr)
if(allocated(face))   deallocate(face,stat=ierr)
if(allocated(own))    deallocate(own,stat=ierr)
if(allocated(nei))    deallocate(nei,stat=ierr)
!
allocate(own(ntface),nei(ntface),face_p(ntface+1),face(ntvfac),stat=ierr)
call memlog('OWN',loc(own))
call memlog('NEI',loc(nei))
call memlog('FACE',loc(face))
call memlog('FACE_P',loc(face_p))
!
face_p(1) = 1
do i=1,ntface
  face_p(i+1) = face_p(i)+nvface_(i)
end do
own = own_
nei = nei_
face= face_
!
! set cell conditions
!
ndo  = ndo_
ncell= 0
do i=1,ndo
  cname(i) = cname_(i)
  ctype(i) = ctype_(i)
  nec(i)   = cncel_(i)  
   
  ncell    = ncell  +nec(i)
  nec(i)   = nsc(i) +nec(i)-1
end do
!
! set boundaries
!
npatch=npatch_

nbfac = 0
do i=1,npatch
  bname(i) = bname_(i)
  btype(i) = btype_(i)
  nep(i)   = bnfac_(i)  
  nsp(i)   = bstart_(i)
   
  nbfac  = nbfac  +nep(i)
  nep(i) = nsp(i) +nep(i)-1

  call patch(i,btype(i))
  
  if(bityp(i)==bc_cyclic) then
    call uinfo(trim(fun),'I','CYCLIC BOUNDARY NEED GEOMETRICAL DATA')
  endif
enddo 

call bnd_reorder(ntface)
call bnd_resetind

return

100 continue
call info(trim(fun),'E',ierr)
return
end subroutine grd_set

!
!>This subroutine gets mesh and boundary information from file
!>@param[in]   mfile  -- mesh file unit
!>@param[in]   bfile  -- boundary file unit
!<
!############################################################################
subroutine grd_get(mfile,bfile)
!############################################################################
! read mesh from file
! mfile --- mesh file unit
! bfile --- boundary file unit
!============================================================================
use precision,only:delim
use scheme
use comm0
implicit none
integer,intent(in) :: mfile
integer,intent(in) :: bfile

integer   :: i,ir,nm,ierr
character :: mode*3,str*64,file*256
character :: fun*16='GRD_GET'
!
! read point
!
call disp('READING POINTS.....')

call getline(mfile,str,ierr)
if(ierr/=0) goto 100

read(str,*,err=200) nm,nvrt
if(nm<=0 .or. nvrt<=0) then
  ierr = err_grd_vrtfacnum ; goto 100
endif  
!
if(allocated(x)) deallocate(x,stat=ierr)
allocate(x(3,nvrt),stat=ierr)
call memlog('X',loc(x))
!
call findc(mfile,'(',.true.,ierr)
if(ierr/=0) goto 100
call getreal(mfile,nm,nvrt,x,1,nvrt,.false.,ierr)
if(ierr/=0) goto 100
call findc(mfile,')',.true.,ierr)
if(ierr/=0) goto 100
!
! read the faces
!
call disp('READING FACES TOPOLOGY.....')

call getline(mfile,str,ierr)
if(ierr/=0) goto 100

read(str,*,err=200) ntvfac,ntface
if(ntface<=0.or.ntvfac<=0) then
  ierr = err_grd_vrtfacnum;  goto 100
endif
nface = ntface
!
! create the strage for face
!
if(allocated(face_p)) deallocate(face_p,stat=ierr)
if(allocated(face))   deallocate(face,stat=ierr)
if(allocated(own))    deallocate(own,stat=ierr)
if(allocated(nei))    deallocate(nei,stat=ierr)
!
allocate(own(ntface),nei(ntface),face_p(ntface+1),face(ntvfac),stat=ierr)
call memlog('OWN',loc(own))
call memlog('NEI',loc(nei))
call memlog('FACE',loc(face))
call memlog('FACE_P',loc(face_p))

call findc(mfile,'(',.true.,ierr)
if(ierr/=0) goto 100
call getfacei(mfile,ntface,ntvfac,face_p,face,own,nei,1,ntface,0,ierr)
if(ierr/=0) goto 100
call findc(mfile,')',.true.,ierr)
if(ierr/=0) goto 100
!
! read the cyclic and interface pairs
!
call couple_get(mfile)
!
close(mfile)
!
! read cell and boundary information and reoder the boundary
!
call vcbnd_get(bfile)
call bnd_reorder(ntface)
call bnd_resetind
!
! begin logging due to mesh load
!  
call log_ini
return
!
100 continue
close(mfile)
close(bfile)
call info(trim(fun),'E',ierr)
return
!
200 continue
ierr = err_str_toint
close(mfile)
close(bfile)
call info(trim(fun),'E',ierr)
end subroutine grd_get


!>This subroutine writes the grid to mesh and boundary file
!>@param[in]   mfile  -- mesh file unit
!>@param[in]   bfile  -- boundary file unit
!<
!############################################################################
subroutine grd_wrt(mfile,bfile)
!############################################################################
! write mesh data to file
! mfile --- mesh file unit
! bfile --- boundary file unit
!============================================================================
use precision,only:delim
use comm0
implicit none
integer,intent(in) :: mfile
integer,intent(in) :: bfile

integer   :: ierr
character :: str*64,fun*16='GRD_WRT'

call disp('WRITING POINTS.....')
call wrtline(mfile,'# POINT COORDINATES')
write(str,'(i0,1x,i0)') 3,nvrt
call wrtline(mfile,str)
call wrtline(mfile,'(')
call wrtreal(mfile,3,nvrt,x,1,nvrt)
call wrtline(mfile,')')

call disp('WRITING FACE-BASED TOPOLOGY.....')
call wrtline(mfile,'# FACE-BASED TOPOLOGY')
write(str,'(i0,1x,i0)') ntvfac,ntface
call wrtline(mfile,str)
call wrtline(mfile,'(')
call wrtfacei(mfile,ntface,ntvfac,face_p,face,own,nei,1,ntface)
call wrtline(mfile,')')

call couple_wrt(mfile)
close(mfile)

call vcbnd_wrt(bfile)
return

100 continue
close(bfile)
call info(trim(fun),'E',ierr)
return
end subroutine grd_wrt

!>This subroutine gets coupled data from mesh file
!>@param[in]   mfile  -- mesh file unit
!<
!############################################################################
subroutine couple_get(mfile)
!############################################################################
use comm0
use scheme,only:err_str_toint
implicit none
integer,intent(in) :: mfile

integer   :: pair(2,ntface),ierr,if1,if2,ir,nm,n,i
character :: str*64,fun*16='COUPLE_GET'

call disp('READING CYCLIC AND INTERFACE PAIRS.....')

call getline(mfile,str,ierr)
if(ierr/=0) goto 100
read(str,*,err=200) ncycle,ninter

if(ncycle>0 .or. ninter>0) then
  if(allocated(nspair)) deallocate(nspair,stat=ierr)
  if(allocated(nepair)) deallocate(nepair,stat=ierr)
  allocate(nspair(ncycle+ninter),nepair(ncycle+ninter),stat=ierr)
  nspair = 0;   nepair = 0

  nspair(1) = 1
  do ir=1,ncycle+ninter
    if(ir>1) nspair(ir)=nepair(ir-1)+1

    call getline(mfile,str,ierr)
    if(ierr/=0) goto 100
    read(str,*,err=200) nm,n
  
    nepair(ir) = nspair(ir) + n-1
    
    call findc(mfile,'(',.true.,ierr)
    if(ierr/=0) goto 100
    call getint(mfile,nm,ntface,pair,nspair(ir),nepair(ir),ierr)
    if(ierr/=0) goto 100
    call findc(mfile,')',.true.,ierr)
    if(ierr/=0) goto 100
  
    do i=nspair(ir),nepair(ir)
      if1 = pair(1,i); if2 =pair(2,i)
      nei(if1) = own(if2); nei(if2) = own(if1)
    end do
  end do
!
! resize the bpair
!
  if(nepair(ncycle+ninter)<ntface) then
    allocate(bpair(2,nepair(ncycle+ninter)),stat=ierr)
    bpair = pair(:,1:nepair(ncycle+ninter))
  endif
endif
return

100 continue
close(mfile)
call info(trim(fun),'E',ierr)
return
!
200 continue
ierr = err_str_toint
close(mfile)
call info(trim(fun),'E',ierr)

end subroutine couple_get

!>This subroutine writes coupled data into mesh file
!>@param[in]   mfile  -- mesh file unit
!<
!############################################################################
subroutine couple_wrt(mfile)
!############################################################################
use comm0
implicit none
integer,intent(in) :: mfile

integer   :: ir,nm,n
character :: str*64,fun*16='COUPLE_WRT'

call disp('WRITING INTERFACE AND CYCLIC PAIRS.....')

call wrtline(mfile,'# CYCLIC AND INTERFACE PAIRS')
write(str,'(i0,1x,i0)') ncycle,ninter
call wrtline(mfile,str)

if(ncycle>0 .or. ninter>0) then
  n = nepair(ncycle+ninter) - nspair(1)
  do ir=1,ncycle+ninter
    write(str,'(i0,1x,i0)') 2,nepair(ir)-nspair(ir)+1
    call wrtline(mfile,str)

    call wrtline(mfile,'(')
    call wrtint(mfile,2,n,bpair,nspair(ir),nepair(ir))
    call wrtline(mfile,')')
  end do
endif
return
end subroutine couple_wrt


!>This subroutine reads cell and boundary conditions of openNabla
!>@param[in]   ifile  -- file unit
!<
!############################################################################
subroutine vcbnd_get(ifile)
!############################################################################
! read cell and boundary conditions from file
! ifile --- boundary file unit
!============================================================================
use scheme
use comm0
implicit none
integer,intent(in) :: ifile

logical,external :: strcmp
integer,external :: stridx

integer   :: i,j,ierr
character :: line*1024,key*256
character :: fun*16='VCBND_GET'
!
! read cells
!
call disp('READING CELLS')

call getline(ifile,line,ierr)
if(ierr/=0) goto 100

call sreadint(line,ndo,ierr)
if(ierr/=0) goto 100

allocate(nsc(ndo),nec(ndo),&
         nsf(ndo),nef(ndo),&
         nsb(ndo),neb(ndo),&
         cityp(ndo),ctype(ndo),cname(ndo),&
         stat=ierr)
if(ierr/=0) goto 100

call findc(ifile,'(',.true.,ierr)
if(ierr/=0) goto 100

ncell = 0
do i=1,ndo
  call getline(ifile,line,ierr)
  if(ierr/=0) goto 100

  call sreadstr(line,cname(i))

  call findc(ifile,'{',.true.,ierr)
  if(ierr/=0) goto 100
!
  call getcpar(ifile,'TYPE',1,ctype(i:),ierr)
  if(ierr/=0) goto 100
  
  cityp(i) = stridx(ctype(i),nvctype,ctypnam)

  call getipar(ifile,'NCELLS',1,nec(i:),ierr)
  if(ierr/=0) goto 100

  call getipar(ifile,'STARTCELL',1,nsc(i:),ierr)
  if(ierr/=0) goto 100
   
  call findc(ifile,'}',.true.,ierr)
  if(ierr/=0) goto 100
  
  ncell  = ncell  + nec(i)
  nec(i) = nsc(i) + nec(i)-1
end do
call findc(ifile,')',.true.,ierr)

! read boundary

call disp('READING BOUNDARIES')

call getline(ifile,line,ierr)
if(ierr/=0) goto 100

call sreadint(line,npatch,ierr)
if(ierr/=0) goto 100

allocate(nsp(npatch),nep(npatch),&
          nspem(npatch),nepem(npatch),&
          bcoup(npatch),bstrain(npatch),&
          brough(npatch),bgmot(npatch),bvmot(npatch),&
          bityp(npatch),btype(npatch),bname(npatch),&
          btrans(npatch),forwardt(9,npatch),reverset(9,npatch),&
          sepaxis(3,npatch),rotcen(3,npatch),&
          cellt(9,npatch),facet(9,npatch),&         
          stat=ierr)
if(ierr/=0) goto 100

call findc(ifile,'(',.true.,ierr)
if(ierr/=0) goto 100

nbfac = 0
do i=1,npatch
  call getline(ifile,line,ierr)
  if(ierr/=0) goto 100

  call sreadstr(line,bname(i))

  call findc(ifile,'{',.true.,ierr)
  if(ierr/=0) goto 100
!
  call getcpar(ifile,'TYPE',1,btype(i:),ierr)
  if(ierr/=0) goto 100

  call getipar(ifile,'NFACES',1,nep(i:),ierr)
  if(ierr/=0) goto 100

  call getipar(ifile,'STARTFACE',1,nsp(i:),ierr)
  if(ierr/=0) goto 100
   
  nbfac  = nbfac+nep(i)
  nep(i) = nsp(i) +nep(i)-1

  call patch(i,btype(i))
  
  select case(bityp(i))
    case(bc_interface)
      bcoup(i)   = .true.
	  btrans(i) = .false.
    case(bc_cyclic) 
      bcoup(i) = .true.

      call findkey(ifile,"TRANSFORM",line,ierr)
      if(ierr/=0) goto 100
    
      call sreadstr(line,key)
    
	  if(strcmp(key,"TRANSLATIONAL")) then
	    btrans(i) = .false.

        call getrpar(ifile,'SEPARATIONVECTOR',3,sepaxis(:,i),ierr)
        if(ierr/=0) goto 100
      elseif(strcmp(key,"ROTATIONAL")) then
	    btrans(i) = .true.

        call getrpar(ifile,'ROTATIONAXIS',3,sepaxis(:,i),ierr)
        if(ierr/=0) goto 100
    
        call getrpar(ifile,'ROTATIONCENTRE',3,rotcen(:,i),ierr)
        if(ierr/=0) goto 100
	  else
	    call uinfo(trim(fun),'W',"CYCLIC BOUNDARY NOT WELL DEFINED")
	  endif
  end select

  call findc(ifile,'}',.true.,ierr)
  if(ierr/=0) goto 100
enddo 

call findc(ifile,')',.true.,ierr)
close(ifile)
return

100 continue
close(ifile)
call info(trim(fun),'E',ierr)
return

200 continue
ierr = 51
close(ifile)
call info(trim(fun),'E',ierr)
return
end subroutine vcbnd_get

!>This subroutine write cell and boundary information into file
!<
!############################################################################
subroutine vcbnd_wrt(ifile)
!############################################################################
! write boundary to file
!============================================================================
use scheme,only:bc_cyclic
use comm0
implicit none
integer,intent(in) :: ifile

integer   :: ir,i
character :: str*256

call disp('WRITING CELLS')

call wrtline(ifile,'# CELLS')

write(str,'(i0)') ndo
call wrtline(ifile,str)
call wrtline(ifile,'(')

do i=1,ndo
  call wrtline(ifile,adjustl(cname(i)))
  call wrtline(ifile,'{')
 
  call wrtline(ifile,'    TYPE '//trim(ctype(i)))
  
  write(str,'(t5,a,i0)')   'NCELLS ',nec(i)-nsc(i)+1
  call wrtline(ifile,str)

  write(str,'(t5,a,i0)')   'STARTCELL ',nsc(i)
  call wrtline(ifile,str)

  call wrtline(ifile,'}')
end do

call wrtline(ifile,')')

call disp('WRITING BOUNDARIES')

call wrtline(ifile,'# BOUNDARIES')

write(str,'(i0)') npatch+nempatch
call wrtline(ifile,str)
call wrtline(ifile,'(')

do ir=1,npatch+nempatch
  call wrtline(ifile,adjustl(bname(ir)))
  call wrtline(ifile,'{')
  
  call wrtline(ifile,'    TYPE '//trim(btype(ir)))
  
  write(str,'(t5,a,i0)')   'NFACES ',nep(ir)-nsp(ir)+1
  call wrtline(ifile,str)

  write(str,'(t5,a,i0)')   'STARTFACE ',nsp(ir)+nface
  call wrtline(ifile,str)
  
  if(bityp(ir)==bc_cyclic) then
    if(btrans(ir)) then
      write(str,'(t5,a)')      'TRANSFORM ROTATIONAL'
      call wrtline(ifile,str)
      
      write(str,'(t5,a,3(es19.11e3,1x))') 'ROTATIONAXIS ',sepaxis(:,ir)
      call wrtline(ifile,str)
      
      write(str,'(t5,a,3(es19.11e3,1x))') 'ROTATIONCENTRE ',rotcen(:,ir)
      call wrtline(ifile,str)
    else
      write(str,'(t5,a)')    'TRANSFORM TRANSLATIONAL'
      call wrtline(ifile,str)

      write(str,'(t5,a,3(es19.11e3,1x))') 'SEPARATIONVECTOR ',sepaxis(:,ir)
      call wrtline(ifile,str)
    endif
  endif  
  
  call wrtline(ifile,'}')
end do

call wrtline(ifile,')')
end subroutine vcbnd_wrt


!>This subroutine reorder the boundary data so that the empty boundary will be 
!!the last, then coupled bounday  will be at the front of the empty boundary
!<
!############################################################################
subroutine bnd_reorder(nfacea)
!############################################################################
! reordering the patch, first(common boundary) then(coupled boundary) finally(empty)
!============================================================================
use comm0,only:ntface,nbfac,npatch,bityp,bcoup,nsp,nep,bname,btype,btrans,own,nei,bstrain,face,face_p
use scheme,only:bc_empty
implicit none
integer,intent(in) :: nfacea

integer   :: nspOld(npatch),nepOld(npatch),bitypOld(npatch),&
              ownOld(nfacea-nbfac+1:nfacea),neiOld(nfacea-nbfac+1:nfacea),regID(npatch)
character :: btypeOld(npatch)*32,bnameOld(npatch)*32
logical   :: btransOld(npatch),bcoupOld(npatch),bstrainOld(npatch)
integer   :: n,ir,iir,ncou,nemp,ibeg,iend,nbc,ifbeg,ifend,iff,ifff

regID=0          ! init the regionID

nEmp = 0         ! first find empty boundaries, the empty boundary will be packed in the tail
do ir=npatch,1,-1
  if(bityp(ir)==bc_empty) then
    nEmp                = nEmp +1
    regID(npatch-nEmp+1)= ir
  endif
end do 

nCou= 0         ! then find coupled boundaries, the coupled boundary will be ahead of the empty
do ir=npatch,1,-1
  if(bcoup(ir)) then
    nCou         = nCou +1
    regID(npatch-nCou-nEmp+1)= ir
  endif
end do 

nbc = 0         ! last the common boundaries
do ir=1,npatch
  if(bityp(ir)/=bc_empty.and.(.not.bcoup(ir))) then
    nbc = nbc+1
    regID(nbc) = ir
  endif
end do  

iEnd = -1       ! the last region index whose data will be changed
do ir=npatch,1,-1
  if(regID(ir)/=ir) then
    iEnd = ir
    exit
  endif
enddo

iBeg = npatch+1 ! the first region index whose data will be changed
do ir=1,npatch
  if(regID(ir)/=ir) then
    iBeg = ir
    exit
  endif
enddo

if(iEnd<=iBeg ) return ! no reordering needed
!
! save old data
!
nspOld(iBeg:iEnd)     = nsp(iBeg:iEnd)
nepOld(iBeg:iEnd)     = nep(iBeg:iEnd)
bitypOld(iBeg:iEnd)   = bityp(iBeg:iEnd)
bnameOld(iBeg:iEnd)   = bname(iBeg:iEnd)
btypeOld(iBeg:iEnd)   = btype(iBeg:iEnd)
bcoupOld(iBeg:iEnd)   = bcoup(iBeg:iEnd)
bstrainOld(iBeg:iEnd) = bstrain(iBeg:iEnd)
btransOld(iBeg:iEnd)  = btrans(iBeg:iEnd)

do ir=iBeg,iEnd
  if(ir>1) nsp(ir) = nep(ir-1)+1
  
  iir = regID(ir)
  
  n       = nepOld(iir) - nspOld(iir)+1 
  nep(ir) = nsp(ir) + n -1 
  
  bname(ir)   = bnameOld(iir)
  bityp(ir)   = bitypOld(iir)
  btype(ir)   = btypeOld(iir)
  bcoup(ir)   = bcoupOld(iir)
  bstrain(ir) = bstrainOld(iir) 
  btrans(ir)  = btransOld(iir) 
end do
!
! reordering the own and nei
!
ifbeg = nspOld(iBeg); ifend = nepOld(iEnd)
ownOld(ifbeg:ifend) = own(ifbeg:ifend)
neiOld(ifbeg:ifend) = nei(ifbeg:ifend)

ifff = ifbeg
do ir = ibeg,iend
  iir = regId(ir)
  do iff= nspOld(iir),nepOld(iir)
    own(ifff) = ownOld(iff)
    nei(ifff) = neiOld(iff)
    ifff = ifff +1
  end do
end do

call bface_reorder(ifbeg,ifend)

contains 
!
!===============================================
subroutine bface_reorder(ifb,ife)
!===============================================
! reordering the face definition
!===============================================
implicit none
integer,intent(in) :: ifb,ife

integer :: faceo(face_p(ifb):face_p(ife+1)-1),faceo_p(ifb:ife+1)

faceo_p(ifb:ife+1)                  = face_p(ifb:ife+1)
faceo(faceo_p(ifb):faceo_p(ife+1)-1)= face(face_p(ifb):face_p(ife+1)-1)

ifff = ifb
do ir = ibeg,iend
  iir = regId(ir)
  do iff= nspOld(iir),nepOld(iir)
    face_p(ifff+1) = face_p(ifff)+(faceo_p(iff+1) - faceo_p(iff))
    face(face_p(ifff):face_p(ifff+1)-1) = faceo(faceo_p(iff):faceo_p(iff+1)-1) 
    
    ifff = ifff +1
  end do
end do
end subroutine bface_reorder

end subroutine bnd_reorder

!>This subroutine reset index of own anf nei so lb and ln point to boundary faces
!<
!################################################################################
subroutine bnd_resetind
!################################################################################
use iso_c_binding,only:c_null_ptr,c_f_pointer
use comm0
implicit none

nface = nsp(1)-1
ntbfac= nbfac+nemfac

nsp(1:npatch)  = nsp(1:npatch) - nface
nep(1:npatch)  = nep(1:npatch) - nface

call c_f_pointer(transfer(loc(own(nface+1)),c_null_ptr),lb,[nbfac])
call c_f_pointer(transfer(loc(nei(nface+1)),c_null_ptr),ln,[nbfac])
end subroutine bnd_resetind 








