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
!! Contains subroutine of_msh_get(),which read openFOAM ascii mesh file including
!! points,faces,owner,neighbour and boundary
!<

!>This subroutine read openFOAM mesh
!>@param[in]   pfile -- points file
!>@param[in]   ffile -- faces file
!>@param[in]   ofile -- owner file
!>@param[in]   nfile -- neighbour file
!>@param[in]   bfile -- boundary file
!<
!############################################################################
subroutine of_msh_get(pfile,ffile,ofile,nfile,bfile)
!############################################################################
! read OpenFOAM mesh 
!============================================================================
use precision,only:delim
use scheme
use meshbase
use listlib,only:list_assign
implicit none
integer,intent(in):: pfile,ffile,ofile,nfile,bfile

integer   :: i,n,ierr,nf,id(100)
character :: str*256,fun*16='OF_MSH_GET'
!
ierr = err_str_toint  ! default value if error occur
!
! read point file
!
call disp('READING OPENFOAM POINTS.....')

call readHeader(pfile)
call getline(pfile,str,ierr)
if(ierr/=0) goto 100

read(str,*,err=100) nvrt_
if(nvrt_<=0) then
  ierr = err_msh_vrtfacnum ; goto 100
endif  

ndim_        =3
point_%ndim  =3
point_%nsize =nvrt_
allocate(point_%rd(3,nvrt_))
!
call findc(pfile,'(',.true.,ierr)
if(ierr/=0) goto 100

do i=1,nvrt_
  call getline(pfile,str,ierr)
  if(ierr/=0) goto 100

  call strblk(str)
  read(str,*,err=100) point_%rd(:,i)
end do

call findc(pfile,')',.true.,ierr)
if(ierr/=0) goto 100

close(pfile,status='delete')
!
! read the faces
!
call disp('READING OPENFOAM FACES.....')

call readHeader(ffile)
call getline(ffile,str,ierr)
if(ierr/=0) goto 100

read(str,*,err=100) nface_
if(nface_<=0) then
  ierr = err_msh_vrtfacnum;  goto 100
endif

nbfac_      =0
face_%ndim  =1
face_%nsize =nface_
allocate(face_%ld(nface_))

call findc(ffile,'(',.true.,ierr)
if(ierr/=0) goto 100

do i=1,nface_
  call getline(ffile,str,ierr)
  if(ierr/=0) goto 100

  call strblk(str)
  read(str,*) nf,id(1:nf)
  
  id(1:nf)=id(1:nf)+1
  
  call list_assign(face_%ld(i),id(1:nf))
end do

call findc(ffile,')',.true.,ierr)
if(ierr/=0) goto 100

close(ffile,status='delete')
!
! read owner
!
call disp('RAEDING OPENFOAM OWNERS.....')

call readHeader(ofile)
call getline(ofile,str,ierr)
read(str,*,err=100) n
if(n/=nface_) then
  ierr=err_msh_vrtfacnum; goto 100
endif  

own_%ndim  =1
own_%nsize =nface_
allocate(own_%id(1,nface_))

call findc(ofile,'(',.true.,ierr)
if(ierr/=0) goto 100

do i=1,nface_
  call getline(ofile,str,ierr)
  if(ierr/=0) goto 100

  read(str,*,err=100) own_%id(1,i)
  own_%id(1,i) = own_%id(1,i) +1
end do

call findc(ofile,')',.true.,ierr)
if(ierr/=0) goto 100

close(ofile,status='delete')
!
! read neighbour
call disp('READING OPENFOAM NEIGHBOURS.....')

call readHeader(nfile)
call getline(nfile,str,ierr)
if(ierr/=0) goto 100

read(str,*,err=100) n
if(n<=0.or.n>nface_)  then
  ierr=err_msh_vrtfacnum ; goto 100
endif  

nei_%ndim  =1
nei_%nsize =nface_
allocate(nei_%id(1,nface_))

call findc(nfile,'(',.true.,ierr)
if(ierr/=0) goto 100

do i=1,n
  call getline(nfile,str,ierr)
  if(ierr/=0) goto 100

  read(str,*,err=100) nei_%id(1,i)
  nei_%id(1,i) = nei_%id(1,i) +1
end do

call findc(nfile,')',.true.,ierr)
if(ierr/=0) goto 100
!
if(n<nface_) nei_%id(1,n+1:nface_) = 0
close(nfile,status='delete')

!
! read boundary
!
call disp('READING OPENFOAM BOUNDARIES.....')

call readHeader(bfile)
call getline(bfile,str,ierr)
if(ierr/=0) goto 100

read(str,*,err=100) nreg_

patch_%name = 'PATCH'
patch_%ndim = 1
patch_%nsize= nreg_
allocate(patch_%id(2,nreg_),stat=ierr)
allocate(patch_%sd(2,nreg_),stat=ierr)

call findc(bfile,'(',.true.,ierr)
if(ierr/=0) goto 100

do i=1,nreg_
  call getline(bfile,str,ierr)
  if(ierr/=0) goto 100

  call sreadstr(str,patch_%sd(1,i))

  call findc(bfile,'{',.true.,ierr)
  if(ierr/=0) goto 100

  call getcpar(bfile,'TYPE',1,patch_%sd(2,i),ierr)
  if(ierr/=0) goto 100
  
  call getipar(bfile,'NFACES',1,patch_%id(2,i),ierr)
  if(ierr/=0) goto 100

  call getipar(bfile,'STARTFACE',1,patch_%id(1,i),ierr)
  if(ierr/=0) goto 100
  
  call findc(bfile,'}',.true.,ierr)
  if(ierr/=0) goto 100
  
  patch_%id(1,i) = patch_%id(1,i)+1
  patch_%id(2,i) = patch_%id(1,i)+patch_%id(2,i)-1
end do

call findc(bfile,')',.true.,ierr)
if(ierr/=0) goto 100
!
close(bfile,status='delete')

ncel_ = maxval(own_%id(1,:))

nczon_ = 1
allocate(cell_%sd(2,nczon_),cell_%id(2,nczon_))
cell_%sd(1,1)='internal'
cell_%sd(2,1)='FLUID'
cell_%id(1,1)= 1 
cell_%id(2,1)= ncel_

return

100 continue
close(pfile)
close(ffile)
close(ofile)
close(nfile)
close(bfile)
call info(trim(fun),'E',ierr)

contains
!############################################################################
subroutine readHeader(ifile)
!############################################################################
! read File header
!============================================================================
implicit none
integer,intent(in)  :: ifile

call findc(ifile,'{',.true.,ierr)
call findc(ifile,'}',.true.,ierr)
return
end subroutine readHeader

end subroutine of_msh_get