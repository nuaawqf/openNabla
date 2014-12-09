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
!! Contains subroutines fopen(),fdel(),findkey(),findc(),getline(),wrtline(),
!! getreal(),wrtreal(),getint(),wrtint(),getlist(),wrtlist(),getface(),wrtface()\n
!<

!> This routine opens a file unit according to filename and mode
!<
!###########################################################################
subroutine fopen(ifile,file,mode,ierr)
!###########################################################################
! filename is given to *file*, and open mode is given to *mode*, 
! then file is opened and unit number ifile is returned.
!===========================================================================
!>@param[out] ifile -- return file unit
!>@param[in]  file  -- file name       
!>@param[in]  mode  -- read and write mode       
!>@param[out] ierr  -- return error code
!<
!===========================================================================
! "r"  :: a file is opened with read-only mode, ascii format
! "w"  :: a file is opened with writable mode,  ascii format. 
!         if a file is exist already, the contest of the file is emptied.
! "a"  :: a file is opened with writable mode,  ascii format.
!         output is appended at the end of the file.
! "rw" :: a file is opened with read/write mode,ascii format. 
!         if a file is exist already, the contest of the file is emptied. 
! "ra" :: a file is opened with read/write mode,ascii format. 
!         if a file is exist already, 
!         a position of read/write is set at the end of the file.
! "rb" :: a file is opened with read-only mode, binary format
! "wb" :: a file is opened with writable mode,  binary format. 
!        if a file is exist already, the contest of the file is emptied.
! "ab" :: a file is opened with writable mode,  binary format.
!         output is appended at the end of the file.
! "rwb":: a file is opened with read/write mode,binary format. 
!         if a file is exist already, the contest of the file is emptied. 
! "rab":: a file is opened with read/write mode,binary format. 
!         if a file is exist already, 
!         a position of read/write is set at the end of the file.
!======================================================================
use scheme
implicit none
integer,intent(out)          :: ifile
character(len=*), intent(in) :: file
character(len=*), intent(in) :: mode
integer,intent(out)          :: ierr

logical,external :: strcmp

logical   :: opend,exsit
integer   :: prevlun,ios,i,ich
character :: open_mode*3

ierr = 0
!
!-----------------------------------------------------------------
!  check optional arguments
!-----------------------------------------------------------------
open_mode = mode
!
!-----------------------------------------------------------------
!  convert to lower ascii
!-----------------------------------------------------------------
do i=1,3
  ich = ichar(open_mode(i:i))
  if(ich>=65 .and. ich<=90) then
    open_mode(i:i) = achar(ich+32)
  endif
enddo  
!
!-----------------------------------------------------------------
!  search available unit number
!-----------------------------------------------------------------
ifile = -1
do prevlun = 1,999
   inquire(unit=prevlun, opened=opend,iostat=ierr )
   if(ierr>0) cycle
   if(.not. opend ) then
     ifile = prevlun
     exit
   endif
enddo
if(ifile<0) goto 100
!
!----------------------------------------------------------------
!  check existance of a file
!----------------------------------------------------------------
!
if(strcmp(open_mode,'r').or.strcmp(open_mode,'rb')) then
  inquire(file=file, exist=exsit)
  if(.not. exsit) goto 200
endif

!----------------------------------------------------------------
!  check readable of a file
!----------------------------------------------------------------
ios = 0
if(strcmp(open_mode,'r')) then
  open(unit=ifile,iostat=ios,file=file,status='old',action='read',&
         form='formatted',access='SEQUENTIAL')
elseif(strcmp(open_mode,'rb')) then
    open(unit=ifile,iostat=ios,file=file,status='old',action='read',&
         form='unformatted',access='STREAM')
endif
close(unit=ifile)
!
if(ios/= 0) goto 300
!
!----------------------------------------------------------------
!  check writable of a file
!----------------------------------------------------------------
ios = 0
if(strcmp(open_mode,'w').or.&
   strcmp(open_mode,'a').or.&
   strcmp(open_mode,'rw').or.&
   strcmp(open_mode,'ra')) then
  open(unit=ifile, iostat=ios,file=file,status='unknown',action='write',&
         form='formatted',access='SEQUENTIAL')
elseif(strcmp(open_mode,'wb').or.&
       strcmp(open_mode,'ab').or.&
       strcmp(open_mode,'rwb').or.&
       strcmp(open_mode,'rab')) then   
  open(unit=ifile, iostat=ios,file=file,status='unknown',action='write',&
         form='unformatted',access='STREAM')
endif
close(unit=ifile)
!
if(ios/= 0) goto 400
!
!----------------------------------------------------------------
!  open a file
!----------------------------------------------------------------
if(strcmp(open_mode,'r')) then
    open(unit=ifile,file=file,status='old',action='read',&
         form='formatted',access='SEQUENTIAL')
elseif(strcmp(open_mode,'w')) then
    open(unit=ifile,file=file,status='replace',action='write',&
         form='formatted',access='SEQUENTIAL')
elseif(strcmp(open_mode,'rw')) then
    open(unit=ifile,file=file,status='replace',action='readwrite',&
         form='formatted',access='SEQUENTIAL')
elseif(strcmp(open_mode,'a')) then
    open(unit=ifile,file=file,status='unknown',position='append',action='write',&
         form='formatted',access='SEQUENTIAL')
elseif(strcmp(open_mode,'ra')) then
    open(unit=ifile,file=file,status='unknown',position='append',action='readwrite',&
         form='formatted',access='SEQUENTIAL')
elseif(strcmp(open_mode,'rb')) then
    open(unit=ifile,file=file,status='old',action='read',&
         form='unformatted',access='STREAM')
elseif(strcmp(open_mode,'wb')) then
    open(unit=ifile,file=file,status='replace',action='write',&
         form='unformatted',access='STREAM')
elseif(strcmp(open_mode,'rwb')) then
    open(unit=ifile,file=file,status='replace',action='readwrite',&
         form='unformatted',access='STREAM')
elseif(strcmp(open_mode,'ab')) then
    open(unit=ifile,file=file,status='unknown',position='append',action='write',&
         form='unformatted',access='STREAM')
elseif(strcmp(open_mode,'rab')) then
    open(unit=ifile,file=file,status='unknown',position='append',action='readwrite',&
         form='unformatted',access='STREAM')
endif
return

100 continue
  ierr = err_file_nounit
  return
200 continue
  ierr = err_file_noexist
  return
300 continue
  ierr = err_file_cantread
  return
400 continue
  ierr = err_file_cantwrite
  return
end subroutine fopen

!> This routine deletes file according to fileunit
!<
!###########################################################################
subroutine fdel(ifile)     
!###########################################################################
!     deletes file
!===========================================================================
!>@param[in] ifile  -- file unit
!<
!===========================================================================
implicit none
integer,intent(in)  :: ifile

character*32 :: access, form
logical      :: exist
integer      :: ierr

inquire(unit=ifile,exist=exist,iostat=ierr)

if(ierr>0) return

if(exist) then
  inquire(unit=ifile,access=access,form=form)

  open(unit=ifile,access=access,form=form)
  close(ifile,status='delete')
end if

end subroutine fdel

!> This routine finds a line beginning with the key in the file
!<
!###########################################################################
subroutine findkey(unit,key,str,ierr)
!###########################################################################
!  read a line including a key from binary or ascii file
!===========================================================================
!>@param[in]  unit  -- file unit
!>@param[in]  key   -- keyword
!>@param[out] str   -- return the line 
!>@param[out] ierr  -- return error code
!<
!===========================================================================
use scheme,only: err_file_keyword
implicit none
integer,intent(in)           :: unit
character(len=*),intent(in)  :: key
character(len=*),intent(out) :: str
integer,intent(out)          :: ierr

logical,external :: strcmp

character :: key1*64

ierr = 0

do 
  call getline(unit,str,ierr)
  if(ierr/=0) goto 100
  
  call sreadstr(str,key1)
  if(strcmp(key1,key)) exit
end do 
return 

100 continue
ierr = err_file_keyword
return

end subroutine findkey

!> This routine finds a line beginning with the character in the file
!<
!###########################################################################
subroutine findc(unit,c,readlf,ierr)
!###########################################################################
!  read a character from binary or ascii file
!===========================================================================
!>@param[in]  unit   -- file unit
!>@param[in]  c      -- the character
!>@param[in ] readlf -- if read the line/feed 
!>@param[out] ierr   -- return error code
!<
!===========================================================================
use precision,only:byt32
use scheme
implicit none
integer,intent(in)           :: unit
character(len=1),intent(in)  :: c
logical,intent(in)           :: readlf
integer,intent(out)          :: ierr

integer        :: ios,n,len,ch
integer(byt32) :: idummy=0,idummy0
character      :: access*11,str*256

ierr = 0
inquire(unit=unit,access=access)

if(access(1:6)=='stream' .or. access(1:6)=='STREAM') then
  300 continue
  do
    idummy0 = idummy
    read(unit,iostat=ios,end=100) idummy
    if(ios/=0) exit
!    
    if((.not.readlf).and.char(idummy)==c) return
!
!  lf or cr 
    if(idummy==10 .or. idummy==13) exit
  end do
  if(ios==0 .and. char(idummy0)/=c) goto 300  
elseif(access(1:10)=='sequential' .or. access(1:10)=='SEQUENTIAL') then
  do
    read(unit,'(a)',iostat=ios,end=100) str
    if(ios/=0) exit
!
!   find character      
    len = len_trim(str)
    do n=1,len
      ch = iachar(str(n:n))
      if(ch>=33 .and. ch<=126) then
        exit
      endif
    enddo
    
    if(n<=len.and.str(n:n)==c) exit
  enddo
endif  
return

100 continue
ierr = err_file_char
return
end subroutine findc

!> This routine reads a valid line \n
!! some comment line beginning with # ! and etc will be omitted
!<
!###########################################################################
subroutine getline(unit,str,ierr)
!###########################################################################
!  read a line from binary & ascii file
!===========================================================================
!>@param[in]  unit   -- file unit
!>@param[out] str    -- return line 
!>@param[out] ierr   -- return error code
!<
!===========================================================================
use precision,only:byt32
use scheme,only:err_file_end
implicit none
integer,intent(in)           :: unit
character(len=*),intent(out) :: str
integer,intent(out)          :: ierr

logical,external  :: validline

integer           :: i,nlen,ios
integer(byt32)    :: idummy
character(len=11) :: access

ierr = 0
inquire(unit=unit,access=access)

nlen = len(str)

if(access(1:6)=='STREAM'.or. access(1:6)=='stream' ) then
  300 continue
  str=''
  i=0
  do
    read(unit,iostat=ios,end=100) idummy
    if(ios/=0) exit
!
!  lf or cr 
    if(idummy==10 .or. idummy==13) exit

    i=i+1
    if(i<=nlen) str(i:i) = char(idummy)
    
  end do
  if(ios==0 .and. (.not.validline(str))) goto 300  
elseif(access(1:10)=='SEQUENTIAL' .or. access(1:10)=='sequential') then
  str=''
  do
    read(unit,'(a)',iostat=ios,end=100) str
    if(ios/=0) exit
    if(validline(str)) exit
  enddo
endif  
return

100 continue
ierr = err_file_end
return
end subroutine getline


!> This routine write a line into a file
!<
!###########################################################################
subroutine wrtline(unit,str)
!###########################################################################
!  write a line with line feed into a binary & ascii file
!===========================================================================
!>@param[in]  unit   -- file unit
!>@param[in]  str    -- input line
!<
!===========================================================================
use precision,only:byt32
implicit none
integer,intent(in)          :: unit
character(len=*),intent(in) :: str

character :: access*11
integer   :: i

inquire(unit=unit,access=access)

if(access(1:6)=='stream' .or. access(1:6)=='STREAM') then
  do i=1,len_trim(str)
    write(unit) int(ichar(str(i:i)),byt32)
  end do
  write(unit) int(10,byt32)              ! 10 line feed lf
elseif(access(1:10)=='sequential' .or. access(1:10)=='SEQUENTIAL') then 
  write(unit,'(a)') str                  ! line feed is default
endif
end subroutine wrtline

!> This routine reads a real array from a file
!<
!###########################################################################
subroutine getreal(unit,nm,n,val,ist,ien,bsingle,ierr)
!###########################################################################
! read a scalar array arr(nm,n) section (m,ist:ien) from binary & ascii file
! the array will be cast into arr(m,n) and the outer dimension will be ist to ien 
!===========================================================================
!>@param[in]    unit    -- file unit
!>@param[in]    nm      -- dimension of val(nm,n)
!>@param[in]    n       -- dimension of val(nm,n)
!>@param[inout] val     -- real array val(nm,n)
!>@param[in]    ist     -- starting index of val(:,ist:ien)
!>@param[in]    ien     -- ending index of val(:,ist:ien)
!>@param[in]    bsingle -- if single precision type 
!>@param[out]   ierr    -- return error code
!<
!===========================================================================
use precision,only:dp,sp
use scheme,only:err_file_realarr
implicit none
integer,intent(in)     :: unit
integer,intent(in)     :: nm,n
real(dp),intent(inout) :: val(nm,n)
integer,intent(in)     :: ist,ien
logical,intent(in)     :: bsingle
integer,intent(out)    :: ierr

real(sp)  :: rarr(nm,n)
logical   :: bsp
character :: access*11

ierr=0

inquire(unit=unit,access=access)

bsp = bsingle

if(access(1:6)=='stream' .or. access(1:6)=='STREAM') then
  if(bsp) then
    read(unit,err=100) rarr(:,ist:ien)
    val(:,ist:ien) = dble(rarr(:,ist:ien))      
  else
    read(unit,iostat=ierr) val(:,ist:ien)
  endif
elseif(access(1:10)=='sequential' .or. access(1:10)=='SEQUENTIAL') then
  read(unit,*,err=100) val(:,ist:ien)  
endif  
return

100 continue
ierr = err_file_realarr
return
end subroutine getreal


!> This routine writes a real array into a file
!<
!###########################################################################
subroutine wrtreal(unit,nm,n,val,ist,ien)
!###########################################################################
!  write a scalar array val(m,n) section (m,ist:ien) to binary & ascii file
!===========================================================================
!>@param[in]    unit    -- file unit
!>@param[in]    nm      -- dimension of val(nm,n)
!>@param[in]    n       -- dimension of val(nm,n)
!>@param[in]    val     -- real array val(nm,n)
!>@param[in]    ist     -- starting index of val(:,ist:ien)
!>@param[in]    ien     -- ending index of val(:,ist:ien)
!<
!===========================================================================
use precision,only:dp
implicit none
integer,intent(in)  :: unit
integer,intent(in)  :: nm,n
real(dp),intent(in) :: val(nm,n)
integer,intent(in)  :: ist,ien

character :: access*11,form*30

inquire(unit=unit,access=access)

if(access(1:6)=='stream' .or. access(1:6)=='STREAM') then
  write(unit) val(:,ist:ien)
elseif(access(1:10)=='sequential' .or. access(1:10)=='SEQUENTIAL') then
  write(form,'(a1,i0,a)')  '(',nm,'(es19.11e3,1x))'
  write(unit,form)  val(:,ist:ien)
endif  
end subroutine wrtreal

!> This routine reads a integer array from a file
!<
!###########################################################################
subroutine getint(unit,nm,n,val,ist,ien,ierr)
!###########################################################################
!  read a integer  array val(n) section (m,ist:ien) from binary & ascii file
!===========================================================================
!>@param[in]    unit    -- file unit
!>@param[in]    nm      -- dimension of val(nm,n)
!>@param[in]    n       -- dimension of val(nm,n)
!>@param[inout] val     -- integer array val(nm,n)
!>@param[in]    ist     -- starting index of val(:,ist:ien)
!>@param[in]    ien     -- ending index of val(:,ist:ien)
!>@param[out]   ierr    -- return error code
!<
!===========================================================================
use scheme,only:err_file_intarr
implicit none
integer,intent(in)   :: unit
integer,intent(in)   :: nm,n
integer,intent(inout):: val(nm,n)
integer,intent(in)   :: ist,ien
integer,intent(out)  :: ierr

character :: access*11

ierr = 0
inquire(unit=unit,access=access)

if(access(1:6)=='STREAM' .or. access(1:6)=='stream') then
  read(unit,err=100) val(:,ist:ien)
elseif(access(1:10)=='SEQUENTIAL' .or. access(1:10)=='sequential') then
  read(unit,*,err=100) val(:,ist:ien)
endif  
return

100 continue
ierr = err_file_intarr
return
end subroutine getint

!> This routine writes a integer array into a file
!<
!###########################################################################
subroutine wrtint(unit,nm,n,val,ist,ien)
!###########################################################################
!  write a integer array val(n) section (m,ist:ien) to binary & ascii file
!===========================================================================
!>@param[in]    unit    -- file unit
!>@param[in]    nm      -- dimension of val(nm,n)
!>@param[in]    n       -- dimension of val(nm,n)
!>@param[in]    val     -- integer array val(nm,n)
!>@param[in]    ist     -- starting index of val(:,ist:ien)
!>@param[in]    ien     -- ending index of val(:,ist:ien)
!<
!===========================================================================
implicit none
integer,intent(in) :: unit
integer,intent(in) :: nm,n
integer,intent(in) :: val(nm,n)
integer,intent(in) :: ist,ien

character :: access*11,form*30

inquire(unit=unit,access=access)

if(access(1:6)=='STREAM' .or. access(1:6)=='stream') then
  write(unit) val(:,ist:ien)
elseif(access(1:10)=='SEQUENTIAL' .or. access(1:10)=='sequential') then
  write(form,'(a1,i0,a)')  '(',nm,'(i0,1x))'
  write(unit,form) val(:,ist:ien)
endif  
end subroutine wrtint

!> This routine reads a list array from a file while creating the list
!<
!###########################################################################
subroutine getlist(unit,n,val,ist,ien,ierr)
!###########################################################################
!  read a integer list val(n) section (ist:ien) from binary & ascii file
!===========================================================================
!>@param[in]    unit    -- file unit
!>@param[in]    n       -- dimension of val(n)
!>@param[inout] val     -- list array val(n)
!>@param[in]    ist     -- starting index of val(ist:ien)
!>@param[in]    ien     -- ending index of val(ist:ien)
!>@param[out]   ierr    -- return error code
!<
!===========================================================================
use precision,only:list
use scheme,only:err_file_listarr
use listlib,only:list_create
implicit none
integer,intent(in)       :: unit
integer,intent(in)       :: n
type(list),intent(inout) :: val(n)
integer,intent(in)       :: ist,ien
integer,intent(out)      :: ierr

integer   :: nf,i,itemp(10000)
character :: access*11

ierr = 0

inquire(unit=unit,access=access)

if(access(1:6)=='STREAM' .or. access(1:6)=='stream') then
  do i=ist,ien
    read(unit,err=100) nf
    call list_create(val(i),nf)
    read(unit,err=100) val(i)%ind
  end do
elseif(access(1:10)=='SEQUENTIAL' .or. access(1:10)=='sequential') then
  do i=ist,ien
    read(unit,*,err=100) nf,itemp(1:nf)
    call list_create(val(i),nf)
    val(i)%ind = itemp(1:nf)
  enddo 
endif  
return

100 continue
ierr = err_file_listarr
return
end subroutine getlist

!> This routine writes a list array into a file 
!<
!###########################################################################
subroutine wrtlist(unit,n,val,ist,ien)
!###########################################################################
!  write a integer list val(n) section (ist:ien) to binary & ascii file
!===========================================================================
!>@param[in]    unit    -- file unit
!>@param[in]    n       -- dimension of val(n)
!>@param[in]    val     -- list array val(n)
!>@param[in]    ist     -- starting index of val(ist:ien)
!>@param[in]    ien     -- ending index of val(ist:ien)
!<
!===========================================================================
use precision,only:list
use listlib,only:list_len
implicit none
integer,intent(in)   :: unit
integer,intent(in)   :: n
type(list),intent(in):: val(n)
integer,intent(in)   :: ist,ien

integer   :: i,nf
character :: fmt*20,access*11

inquire(unit=unit,access=access)

if(access(1:6)=='stream' .or. access(1:6)=='STREAM') then
  do i=ist,ien
    write(unit) list_len(val(i)),val(i)%ind
  end do
elseif(access(1:10)=='sequential' .or. access(1:10)=='SEQUENTIAL') then
  do i=ist,ien
    nf = list_len(val(i))
    write(fmt,'(a1,i0,a)') '(',nf+1,'(i0,x))'
    write(unit,trim(fmt))  nf,val(i)%ind
  end do
endif  
end subroutine wrtlist

!> This routine reads a face array from a file while creating the face
!<
!###########################################################################
subroutine getface(unit,n,face,own,nei,ist,ien,nv,ierr)
!###########################################################################
!  read a face list face(n) own(n) nei(n) section (ist:ien) from binary & ascii file
!===========================================================================
!>@param[in]    unit    -- file unit
!>@param[in]    n       -- dimension of face(n)
!>@param[inout] face    -- list array face(n)
!>@param[inout] own     -- list array own(n)
!>@param[inout] nei     -- list array nei(n)
!>@param[in]    ist     -- starting index of face(ist:ien),own(ist:ien),nei(ist:ien)
!>@param[in]    ien     -- ending index of face(ist:ien),own(ist:ien),nei(ist:ien)
!>@param[in]    nv      -- number of vertexs defining the face 
!>@param[out]   ierr    -- return error code
!<
!===========================================================================
use precision,only:list
use scheme,only:err_file_listarr
use listlib,only:list_create
implicit none
integer,intent(in)       :: unit
integer,intent(in)       :: n
type(list),intent(inout) :: face(n)
integer,intent(inout)    :: own(n),nei(n)
integer,intent(in)       :: ist,ien
integer,intent(in)       :: nv
integer,intent(out)      :: ierr

integer   :: nf,i,itemp(10000)
character :: access*11

ierr = 0

inquire(unit=unit,access=access)

if(access(1:6)=='STREAM' .or. access(1:6)=='stream') then
  if(nv>0) then
    do i=ist,ien
      call list_create(face(i),nv)
    end do
    do i=ist,ien
      read(unit,err=100) itemp(1:nv+2)
      face(i)%ind = itemp(1:nv)
      own(i)      = itemp(nv+1)
      nei(i)      = itemp(nv+2)
    end do
  else
    do i=ist,ien
      read(unit,err=100) nf
      call list_create(face(i),nf)
      read(unit,err=100) face(i)%ind,own(i),nei(i)
    end do
  endif
elseif(access(1:10)=='SEQUENTIAL' .or. access(1:10)=='sequential') then
  if(nv>0) then
    do i=ist,ien
      call list_create(face(i),nv)
    end do
    do i=ist,ien
      read(unit,*,err=100) face(i)%ind,own(i),nei(i)
    end do
  else
    do i=ist,ien
      read(unit,*,err=100) nf,itemp(1:nf+2)
      call list_create(face(i),nf)
      face(i)%ind = itemp(1:nf)
      own(i)      = itemp(nf+1)
      nei(i)      = itemp(nf+2)
    enddo
  endif 
endif  
return

100 continue
ierr = err_file_listarr
return
end subroutine getface


!> This routine writes a face array into a file
!<
!###########################################################################
subroutine wrtface(unit,n,face,own,nei,ist,ien)
!###########################################################################
!  write a integer list val(n) section (ist:ien) to binary & ascii file
!  n v1 v2 ... vn cL cR
!===========================================================================
!>@param[in]    unit    -- file unit
!>@param[in]    n       -- dimension of face(n)
!>@param[in]    face    -- list array face(n)
!>@param[in]    own     -- list array own(n)
!>@param[in]    nei     -- list array nei(n)
!>@param[in]    ist     -- starting index of face(ist:ien),own(ist:ien),nei(ist:ien)
!>@param[in]    ien     -- ending index of face(ist:ien),own(ist:ien),nei(ist:ien)
!<
!===========================================================================
use precision,only:list
use listlib,only:list_len
implicit none
integer,intent(in)   :: unit
integer,intent(in)   :: n
type(list),intent(in):: face(n)
integer,intent(in)   :: own(n),nei(n)
integer,intent(in)   :: ist,ien

integer   :: i,lens(n)
character :: fmt*20,access*11

inquire(unit=unit,access=access)

do i=ist,ien
  lens(i) = list_len(face(i))
end do

if(access(1:6)=='stream' .or. access(1:6)=='STREAM') then
  do i=ist,ien
    write(unit) lens(i),face(i)%ind,own(i),nei(i)
  end do
elseif(access(1:10)=='sequential' .or. access(1:10)=='SEQUENTIAL') then
  do i=ist,ien
    write(fmt,'(a1,i0,a)') '(',lens(i)+3,'(i0,x))'
    write(unit,trim(fmt))  lens(i),face(i)%ind,own(i),nei(i)
  end do
endif  
end subroutine wrtface

