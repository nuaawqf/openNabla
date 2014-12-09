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
!! Contains Module listlib,which defines integer list 
!<

!> 
!! Subroutines include list_create(),list_clone(),list_assign(),list_del(),
!! list_setsize(),list_add(),list_reverse(),list_prn(). Functions include 
!! list_len(), list_eq(), list_comp(),list_rmhead() 
!<
          
!############################################################################
module listlib
!############################################################################
!  log : 2014/1/3 remove list_null
!                 bug fixed in list_create
!============================================================================
use precision,only:list
implicit none
private
save

integer,public :: list_num=0
!
public :: list_create,list_clone,list_assign,list_del,list_setsize,&
          list_len,list_add, list_eq, list_comp, list_reverse,list_prn,&
          list_rmhead

interface list_assign
  module procedure list_assign_l
  module procedure list_assign_a
end interface

interface list_del
  module procedure list_del_l
end interface

interface list_add
  module procedure list_add_i
  module procedure list_add_ia
end interface   

contains

!>This subroutine creates a list of length n
!>@param[in]   var  -- list
!>@param[in]   n    -- list length
!<
!############################################################################
subroutine list_create(var,n) 
!############################################################################
!----- list creation
!============================================================================
implicit none
type(list),intent(inout) :: var 
integer,intent(in)       :: n

integer:: istat

if(n<1) return

if(allocated(var%ind)) then
  if(size(var%ind)==n) then
    var%ind =0
    return
  else
    call list_del(var)
  endif
endif

allocate(var%ind(n),stat=istat)
list_num = list_num +n
end subroutine list_create

!>This subroutine deletes a list 
!>@param[inout]   var  -- list
!<
!############################################################################
subroutine list_del_l(var)
!############################################################################
implicit none
type(list),intent(inout) :: var

integer:: istat

if(allocated(var%ind)) then
   list_num = list_num -size(var%ind)
   deallocate(var%ind,stat=istat)
endif
end subroutine list_del_l

!>This subroutine clones a list : var = expr
!>@param[inout]   var  -- list
!>@param[in]      expr -- list
!<
!############################################################################
subroutine list_clone(var,expr) 
!############################################################################
!----- list clone
!============================================================================
implicit none
type(list),intent(inout):: var 
type(list),intent(in)   :: expr 

integer :: n

n = list_len(expr)

if(n==0) return
call list_create(var,n)

if(loc(var%ind)==loc(expr%ind)) then
  return
else
  var%ind = expr%ind
endif

endsubroutine list_clone

!>This subroutine adds a integer into list var
!>@param[inout]   var  -- list
!>@param[in]      i    -- a integer
!<
!############################################################################
subroutine list_add_i(var,i)   
!############################################################################
!------ list add operators --------------------- 
!============================================================================
implicit none
type(list),intent(inout)  :: var
integer,intent(in)        :: i

integer,allocatable:: olddat(:)
integer:: oldn,istat

if(allocated(var%ind)) then
  oldn   = size(var%ind)
  allocate(olddat(oldn),stat=istat)
  olddat = var%ind
  
  call list_del(var)
  call list_create(var,oldn+1)

  var%ind(1:oldn)        = olddat
  var%ind(oldn+1:oldn+1) = i
  deallocate(olddat,stat=istat)
else
  call list_create(var,1)
  var%ind(1) = i
endif
endsubroutine list_add_i

!>This subroutine adds a integer array into list var
!>@param[inout]   var  -- list
!>@param[in]      m    -- array dimension
!>@param[in]      v    -- integer array v(m)
!<
!############################################################################
subroutine list_add_ia(var,m,v)   
!############################################################################
!------ list add operators --------------------- 
!============================================================================
implicit none
type(list),intent(inout)  :: var 
integer,intent(in)        :: m
integer,intent(in)        :: v(m) 

integer,allocatable:: olddat(:)
integer:: oldn,istat

if(allocated(var%ind)) then
  oldn   = size(var%ind)
  allocate(olddat(oldn),stat=istat)
  olddat = var%ind
  
  call list_del(var)
  call list_create(var,oldn+m)

  var%ind(1:oldn)        = olddat
  var%ind(oldn+1:oldn+m) = v
  deallocate(olddat,stat=istat)
else
  call list_create(var,m)
  var%ind = v
endif
endsubroutine list_add_ia

!>This subroutine remove the first element in list var
!>@param[inout]   var  -- list
!<
!############################################################################
integer function list_rmhead(var)   
!############################################################################
!------ list remove header operators --------------------- 
!============================================================================
implicit none
type(list),intent(inout)  :: var

integer,allocatable:: olddat(:)
integer:: oldn,istat

oldn   = size(var%ind)

list_rmhead = 0  
if(oldn<1) then
  return
elseif(oldn==1) then
  list_rmhead = var%ind(1)
  call list_del(var)
elseif(oldn>1) then
  list_rmhead = var%ind(1)
  allocate(olddat(oldn-1),stat=istat)
  olddat = var%ind(2:oldn)
  
  call list_del(var)
  call list_create(var,oldn-1)

  var%ind = olddat
  deallocate(olddat,stat=istat)
endif
endfunction list_rmhead

!>This subroutine resizes list var
!!the origin data will be saved 
!>@param[inout]   var  -- list
!>@param[in]      n    -- modified list length
!<
!############################################################################
subroutine list_setsize(var,n)   
!############################################################################
!------ list setsize operators ----------------- 
!============================================================================
implicit none
type(list),intent(inout)  :: var 
integer,intent(in)        :: n 

integer,allocatable:: olddat(:)
integer :: oldn,istat

if(n<=0) then
  call list_del(var)
  return  
endif

if(.not.allocated(var%ind)) then
  call list_create(var,n)
  return
else
  oldn   = size(var%ind)
  if(oldn/=n) then
    allocate(olddat(1:oldn),stat=istat)
    olddat = var%ind
  
    call list_del(var)
    call list_create(var,n)
  
    if(oldn<n) then
      var%ind(1:oldn) = olddat(1:oldn)
    else
      var%ind(1:n)    = olddat(1:n)
    end if
    deallocate(olddat,stat=istat)
  endif
endif
endsubroutine list_setsize 

!>This subroutine assigns var = expr
!>@param[inout]   var  -- list
!>@param[in]      expr -- list
!<
!############################################################################
subroutine list_assign_l(var,expr) 
!############################################################################
!        assignment procedures
!============================================================================
implicit none
type(list),intent(inout):: var 
type(list),intent(in)   :: expr 

!  assign a list value to a list variable overriding default assignement 
!  reallocates list variable to size of list value and copies data 

if(.not.allocated(expr%ind)) return

if(loc(var%ind)==loc(expr%ind))   return  

call list_create(var,size(expr%ind))
var%ind = expr%ind
endsubroutine list_assign_l

!>This subroutine assigns a integer array to list var
!>@param[inout]   var  -- list
!>@param[in]      expr -- integer array
!<
!############################################################################
subroutine list_assign_a(var,expr) 
!############################################################################
!       assignment procedures
!============================================================================
implicit none
type(list),intent(inout) :: var 
integer,intent(in)       :: expr(:) 

integer  :: n

!  assign a list value to a list variable overriding
!  default assignement reallocates list variable to 
!  size of list value and copies data 

n=size(expr)
if(n<1) return

call list_create(var,n)
var%ind = expr
endsubroutine list_assign_a


!>This function determines if list var_a=var_b
!>@param[in]   var_a -- list
!>@param[in]   var_b -- list
!<
!############################################################################
function list_eq(var_a,var_b)  result(ret)
!############################################################################
!    equality comparison operators 
!============================================================================
implicit none
type(list),intent(in) :: var_a,var_b 

logical :: ret
integer :: la,lb
 
ret = .false.

la = size(var_a%ind); lb = size(var_b%ind)

if(la<=0 .or. lb <=0 .or. la/=lb) return

ret = all(var_a%ind == var_b%ind) 
endfunction list_eq 
 
!>This function determines if the elements of list var_a and var_b contains
!!each other
!>@param[in]   var_a -- list
!>@param[in]   var_b -- list
!<
!############################################################################
function list_comp(var_a,var_b)  
!############################################################################
!------ equality comparison operators ---------
!============================================================================
implicit none
type(list),intent(in):: var_a,var_b 
logical       :: list_comp

integer       :: la,lb,i,n

list_comp = .false.

la = size(var_a%ind); lb = size(var_b%ind)
if(la<=0 .or. lb <=0 .or. la/=lb) return

n = 0
do i=1,la
   if(any(var_b%ind==var_a%ind(i))) n=n+1
end do  
if(n/=la) return

n = 0
do i=1,lb
   if(any(var_a%ind==var_b%ind(i))) n=n+1
end do  
if(n/=lb) return
list_comp = .true.

endfunction list_comp

!>This subroutine reverses the order of elements in list var
!>@param[in]   var -- list
!<
!############################################################################
subroutine list_reverse(var) 
!############################################################################
implicit none
type(list),intent(inout)  :: var

integer,allocatable:: dat(:)
integer:: n,i,istat

if(.not.allocated(var%ind)) return

n =  size(var%ind)
allocate(dat(1:n),stat=istat) 

dat = var%ind

do i=2,n
  var%ind(i) = dat(n+2-i)
enddo

deallocate(dat,stat =istat)
endsubroutine list_reverse

!>This function calculates the length of list var
!>@param[in]   var -- list
!<
!############################################################################
integer function list_len(var)
!############################################################################
implicit none
type(list),intent(in)  :: var

if(.not.allocated(var%ind)) then
  list_len =0
else
  list_len = size(var%ind)
end if
end function list_len


!>This subroutine print list var
!>@param[in]   var -- list
!<
!############################################################################
subroutine list_prn(var)
!############################################################################
implicit none
type(list),intent(in) :: var

integer           :: n
character(len=64) :: form

if(.not.allocated(var%ind)) then
  write(*,*) 'NOT ALLOCATED LIST'
  return
endif  

n=size(var%ind)
write(form,'(a,i0,a)') "(i0,a1,",n,"(i0,x),tl1,a1)"

write(*,form) n,'(',var%ind,')'
endsubroutine list_prn

end module listlib


