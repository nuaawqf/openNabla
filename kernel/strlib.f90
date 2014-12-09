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
!! Contains subroutines toupper(),tolower(),strcat(),sreadstr(),sreadreal(),
!! sreadint(),strblk(),sreverse(),strsub(),strcomp()
!! and functions iscomment(),isspace(),strfnb(),strlnb(),strlen(),strcmp(),
!! stridx(),validline(),itoc()
!<

!############################################################################
!module strlib
!############################################################################
! Fortran 95 module character_functions
!============================================================================
!*-------*-------*-------*-------*-------*-------*-------*-------*
!| 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
!| 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |
!| 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|
!| 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |
!| 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
!| 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
!| 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
!| 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
!| 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
!| 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
!| 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
!| 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
!| 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
!|104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
!|112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
!|120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|
!*-------*-------*-------*-------*-------*-------*-------*-------*
!  0-del         isascii :
!  a-z A-Z       isalpha :
!  0-30          iscntrl :  control character
!  0-9 a-z A-Z   isalnum :  alpha and numeric
!  A-F a-f 0-9   isxdigit:
!============================================================================

!>This function determines if the character is a comment
!>@details comment character including / \ ! % # |
!>@param[in]   ch   -- a character
!<
!#############################################################################
logical function iscomment(ch)
!#############################################################################
!inline bool Foam::word::valid(char c)
!{
!    return
!    (
!        !isspace(c)
!     && c != '"'   // string quote
!     && c != '\''  // string quote
!     && c != '/'   // path separator
!     && c != ';'   // end statement
!     && c != '{'   // beg subdict
!     && c != '}'   // end subdict
!    );
!}
!============================================================================
implicit none
character(len=1),intent(in) :: ch

select case(ch)
  case('/','\','#','!','%','|')
   iscomment = .true.
  case default
   iscomment = .false.
end select 
end function iscomment


!>This function determines if the character is a space
!>@details comment character including asc_ht,asc_lf,asc_vt,asc_cr,asc_sp
!>@param[in]   ch   -- a character
!<
!############################################################################
logical function isspace( ch)
!############################################################################
!  isspace(): true if input ascii character is space
!  inline bool isspace(char c)
!  {
!   return(
!    c == ' '  ascii_sp || c == '\n' ascii_lf ||
!    c == '\r' ascii_cr || c == '\t' ascii_ht ); 
!}  //code from c++
!
! asc_ht = 9   asc_lf = 10  
! asc_vt = 11  asc_cr = 13
! asc_sp = 32
!============================================================================
implicit none
character( len= 1), intent( in) :: ch

integer :: ich
                              
ich = iachar( ch)             
select case(ich)             
  case(9,10,11,13,32) 
    isspace = .true.                                  
  case default
    isspace = .false.                                 
end select                                   
return                                       
end function isspace

!>changes the lowercase character to uppercase
!>@param[in]   s   -- a character
!<
!############################################################################
subroutine toupper(s)
!############################################################################
!  toupper(): if lowercase, change to uppercase
!  asc_lca = 97  asc_lcz = 122
!===========================================================================
implicit        none
character(len=*),intent(inout) :: s

integer           :: i,ich

do i = 1 , len_trim(s)
  ich = iachar(s(i:i)) 

  select case(ich)
    case(97: 122) 
      s(i:i) = achar( ich - 32)                      ! change to upper case
  end select 
end do  
end subroutine toupper

!>changes the uppercase character to lowercase
!>@param[in]   s   -- a character
!<
!############################################################################
subroutine tolower(s)
!############################################################################
!  tolower(): if uppercase, change to lowercase
! asc_uca = 65 asc_ucz = 90
!===========================================================================
implicit        none
character(len=*),intent(inout) :: s

integer           :: i,ich

do i = 1 , len_trim(s)
  ich = iachar( s(i:i)) 

  select case( ich)
    case( 65: 90) 
      s(i:i) = achar( ich + 32)   
  end select 
end do  
end subroutine tolower


!>finds the position where the first non-blank character locates in string
!>@param[in]   str   -- a string
!<
!############################################################################
function strfnb(str) result(n)
!############################################################################
!  Find first non-blank character in string
!  asc_sp  = 32    asc_del = 127
!============================================================================
implicit none
character(len=*),intent(in) :: str
integer                     :: n

integer :: len,ch

len = len_trim(str)
do n=1,len
   ch = iachar(str(n:n))
   if(ch>32 .and. ch<127) return
enddo
n=0
end function strfnb

!>finds the position where the last non-blank character locates in string
!>@param[in]   str   -- a string
!<
!############################################################################
function strlnb(str) result(n)
!############################################################################
! returns last non-blank character position from a string
!  asc_sp  = 32    asc_del = 127
!============================================================================
implicit none
character(len=*),intent(in) :: str
integer                     :: n

integer :: len,ch

len=len_trim(str)
do n=len,1,-1
   ch = iachar(str(n:n))
   if(ch>32 .and. ch<127) return
enddo
n=0 
end function strlnb

!>calculates the string non-blank length 
!>@param[in]   st   -- a string
!<
!############################################################################
integer function strlen(st)
!############################################################################
!  returns the length of a string (position of last non-blank
!============================================================================
implicit none
character(len=*), intent(in)  :: st

integer,external :: strlnb
integer :: len

len = strlnb(st)

do strlen=len,1,-1
  if(st(strlen:strlen)/=' ') return
end do
strlen = 0  
end function strlen


!>compares two strings ignoring upper/lower case differeneces
!>@param[in]   sa   -- a string
!>@param[in]   sb   -- a string
!<
!############################################################################
logical function strcmp(sa,sb)
!############################################################################
! Purpose: Compare character strings for match
!               Ignores upper/lower case differences.
!      Inputs:
!         sa(*)   - Character string 1
!         sb(*)   - Character string 2
!      Outputs:
!         strcmp  - Flag, true if sa = sb
!============================================================================
implicit none
character( len= *), intent( in) :: sa     ! the first string
character( len= *), intent( in) :: sb     ! the second string
!
integer :: i,i1,i2,j1,j2,ia,ib,inc,loop_len
integer :: strlnb,strfnb
!
inc = ichar('A') - ichar('a')

i1=strfnb(sa);  i2=strlnb(sa)
j1=strfnb(sb);  j2=strlnb(sb)
!
strcmp = .false.
!
if((i2-i1)/=(j2-j1)) return
!
loop_len = i2-i1+1
!
!  loop thru each character
!
do i =1,loop_len
!
  ia = iachar( sa(i1+i-1:i1+i-1) )  
  ib = iachar( sb(j1+i-1:j1+i-1) )  
!
  if(ia.ne.ib .and. ia+inc.ne.ib .and. ia.ne.ib+inc ) return
enddo                                            
strcmp = .true.
end function strcmp

!>finds the location where the word is in string array
!>@param[in]   word   -- a string
!>@param[in]   n      -- dimension of string array name(n)
!>@param[in]   name   -- string array name(n)
!<
!############################################################################
function stridx(word,n,name)
!############################################################################
!  Returns index of first string in vector vstring which 
!  contains substring key ...
!==========================================================================
implicit none
character(len=*),intent(in)  :: word
integer,intent(in)           :: n
character(len=*),intent(in)  :: name(n)
integer                      :: stridx

logical :: strcmp
integer :: i

stridx=0
do i=1,n 
  if(strcmp(word,name(i))) then
    stridx = i
    exit
  endif
end do
end function stridx

!>concat two strings
!>@param[in]   sa   -- string a
!>@param[in]   sb   -- string b
!<
!############################################################################
subroutine strcat(sa, sb)
!############################################################################
!  string_cat() copy string_b into string_a following all nonblanks
!!============================================================================
implicit none
character( len= *), intent( inout) :: sa  ! the string containing the substrings
character( len= *), intent( in)    :: sb  ! the target substring

integer :: len1,len2,lens,i,ii
integer :: strlen

len1 = strlen(sa);   len2 = strlen(sb)
lens = min(len(sa),len1+len2)
!
!  concatenate string_b after len_trim( string_a)
!
ii =0
do i=len1+1,lens
  ii = ii +1
  sa(i:i) = sb(ii:ii)
end do

do i=lens+1,len(sa)
  sa(i:i)=''
end do
return               
end subroutine strcat

!>take a string from a line
!>@param[inout]   line   -- string line
!>@param[out]     str    -- the string taken from line
!<
!############################################################################
subroutine sreadstr(line,str)
!############################################################################
!  get a string from a line 
!============================================================================
implicit none
character(len=*),intent(inout) :: line
character(len=*),intent(out)   :: str

integer :: ist,ien,n,i

integer :: strlnb
logical :: isspace

n   = strlnb(line) 

do ist = 1,n 
  if(.not.isspace(line(ist:ist))) exit  
end do    

ien = n
do i = ist+1,n
  if(isspace(line(i:i))) then
    ien =i-1
    exit
  endif  
end do    

if(ist<=ien) then
  str = line(ist:ien)
else
  str =''
endif    

do ist = ien+1,n 
  if(.not.isspace(line(ist:ist)))  exit 
enddo

if(ist<=n) then
  line= line(ist:n)
else
  line=''
endif  
end subroutine sreadstr

!>read a integer from a string
!>@param[in]   str   -- a string 
!>@param[out]  val   -- the integer read from string
!>@param[out]  ierr  -- return error code
!<
!###########################################################################
subroutine sreadint(str,val,ierr)
!###########################################################################
!  read a integer from string
!===========================================================================
use scheme,only:err_str_toint
implicit none
character(len=*),intent(in) :: str
integer,intent(out)         :: val
integer,intent(out)         :: ierr

integer  :: ios

ierr = 0
read(str,'(i32)',iostat=ios) val
if(ios/=0) ierr= err_str_toint
end subroutine sreadint

!>read a real from a string
!>@param[in]   str   -- a string 
!>@param[out]  res   -- the real value read from string
!>@param[out]  ierr  -- return error code
!<
!############################################################################
subroutine sreadreal(str,res,ierr) 
!############################################################################
! get real number from string - format: [blanks][+|-][nnn][.nnn][e|e|d|d[+|-]nnn]
!============================================================================
use precision,only:dp
use scheme,only:err_str_toreal
implicit none
character (len=*),intent(in) :: str        ! string
real(dp),intent(out)         :: res        ! real number
integer,  intent(out)        :: ierr      ! error flag
    
integer ::  nlen

integer ::  ib,in,istat,ich
logical ::  bflag,&               ! .t. at begin of number in str
            inman,&               ! .t. in mantissa of number
            pflag,&               ! .t. after 1st '.' encountered
            eflag,&               ! .t. at exponent identifier 'eedd'
            inexp,&               ! .t. in exponent of number
            dinman,&              ! .t. if at least 1 digit in mant.
            dinexp,&              ! .t. if at least 1 digit in exp.
            err

integer,parameter :: asc_sp  = 32       ! space
integer,parameter :: asc_pls = 43       ! + plus
integer,parameter :: asc_mns = 45       ! - minus
integer,parameter :: asc_0   = 48       ! 0 zero
integer,parameter :: asc_9   = 57       ! 9 nine
integer,parameter :: asc_prd = 46       ! . period
integer,parameter :: asc_ucd = 68       ! upper case D
integer,parameter :: asc_uce = 69       ! upper case E
integer,parameter :: asc_lcd = 100      ! lower case d
integer,parameter :: asc_lce = 101      ! lower case e
integer,parameter :: asc_ep  = 33       ! exclamation 

bflag=.true.;   inman=.false.; pflag=.false.; eflag=.false.; inexp=.false.
dinman=.false.; dinexp=.false.

ib   = 1;  in   = 1
nlen = len_trim(str)

do while(in <=nlen)
   ich = iachar(str(in:in))
   select case (ich)
     case (asc_sp)                                  ! only leading blanks permitted
       ib = ib+1
       if (inman .or. eflag .or. inexp) exit
     case (asc_pls,asc_mns)                         ! + - permitted only
      if(bflag) then           
         inman=.true.; bflag=.false.                ! - at beginning of mantissa
      elseif (eflag) then               
         inexp=.true.; eflag=.false.                ! - at beginning of exponent
      else
         exit                                       ! - otherwise stop
      endif
     case (asc_0:asc_9)                             ! - mark
      if     (bflag) then           
         inman=.true.; bflag=.false.                ! - beginning of mantissa
      elseif (eflag) then               
         inexp=.true.; eflag=.false.                ! - beginning of exponent
      endif
      
	  if (inman) dinman=.true.                      ! - mantissa contains digit
      if (inexp) dinexp=.true.                      ! - exponent contains digit
     case (asc_prd)
      if(bflag) then
          pflag=.true.                              ! - mark 1st appearance of '.'
          inman=.true.; bflag=.false.               !   mark beginning of mantissa
      elseif (inman .and..not.pflag) then
          pflag=.true.                              ! - mark 1st appearance of '.'
      else
          exit                                      ! - otherwise stop
      end if
     case (asc_uce,asc_lce,asc_ucd,asc_lcd)         ! - e E d D permitted only
      if (inman) then
         eflag=.true.; inman=.false.                ! - following mantissa
      else
         exit                                       ! - otherwise stop
      endif
     case default
      exit                                          ! - stop at all other characters
   end select
   in = in+1
 end do
 
err = (ib > in-1) .or. (.not.dinman) .or. ((eflag.or.inexp).and..not.dinexp)
if (err) then
   res = 0.0
else
   read(str(ib:in-1),*,iostat=istat) res
   err = istat /= 0
end if

ierr = 0
if(err) ierr = err_str_toreal
end subroutine  sreadreal

!>if a string is a valid line, comment character will be omitted
!>@param[in]   str   -- a string 
!<
!#######################################################################
logical function validline(str)
!#######################################################################
implicit none
character(len=*),intent(inout) :: str

integer :: ich1,ich2,lenk
integer :: strfnb,strlnb
logical :: iscomment

validLine=.false.

ich1 = strfnb(str)
if(ich1<1) return
if(iscomment(str(ich1:ich1))) return
    
ich2 = strlnb(str)
if(ich2<ich1)   return

validLine = .true.

if(str(ich2:ich2)==';') ich2=ich2-1
   
lenk=ich2-ich1+1
str(1:lenk) = str(ich1:ich2)
str(lenk+1:)= ''
end function validline


!############################################################################
subroutine strcomp(str)
!############################################################################
! Converts multiple spaces and tabs to single spaces; deletes control characters;
! removes initial spaces.
!================================================================================
implicit none
character(len=*),intent(inout) :: str

character(len=1)             :: ch
character(len=len_trim(str)) :: outstr

integer           :: lenstr,isp,k,i,ich

integer,parameter :: asc_ht = 9   
integer,parameter :: asc_lf = 10  
integer,parameter :: asc_vt = 11
integer,parameter :: asc_cr = 13
integer,parameter :: asc_sp = 32
integer,parameter :: asc_ep  = 33
integer,parameter :: asc_tld = 126


str    = adjustl(str)
lenstr = len_trim(str)
outstr = ' '
isp    = 0
k      = 0

do i=1,lenstr
  ch  = str(i:i)
  ich = iachar(ch)
  
  select case(ich)
    case(asc_ht,asc_vt, asc_lf,asc_sp)      ! space or tab character
      if(isp==0) then
        k=k+1
        outstr(k:k)=' '
      end if
      isp=1
    case(asc_ep:asc_tld)                    ! not a space, quote, or control character
      k=k+1
      outstr(k:k)=ch
      isp=0
  end select
end do

str=adjustl(outstr)
end subroutine strcomp

!>substract string according to incl, for example [] {} ()
!>@param[in]   str   -- a string 
!>@param[in]   incl  -- include characters
!>@param[out]  sub   -- substract string
!<
!############################################################################
subroutine strsub(str,incl,sub)
!############################################################################
! substract string according to incl, for example [] {} ()
!============================================================================
implicit none
character *(*),intent(in)   :: str
character(len=2),intent(in) :: incl
character *(*),intent(out)  :: sub

integer :: i, ifirst, ilast, slen, icnt, ssub

slen = len_trim(str)
ssub = len(sub)

ifirst  = -1
ilast   = -1
sub     = ''

i = 1
do while (i.le.slen.and.ilast.eq.-1)
    if (str(i:i)==incl(1:1)) then
      if (ifirst==-1) ifirst = i
    elseif(str(i:i)==incl(2:2)) then
      ilast = i
    endif
    i = i + 1
end do

if (ifirst.ne.-1.and.ilast.ne.-1) then
    if (ifirst+1.le.ilast-1) then
      icnt = 1
      do i = ifirst, ilast
        sub(icnt:icnt) = str(i:i)
        icnt = icnt + 1
		if(icnt>ssub) return
      end do
    endif
endif
return
end subroutine strsub

!>reverse a string
!>@param[in]   word  -- a string 
!<
!############################################################################
function sreverse(word)
!############################################################################
implicit none
character(len=*),intent(in) :: word
character(len=len(word))    :: sreverse

integer :: lw,i
lw = len(word)
!
! reverse characters
!
do i=1,lw
  sreverse(lw-i+1:lw-i+1)=word(i:i)
end do
end function sreverse  

!>converts integer to character representation
!>@param[in]   irep  -- a integer 
!<
!############################################################################
character(len=16) function  itoc(irep)
!############################################################################
! converts integer to character representation.
!-------------------------------------------------------------------------------
implicit none
integer,intent(in) :: irep

character ::  nums*10 ='0123456789' 
integer   ::  curs, dig,  k, mag, lcirep, lower,upper

itoc = ' '
lcirep = irep
if( lcirep==0 ) then
  itoc(1:1) = '0'
  return
else if( lcirep<0 ) then
  itoc(1:1) = '-'
  lcirep = -lcirep
  curs = 2
else
  curs = 1
end if

lower = 1
do mag = 1 , 10
  upper = 10 * lower
  if( lcirep>=lower .and. lcirep<upper ) exit
  lower = upper
end do

do k = mag , 1 , -1
  dig = lcirep / (10 ** (k - 1))
  itoc(curs:curs) = nums(dig + 1:dig + 1)
  lcirep = lcirep - dig * (10 ** (k - 1))
  curs = curs + 1
end do
end function itoc

!>converse ( and ) to blank in a string
!>@param[in]   str  -- a string
!<
!############################################################################
subroutine strblk(str)
!############################################################################
! convert '(' or ')' into blank
!-------------------------------------------------------------------------------
implicit none
character(len=*),intent(inout) :: str

integer :: ich,i

do i=1,len_trim(str)
  ich = iachar(str(i:i))             
  
  if(ich==40 .or. ich==41) str(i:i)=' '
end do
end subroutine



