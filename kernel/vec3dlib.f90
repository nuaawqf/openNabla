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
!! Contains Module vec3dlib,which defines vector and tensor calculus
!<

!> Contains three dimensional vector and tensor calculus
!! Functions includes vdot(),sizedot(),tdot(),ddotprod(),crossprod(),
!! outerprod(),vcos(),inverse(),transform(),transpose(),vsqr(),mag(),
!! magsqr(),norm(),symm(),skew(),tr(),dev(),dev2(),diag(),det(),vrot(),
!! rodrigues(),inv(),validcmpt()
!<
!############################################################################
module vec3dlib
!############################################################################
!        three dimensional vector module
!============================================================================
!attention:
! a physical variable can be presented as 
!          a scalar, p,c...
!          a vector, [u1 u2 u3] or ui<i=1,3>
!          a system tensor [r11 r12 r12 r22 r23 r33]
!          a tensor [t11 t12 t13 t21 t22 t23 t31 t32 t33]
!
! geometrical vector, for examples, surface area vector s,normal surface vector n 
! and distance vector d can be present as
!         s=  [s1 s2 s3]t    d = [d1 d2 d3]t  n = [n1 n2 n3]t
!
! the gradient of physical varialbe can be presented as
!                |dp/dx1| 
!   ¨Œp= dp/dxi =|dp/dx2|       <i=1,3> 
!                |dp/dx3| 
!
!                                 | du1/dx1 du2/dx1 du3/dx1 |
!   ¨Œu= duj/dxi=(¨Œu1,¨Œu2,¨Œu3)=| du1/dx2 du2/dx2 du3/dx2 |  <i=1..3>,<j=1..3>
!                                 | du1/dx3 du2/dx3 du3/dx3 |
!                                 
!   ¨Œr= drmn/dxi=(¨Œr11,¨Œr12,¨Œr13,¨Œr22,¨Œr23,¨Œr33)      <i=1..3>
!                 |dr11/dx1 dr12/dx1 dr13/dx1 dr22/dx1 dr23/dx1 dr33/dx1|
!                =|dr11/dx2 dr12/dx2 dr13/dx2 dr22/dx2 dr23/dx2 dr33/dx2|
!                 |dr11/dx3 dr12/dx3 dr13/dx3 dr22/dx3 dr23/dx3 dr33/dx3|
!
!   ¨Œt= dtmn/dxi=(¨Œt11,¨Œt12,¨Œt13,¨Œt21,¨Œt22,¨Œt23,¨Œt31,¨Œt32,¨Œt33) <m=1..3>,<n=1..3> <i=1..3>
!                 |dt11/dx1 dt12/dx1 dt13/dx1 dt21/dx1 dt22/dx1 dt23/dx1,dt31/dx1,dt32/dx1,dt33/dx1|
!                =|dt11/dx2 dt12/dx2 dt13/dx2 dt21/dx2 dt22/dx2 dt23/dx2,dt31/dx2,dt32/dx2,dt33/dx2|
!                 |dt11/dx3 dt12/dx3 dt13/dx3 dt21/dx3 dt22/dx3 dt23/dx3,dt31/dx3,dt32/dx3,dt33/dx3|
!
!  geometrical vector can be dotted with physical variables and gradient,
!  in the dot calculation, geometrical vector must be presented as (n1,n2,n3) or (s1,s2,s3)
!
!   n.u = n1*u1+n2*u2+n3*u3    vector dot product
!
!   s.u = s1*u1+s2*u2+s3*u3    vector dot product
!
!                    |t11 t12 t13|                        |r11 r12 r13| 
!   n.t = (n1,n2,n3).|t21 t22 t23|       n.r = (n1,n2,n3).|    r22 r23|
!                    |t31 t32 t33|                        |        r33|
!                     
!   n.¨Œt = (n1,n2,n3).(¨Œt11,¨Œt12,¨Œt13,¨Œt21,¨Œt22,¨Œt23,¨Œt31,¨Œt32,¨Œt33)
!                    |dt11/dx1 dt12/dx1 dt13/dx1 dt21/dx1 dt22/dx1 dt23/dx1,dt31/dx1,dt32/dx1,dt33/dx1|
!         =(n1,n2,n3)|dt11/dx2 dt12/dx2 dt13/dx2 dt21/dx2 dt22/dx2 dt23/dx2,dt31/dx2,dt32/dx2,dt33/dx2|
!                    |dt11/dx3 dt12/dx3 dt13/dx3 dt21/dx3 dt22/dx3 dt23/dx3,dt31/dx3,dt32/dx3,dt33/dx3|
!         =(n.¨Œt11,n.¨Œt12,n.¨Œt13,n.¨Œt21,n.¨Œt22,n.¨Œt23,n.¨Œt31,n.¨Œt32,n.¨Œt33)
!
!   n.¨Œr = (n1,n2,n3).(¨Œr11,¨Œr12,¨Œr13,¨Œr22,¨Œr23,¨Œr33)
!                      |dr11/dx1 dr12/dx1 dr13/dx1 dr22/dx1 dr23/dx1 dr33/dx1|
!         = (n1,n2,n3).|dr11/dx2 dr12/dx2 dr13/dx2 dr22/dx2 dr23/dx2 dr33/dx2|
!                      |dr11/dx3 dr12/dx3 dr13/dx3 dr22/dx3 dr23/dx3 dr33/dx3|
!         =(n.¨Œr11,n.¨Œr12,n.¨Œr13,n.¨Œr22,n.¨Œr23,n.¨Œr33)
!
!   cross product between two vector
!
!   u cross v 
!
!   outer product of geometrical vector and physical variable
!                |s1|    |p*s1|
!   s¨’p =si p = |s2|p = |p*s2|
!                |s3|    |p*s3|
!
!         |s1|                |s1u1 s1u2 s1u3|
!   s¨’u =|s2|(u1,u2,u3)=siuj=|s2u1 s2u2 s2u3|
!         |s3|                |s3u1 s3u2 s3u3|
!
!         |n1|                |n1n1 n1n2 n1n3|
!   n¨’n =|n2|(n1,n2,n3)=ninj=|n2n1 n2n2 n2n3|
!         |n3|                |n3n1 n3n2 n3n3|
!
!         |n1|                          |n1*r11,n1*r12,n1*r13,n1*r22,n1*r23,n1*r33|
!   n¨’r =|n2|(r11,r12,r13,r22,r23,r33)=|n2*r11,n2*r12,n2*r13,n2*r22,n2*r23,n2*r33|
!         |n3|                          |n3*r11,n3*r12,n3*r13,n3*r22,n3*r23,n3*r33|

!============================================================================
!log:
!  2012-4-14 inverse of symtens and tensor bug fixed. 
!            ( zero divided need attention)
!============================================================================
use precision,only:dp,small,vsmall,ndim,pi
implicit none
private
save

! -- constants    -----------------------------------------------------------
			                                      
real(dp),parameter,public :: tens_i(9)   = [1.0d0, 0.0d0, 0.0d0,&
                                            0.0d0, 1.0d0, 0.0d0,&
 	 							            0.0d0, 0.0d0, 1.0d0]
real(dp),parameter,public :: tens_zero(9)= [0.0d0, 0.0d0, 0.0d0,&
                                            0.0d0, 0.0d0, 0.0d0,&
 	 							            0.0d0, 0.0d0, 0.0d0]
real(dp),parameter,public :: symt_i(6)   = [1.0d0, 0.0d0, 0.0d0,&
                                            1.0d0, 0.0d0, 1.0d0]
real(dp),parameter,public :: symt_zero(6)= [0.0d0, 0.0d0, 0.0d0,&
                                            0.0d0, 0.0d0, 0.0d0]

public :: vdot                      ! r=v1.v2

public :: tdot                      ! v=v1.st v=v1.t t=t1.t2 t=t1.st1 t=st.t1 st=st1.st2 v=st.v1 v=t1.v1
!
public :: ddotprod                  ! r=t1:t2 r=st1:t2 r=t1:st2 r=st1:st2

public :: crossprod                 ! v = crossprod(v1,v2)
!
public :: outerprod                 ! t = outerprod(v1,v2) 
!
public :: transform                 ! v = transform(tt,v1) 
                                    !st = transform(tt,st1)
                                    ! t = transform(tt,t1)
public :: vcos                      ! angle = vcos(v1,v2)
!
public :: transpose                 ! t = transpose(t1)
!
public :: magsqr                    ! r = magsqr(v)   r=magsqr(st)   r=magsqr(t)         
!
public :: vsqr                      ! st= sqr(v)
!
public :: vrot                      ! t = vrot(v1,v2)
!
public :: rodrigues                 ! t = rodrigues(v,angle,indegree)
!
public :: det                       ! r = det(t)  r = det(st)
!
public :: diag                      ! v = diag(t) v = diag(st)
!
public :: tr                        ! r = tr(t)   r = tr(st)
!
public :: dev                       ! t = dev(t1) st = dev(st1)
!
public :: dev2                      ! t = dev2(t1) st = dev2(st1)
!
public :: symm                      ! st= symm(t1)
!
!public :: twosymm                  ! st= twosymm(t1)
!
public :: skew                      ! t = skew(t1)
!
public :: mag                       ! r = mag(v)  r = mag(st)  r=mag(t)
!
public :: norm                      ! v = norm(v1)
!
!!!!puliic :: max                   ! v = max(v1,v2)  st = max(st1,st2)  t = max(t1,t2)
!
!!!!puliic :: min                   ! v = min(v1,v2)  st = min(st1,st2)  t = min(t1,t2)
!
!!!!public :: cmptmag               ! v = abs(v1)
!
!!!!public :: cmptsum               ! r = sum(v)
!
!!!!public :: cmptmax               ! r = maxval(v)
!
!!!!public :: cmptmin               ! r = minval(v)
!
!!!!public :: summag                ! r = sum(mag(v))   r = sum(mag(st))  r = sum(mag(t))
!
!!!public :: sum                    ! v  = sum(v(:,:),dim=2)
                                    ! st = sum(st(:,:),dim=2)  
                                    ! t  = sum(t(:,:),dim=2)  
! private :: inverse                ! st = inverse(st1) t=inverse(t1)
 public  :: inv                     ! st=  inv(st)    t=inv(t)

! public :: vprint
 public  :: validcmpt

contains

!>This function gives dot product of two vectors v1.v2
!>@param[in]   v1  -- vector 1
!>@param[in]   v2  -- vector 2
!<
!############################################################################
function vdot(v1, v2) result(r)
!############################################################################
! function : vec3d dot product 
! ai*bi gives products of two vectors
! vdot([a1,b1,c1],[b1,b2,b3]) = a1*b1+a2*b2+a3*b3
!============================================================================
!============================================================================
implicit none
real(dp), intent(in) :: v1(3),v2(3)
real(dp)             :: r

r = v1(1)*v2(1) +v1(2)*v2(2)+v1(3)*v2(3)
endfunction vdot

!>This function gives component number of dotproduct between tensor,
!!symmetrical tensor and vector 
!>@param[in]   m  -- component number of vector,symTens or tensor
!>@param[in]   n  -- component number of vector,symTens or tensor
!<
!############################################################################
pure function sizedot(m,n) result(r)
!############################################################################
integer,intent(in) :: m,n
integer            :: r
!-------------------------------------------------------------------------------!
!   vec   . tensor =3    vec . symtens =3      vec . general tensor(3*n) =n     !
!         | ¡õ¡õ¡õ         | ¡õ¡õ¡õ            | ¡õ¡õ¡õ...¡õ¡õ¡õ                !
!   ¡õ¡õ¡õ| ¡õ¡õ¡õ   ¡õ¡õ¡õ|   ¡õ¡õ      ¡õ¡õ¡õ| ¡õ¡õ¡õ...¡õ¡õ¡õ                !
!         | ¡õ¡õ¡õ         |     ¡õ            | ¡õ¡õ¡õ...¡õ¡õ¡õ                !
!-------------------------------------------------------------------------------!
!  tensor.vec=3   symtens.vec=3   gen tensor(n*3). vec=n     vec.vec            !
!  ¡õ¡õ¡õ|¡õ    ¡õ¡õ¡õ| ¡õ          ¡õ¡õ¡õ|            | ¡õ                     !  
!  ¡õ¡õ¡õ|¡õ      ¡õ¡õ| ¡õ          ¡õ¡õ¡õ| ¡õ   ¡õ¡õ¡õ| ¡õ                     !
!  ¡õ¡õ¡õ|¡õ        ¡õ| ¡õ          ¡õ¡õ¡õ| ¡õ         | ¡õ                     !  
!                                  . . .  | ¡õ                                  !
!                                   ¡õ¡õ¡õ|                                     !
!-------------------------------------------------------------------------------!
!  symtens.symtens=6  symtens. tensor=9    tensor.symtens=9  tensor. tensor=9   !
!  ¡õ¡õ¡õ|¡õ¡õ¡õ      ¡õ¡õ¡õ| ¡õ¡õ¡õ    ¡õ¡õ¡õ| ¡õ¡õ¡õ   ¡õ¡õ¡õ| ¡õ¡õ¡õ         !
!    ¡õ¡õ|  ¡õ¡õ        ¡õ¡õ| ¡õ¡õ¡õ    ¡õ¡õ¡õ|   ¡õ¡õ   ¡õ¡õ¡õ| ¡õ¡õ¡õ         ! 
!      ¡õ|    ¡õ          ¡õ| ¡õ¡õ¡õ    ¡õ¡õ¡õ|     ¡õ   ¡õ¡õ¡õ| ¡õ¡õ¡õ         !
!-------------------------------------------------------------------------------!
if(m==3) then
  if(n==6) then
    r=3
  else
    r=n/3
  endif
elseif(n==3) then
  if(m==6) then
    r=3
  else  
    r=m/3
  endif  
elseif(m==6) then
  if(n==6) then
    r=6
  else
    r=9
  endif      
elseif(m==9) then
  if(n==6.or.n==9) then
    r=9
  endif
else
  r=0
endif  
end function sizedot

!>This function gives dotproduct A.B between rank n tensor A and rank m tensor B
!>@param[in]   A    -- tensor A
!>@param[in]   B    -- tensor B
!<
!############################################################################
function tdot(a,b) result(ret)
!############################################################################
! applying dot to a rank n tensor and a rank m tensor gives a rank n+m-2 tensor. 
! at this time only rank 1 to 2 tensor is supported
!============================================================================
implicit none
real(dp),intent(in) :: a(:),b(:)
real(dp)            :: ret(sizedot(size(a),size(b)))

integer  :: m,n,im,nm
m= size(a) ; n = size(b)

!============================================================================
! (inner product between a vec3d and a tensor)                       !
! example1: direction vector . gradient of vector                    !  
!  d.¨Œu = d.[¨Œu1 ¨Œu2 ¨Œu3]  =[d.¨Œu1 d.¨Œu2 d.¨Œu3]               !
!                     |du1/dx1,du2/dx1,du3/dx1|                      !
!        = [d1 d2 d3].|du1/dx2,du2/dx2,du3/dx2|                      !
!                     |du1/dx3,du2/dx3,du3/dx3|                      ! 
! example2: surface normal gradient of vector=surface normal vector . gradient of vector !
!  n.¨Œu = n.[¨Œu1 ¨Œu2 ¨Œu3]  =[n.¨Œu1 n.¨Œu2 n.¨Œu3] =[sng(u1) sng(u2) sng(u3)]!
!                    |du1/dx1  du2/dx1 du3/dx1|                                  ! 
!        =[n1 n2 n3].|du1/dx2  du2/dx2 du3/dx2|                                  !
!                    |du1/dx3  du2/dx3 du3/dx3|                                  !
! example3: surface normal vector * gradient of vector                           ! 
!  s.¨Œu = s.[¨Œu1,¨Œu2,¨Œu3]  =[s.¨Œu1 s.¨Œu2 s.¨Œu3]                           !
!                    |du1/dx1  du2/dx1 du3/dx1|                                  !
!        =[s1 s2 s3].|du1/dx2  du2/dx2 du3/dx2|                                  ! 
!                    |du1/dx3  du2/dx3 du3/dx3|                                  !
! example4: surface normal vector * gradient of symtens tensor                   !
!  s.¨Œr  =[s.¨Œr11 s.¨Œr12 s.¨Œr13 s.¨Œr22 s.¨Œr23 s.¨Œr33]                     !
!                    |dr11/dx1  dr12/dx1 dr13/dx1 dr22/dx1 dr23/dx1 dr33/dx1|    !
!        =[s1,s2,s3].|dr11/dx2  dr12/dx2 dr13/dx2 dr22/dx2 dr23/dx2 dr33/dx2|    !
!                    |dr11/dx3  dr12/dx3 dr13/dx3 dr22/dx3 dr23/dx3 dr33/dx3|    ! 
! example5: surface normal vector * gradient of tensor                           !
!  s.¨Œt                                                                         !
!============================================================================    ! 

if(m==3.and.n==9) then  ! inner product between vector and tensor ai bij
  ret(1)=  a(1)*b(1) + a(2)*b(4) + a(3)*b(7)
  ret(2)=  a(1)*b(2) + a(2)*b(5) + a(3)*b(8)
  ret(3)=  a(1)*b(3) + a(2)*b(6) + a(3)*b(9)
elseif(m==3.and.n==6) then     ! inner product between a vec3d and a symmetry tensor 
  ret(1)=  a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
  ret(2)=  a(1)*b(2) + a(2)*b(4) + a(3)*b(5)
  ret(3)=  a(1)*b(3) + a(2)*b(5) + a(3)*b(6)
elseif(m==3.and.n==3) then     ! inner product between a vec3d and a vec3d 
  ret(1)=  a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
elseif(m==3.and.n>9) then     ! inner product between a vec3d and general tensor ai bij
  nm = n/3
  do im=1,nm
    ret(im)=  a(1)*b(im) + a(2)*b(im+nm) + a(3)*b(im+2*nm)
  end do
elseif(m==9.and.n==3) then    ! inner product between a tensor and a vec3d    tij*aj
  ret(1)= a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
  ret(2)= a(4)*b(1) + a(5)*b(2) + a(6)*b(3)
  ret(3)= a(7)*b(1) + a(8)*b(2) + a(9)*b(3)
elseif(m==6.and.n==3) then     !inner product between a symmetry tensor and a vec3d
  ret(1)= a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
  ret(2)= a(2)*b(1) + a(4)*b(2) + a(5)*b(3)
  ret(3)= a(3)*b(1) + a(5)*b(2) + a(6)*b(3)
elseif(m>9.and.n==3) then      !inner product between general tensor and a vec3d
  nm = m/3
  do im=1,nm
    ret(im)=  a(3*im-2)*b(1) + a(3*im-1)*b(2) + a(3*im)*b(3)
  end do
elseif(m==6.and.n==6) then   !//- inner-product between two symmetry tensors sik tjk
  ret=(/a(1)*b(1) + a(2)*b(2) + a(3)*b(3),&
        a(1)*b(2) + a(2)*b(4) + a(3)*b(5),&
        a(1)*b(3) + a(2)*b(5) + a(3)*b(6),&
        a(2)*b(2) + a(4)*b(4) + a(5)*b(5),&
        a(2)*b(3) + a(4)*b(5) + a(5)*b(6),&
        a(3)*b(3) + a(5)*b(5) + a(6)*b(6) /) 
elseif(m==6.and.n==9) then   !inner-product between symtens and tensor
  ret=(/ a(1)*b(1) + a(2)*b(4) + a(3)*b(7),&
         a(1)*b(2) + a(2)*b(5) + a(3)*b(8),&
         a(1)*b(3) + a(2)*b(6) + a(3)*b(9),&

         a(2)*b(1) + a(4)*b(4) + a(5)*b(7),&
         a(2)*b(2) + a(4)*b(5) + a(5)*b(8),&
         a(2)*b(3) + a(4)*b(6) + a(5)*b(9),&

         a(3)*b(1) + a(5)*b(4) + a(6)*b(7),&
         a(3)*b(2) + a(5)*b(5) + a(6)*b(8),&
         a(3)*b(3) + a(5)*b(6) + a(6)*b(9) /)
elseif(m==9.and.n==9) then    ! inner-product between two tensors  
  ret=(/ a(1)*b(1) + a(2)*b(4) + a(3)*b(7),&
         a(1)*b(2) + a(2)*b(5) + a(3)*b(8),&
         a(1)*b(3) + a(2)*b(6) + a(3)*b(9),&

         a(4)*b(1) + a(5)*b(4) + a(6)*b(7),&
         a(4)*b(2) + a(5)*b(5) + a(6)*b(8),&
         a(4)*b(3) + a(5)*b(6) + a(6)*b(9),&

         a(7)*b(1) + a(8)*b(4) + a(9)*b(7),&
         a(7)*b(2) + a(8)*b(5) + a(9)*b(8),&
         a(7)*b(3) + a(8)*b(6) + a(9)*b(9) /)   
elseif(m==9.and.n==6) then    ! inner-product between  tensor and symtens
  ret=(/ a(1)*b(1) + a(2)*b(2) + a(3)*b(3),&
         a(1)*b(2) + a(2)*b(4) + a(3)*b(5),&
         a(1)*b(3) + a(2)*b(5) + a(3)*b(6),&

         a(4)*b(1) + a(5)*b(2) + a(6)*b(3),&
         a(4)*b(2) + a(5)*b(4) + a(6)*b(5),&
         a(4)*b(3) + a(5)*b(5) + a(6)*b(6),&

         a(7)*b(1) + a(8)*b(2) + a(9)*b(3),&
         a(7)*b(2) + a(8)*b(4) + a(9)*b(5),&
         a(7)*b(3) + a(8)*b(5) + a(9)*b(6) /)
endif              
end function tdot

!>This function gives double dotproduct between tensor t1 and t2
!>@param[in]   t1   -- tensor t1
!>@param[in]   t2   -- tensor t2
!<
!############################################################################
function ddotprod(t1,t2) result(t12)
!############################################################################
! double inner product of two tensors
!============================================================================
real(dp), intent(in) :: t1(:),t2(:)
real(dp)             :: t12

integer :: nt1,nt2
nt1=size(t1);  nt2=size(t2)

if(nt1==9 .and. nt2==9) then
! double inner product of two tensors
  t12=t1(1)*t2(1)+t1(2)*t2(4)+t1(3)*t2(7)+&
      t1(4)*t2(2)+t1(5)*t2(5)+t1(6)*t2(8)+&
	  t1(7)*t2(3)+t1(8)*t2(6)+t1(9)*t2(9)
elseif(nt1==9 .and. nt2==6) then
! double inner product of tensor and symtens
  t12=t1(1)*t2(1) + t1(2)*t2(2) + t1(3)*t2(3) +&
      t1(4)*t2(2) + t1(5)*t2(4) + t1(6)*t2(5) +&
      t1(7)*t2(3) + t1(8)*t2(5) + t1(9)*t2(6)
elseif(nt1==6 .and. nt2==6) then
! double inner product of two symmetry tensors
  t12=t1(1)*t2(1)+2.*t1(2)*t2(2)+2.*t1(3)*t2(3)+&
                            t1(4)*t2(4)+2.*t1(5)*t2(5)+&
	                                                t1(6)*t2(6)
elseif(nt1==6 .and. nt2==9) then
!- double-dot-product between a symtens tensor and a tensor
  t12=t1(1)*t2(1) + t1(2)*t2(2) + t1(3)*t2(3) +&
      t1(2)*t2(4) + t1(4)*t2(5) + t1(5)*t2(6) +&
      t1(3)*t2(7) + t1(5)*t2(8) + t1(6)*t2(9)
else
  t12=0.0
endif
end function ddotprod

!>This function gives cross product between vector v1 and v2
!>@param[in]   v1  -- vector v1
!>@param[in]   v2  -- vector v2
!<
!############################################################################
function crossprod(v1, v2) result(tr)
!############################################################################
! function : gives the vector cross product of a and b
! if a and b are lists of length 3, corresponding to vectors in three dimensions,
! then crossprod(a, b) is also a list of length 3 
! cross is antisymmetric, so that crossprod(b, a) is -crossprod(a, b)
! a = [1, 2, 3]
! b = [3, 4, 2]
! w = crossprod(a,b) = [-8, 7, -2]
! the cross product of two vectors in three dimensions is perpendicular to 
! the two vectors
! vdot(a,w)= 0 vdot(b,w)=0
!============================================================================
implicit none
real(dp), intent(in):: v1(3),v2(3)
real(dp)            :: tr(3)

tr(1) = v1(2)*v2(3) - v1(3)*v2(2)
tr(2) = v1(3)*v2(1) - v1(1)*v2(3)
tr(3) = v1(1)*v2(2) - v1(2)*v2(1)
endfunction crossprod

!>This function gives outer product between vector v1 and tensor t2
!>@param[in]   v1  -- vector v1
!>@param[in]   v2  -- tensor v2
!<
!############################################################################
function outerprod(v1, v2) result(tr)
!############################################################################
!//- outer-product between two vec3ds
!   ai * bj
! example1
! surface normal vector * vector
!  |s1|            | s1u1 s1u2 s1u3 |
!  |s2|(u1 u2 u3) =| s2u1 s2u2 s2u3 | =|s*u1 s*u2 s*u3|
!  |s3|            | s3u1 s3u2 s3u3 |
!
! surface normal vector * tensor or symtens
!
!  |s1|                          | s1u1 s1u2 s1u3 ... s1u9 |
!  |s2|(u1 u2 u3 u4 u5 u6...u9) =| s2u1 s2u2 s2u3 ... s2u9 | =|s*u1 s*u2 s*u3 ... su9|
!  |s3|                          | s3u1 s3u2 s3u3 ... s3u9 |
!============================================================================
implicit none
real(dp), intent(in):: v1(3),v2(:)
real(dp)            :: tr(3*size(v2))
integer :: im,nm

nm=size(v2)

do im=1,nm
   tr(im)     = v1(1)*v2(im) 
   tr(im+nm)  = v1(2)*v2(im)
   tr(im+2*nm)= v1(3)*v2(im)
end do   
endfunction outerprod

!>This function gives cosine of separation angle between two cartesian 3-vectors 
!!v1 and v2
!>@param[in]   v1  -- vector v1
!>@param[in]   v2  -- vector v2
!<
!############################################################################
function vcos(v1, v2) result(ret)
!############################################################################
! the separation angle between two cartesian 3-vectors v1 and v2,
! return cosine of the separation angle
!============================================================================
real(dp), intent(in):: v1(3),v2(3)
real(dp)            :: ret
ret = vdot(v1,v2)/(mag(v1)*mag(v2))
end function

!>This function gives inverse of a tensor( or symtens) 
!>@param[in]   t  -- symmetry tensor or 3*3 tensor
!<
!############################################################################
function inverse(t) result(tr)
!############################################################################
!- return the inverse of a tensor( or symtens) or inverse jacobi matrix(3x3)
!============================================================================
implicit none
real(dp), intent(in) :: t(:)
real(dp)             :: tr(size(t))

select case(size(t))
  case(6)  ! symtens
    tr = (/t(4)*t(6) - t(5)*t(5),&
           t(3)*t(5) - t(2)*t(6),&
           t(2)*t(5) - t(3)*t(4),&

           t(1)*t(6) - t(3)*t(3),&
           t(2)*t(3) - t(1)*t(5),&

           t(1)*t(4) - t(2)*t(2)/)/det(t)
  case(9)  ! tensor
    tr = (/t(5)*t(9) - t(8)*t(6),&
           t(3)*t(8) - t(2)*t(9),&
           t(2)*t(6) - t(3)*t(5),&
           t(7)*t(6) - t(4)*t(9),&
           t(1)*t(9) - t(3)*t(7),&
           t(4)*t(3) - t(1)*t(6),&
           t(4)*t(8) - t(5)*t(7),&
           t(2)*t(7) - t(1)*t(8),&
           t(1)*t(5) - t(4)*t(2)/)/det(t)
end select
end function inverse

!>This function transform a vector,symmetry tensor or rank 2 tensor 
!!according to tranformation tensor 
!>@param[in]   tt  -- transformation tensor
!>@param[in]   v   -- a vector,symmetry tensor or rank 2 tensor
!<
!############################################################################
function transform(tt,v) result(tr)
!############################################################################
! transformatin of vector, symtens and tensor
! for vector : tt v
! for tensor :: tt v (tt)t
!============================================================================
implicit none
real(dp),intent(in) :: tt(9)
real(dp), intent(in):: v(:)
real(dp)            :: tr(size(v))

select case(size(v))
  case(3)
    tr(1)= tt(1)*v(1) + tt(2)*v(2) + tt(3)*v(3)
    tr(2)= tt(4)*v(1) + tt(5)*v(2) + tt(6)*v(3)
    tr(3)= tt(7)*v(1) + tt(8)*v(2) + tt(9)*v(3)
  case(6)
    tr =(/&
        (tt(1)*v(1) + tt(2)*v(2) + tt(3)*v(3))*tt(1)&
      + (tt(1)*v(2) + tt(2)*v(4) + tt(3)*v(5))*tt(2)&
      + (tt(1)*v(3) + tt(2)*v(5) + tt(3)*v(6))*tt(3),&

        (tt(1)*v(1) + tt(2)*v(2) + tt(3)*v(3))*tt(4)&
      + (tt(1)*v(2) + tt(2)*v(4) + tt(3)*v(5))*tt(5)&
      + (tt(1)*v(3) + tt(2)*v(5) + tt(3)*v(6))*tt(6),&

        (tt(1)*v(1) + tt(2)*v(2) + tt(3)*v(3))*tt(7)&
      + (tt(1)*v(2) + tt(2)*v(4) + tt(3)*v(5))*tt(8)&
      + (tt(1)*v(3) + tt(2)*v(5) + tt(3)*v(6))*tt(9),&

        (tt(4)*v(1) + tt(5)*v(2) + tt(6)*v(3))*tt(4)&
      + (tt(4)*v(2) + tt(5)*v(4) + tt(6)*v(5))*tt(5)&
      + (tt(4)*v(3) + tt(5)*v(5) + tt(6)*v(6))*tt(6),&

        (tt(4)*v(1) + tt(5)*v(2) + tt(6)*v(3))*tt(7)&
      + (tt(4)*v(2) + tt(5)*v(4) + tt(6)*v(5))*tt(8)&
      + (tt(4)*v(3) + tt(5)*v(5) + tt(6)*v(6))*tt(9),&
        
        (tt(7)*v(1) + tt(8)*v(2) + tt(9)*v(3))*tt(7)&
      + (tt(7)*v(2) + tt(8)*v(4) + tt(9)*v(5))*tt(8)&
      + (tt(7)*v(3) + tt(8)*v(5) + tt(9)*v(6))*tt(9)/)
  case(9)
    tr =(/&
        (tt(1)*v(1) + tt(2)*v(4) + tt(3)*v(7))*tt(1)&
      + (tt(1)*v(2) + tt(2)*v(5) + tt(3)*v(8))*tt(2)&
      + (tt(1)*v(3) + tt(2)*v(6) + tt(3)*v(9))*tt(3),&
        (tt(1)*v(1) + tt(2)*v(4) + tt(3)*v(7))*tt(4)&
      + (tt(1)*v(2) + tt(2)*v(5) + tt(3)*v(8))*tt(5)&
      + (tt(1)*v(3) + tt(2)*v(6) + tt(3)*v(9))*tt(6),&
        (tt(1)*v(1) + tt(2)*v(4) + tt(3)*v(7))*tt(7)&
      + (tt(1)*v(2) + tt(2)*v(5) + tt(3)*v(8))*tt(8)&
      + (tt(1)*v(3) + tt(2)*v(6) + tt(3)*v(9))*tt(9),&
        (tt(4)*v(1) + tt(5)*v(4) + tt(6)*v(7))*tt(1)&
      + (tt(4)*v(2) + tt(5)*v(5) + tt(6)*v(8))*tt(2)&
      + (tt(4)*v(3) + tt(5)*v(6) + tt(6)*v(9))*tt(3),&
        (tt(4)*v(1) + tt(5)*v(4) + tt(6)*v(7))*tt(4)&
      + (tt(4)*v(2) + tt(5)*v(5) + tt(6)*v(8))*tt(5)&
      + (tt(4)*v(3) + tt(5)*v(6) + tt(6)*v(9))*tt(6),&
        (tt(4)*v(1) + tt(5)*v(4) + tt(6)*v(7))*tt(7)&
      + (tt(4)*v(2) + tt(5)*v(5) + tt(6)*v(8))*tt(8)&
      + (tt(4)*v(3) + tt(5)*v(6) + tt(6)*v(9))*tt(9),&
        (tt(7)*v(1) + tt(8)*v(4) + tt(9)*v(7))*tt(1)&
      + (tt(7)*v(2) + tt(8)*v(5) + tt(9)*v(8))*tt(2)&
      + (tt(7)*v(3) + tt(8)*v(6) + tt(9)*v(9))*tt(3),&
        (tt(7)*v(1) + tt(8)*v(4) + tt(9)*v(7))*tt(4)&
      + (tt(7)*v(2) + tt(8)*v(5) + tt(9)*v(8))*tt(5)&
      + (tt(7)*v(3) + tt(8)*v(6) + tt(9)*v(9))*tt(6),&
        (tt(7)*v(1) + tt(8)*v(4) + tt(9)*v(7))*tt(7)&
      + (tt(7)*v(2) + tt(8)*v(5) + tt(9)*v(8))*tt(8)&
      + (tt(7)*v(3) + tt(8)*v(6) + tt(9)*v(9))*tt(9)/)
end select      
end function transform

!>This function transpose a rank 2 tensor 
!>@param[in]   t  -- rank 2 tensor
!<
!############################################################################
function transpose(t) result(tr)
!############################################################################
! function :  tensor transpose
!============================================================================
implicit none
real(dp), intent(in) :: t(9)
real(dp)             :: tr(9)

tr=(/t(1),t(4),t(7),&
     t(2),t(5),t(8),&
     t(3),t(6),t(9)/)
endfunction transpose

!>This function gives square of a vector v^2 
!>@param[in]   v1 -- vector v1
!<
!############################################################################
function vsqr(v1) result(tr)
!############################################################################
!//- outer-product between two vec3ds (vec3d sqr)
!============================================================================
implicit none
real(dp), intent(in)    :: v1(3)
real(dp)                :: tr(6)

tr=(/ v1(1)*v1(1), v1(1)*v1(2), v1(1)*v1(3),&
                    v1(2)*v1(2), v1(2)*v1(3),&
                                  v1(3)*v1(3)  /)
endfunction vsqr

!>This function gives |v| or |T|,magnitude of vector,symmetry tensor or rank 2 tensor 
!>@param[in]   v   -- vector,symmetry tensor or rank 2 tensor 
!<
!############################################################################
real(dp) function mag(v)
!############################################################################
! function : magnitude of vector,symtens and tensor
!============================================================================
implicit none
real(dp), intent(in) :: v(:)

select case(size(v))
  case(1)  ! scalar mag
     mag = abs(v(1))
  case(3)  ! vec3d mag
     mag = sqrt(v(1)*v(1) + v(2)*v(2) + v(3)*v(3))
  case(6)  ! symtens mag
     mag = sqrt(v(1)*v(1) + 2.0d0*v(2)*v(2) + 2.0d0*v(3)*v(3)&
                          + v(4)*v(4)&
						  + 2.0d0*v(5)*v(5) + v(6)*v(6))
  case(9)  ! tensor
     mag = sqrt(v(1)*v(1) + v(2)*v(2) + v(3)*v(3)&
              + v(4)*v(4) + v(5)*v(5) + v(6)*v(6)&
              + v(7)*v(7) + v(8)*v(8) + v(9)*v(9))
  case default
     mag = 0.0
end select						  
endfunction mag

!>This function gives |v|^2 or |T|^2, magnitude square of vector,symmetry tensor 
!!or rank 2 tensor 
!>@param[in]   v   -- vector,symmetry tensor or rank 2 tensor 
!<
!############################################################################
real(dp) function magsqr(v)
!############################################################################
! function : square of norm magnitude
!============================================================================
implicit none
real(dp), intent(in) :: v(:)

select case(size(v))
  case(3)  ! vec3d
    magsqr = v(1)*v(1) + v(2)*v(2) + v(3)*v(3)
  case(6)  ! symtens
    magsqr = v(1)*v(1) + 2.0d0*v(2)*v(2) + 2.0d0*v(3)*v(3)&
                             + v(4)*v(4)   + 2.0d0*v(5)*v(5)&
	    				                          + v(6)*v(6)
  case(9)  ! tensor
    magsqr = v(1)*v(1) + v(2)*v(2) + v(3)*v(3)&
           + v(4)*v(4) + v(5)*v(5) + v(6)*v(6)&
           + v(7)*v(7) + v(8)*v(8) + v(9)*v(9)
  case default
    magsqr = 0.0
end select	    				                       
endfunction magsqr

!>This function gives normalization of vector
!>@param[in]   v   -- vector
!<
!############################################################################
function norm(v) result(tr)
!############################################################################
! function : vec3d normalization
!============================================================================
implicit none
real(dp), intent(in) :: v(3)
real(dp)             :: tr(3)

tr = v/(sqrt(v(1)*v(1) + v(2)*v(2) + v(3)*v(3))+vsmall)
endfunction norm

!>This function gives symmetric part of a tensor
!>@param[in]   t   -- tensor
!<
!############################################################################
function symm(t) result(tr)
!############################################################################
! symmetric part of a tensor
!============================================================================
implicit none
real(dp), intent(in) :: t(9)
real(dp)             :: tr(6)
tr=(/t(1), 0.5d0*(t(2) + t(4)), 0.5d0*(t(3) + t(7)),&
           t(5),                0.5d0*(t(6) + t(8)),&
                                                  t(9)/)

endfunction symm

!>This function gives skew part of a tensor
!>@param[in]   t   -- tensor
!<
!############################################################################
function skew(t) result(tr)
!############################################################################
! skew part of a tensor
!============================================================================
implicit none
real(dp), intent(in) :: t(9)
real(dp)             :: tr(9)

tr   = (/0.0d0, 0.5d0*(t(2) - t(4)), 0.5d0*(t(3) - t(7)),&
         0.5d0*(t(4) - t(2)), 0.0d0, 0.5d0*(t(6) - t(8)),&
         0.5d0*(t(7) - t(3)), 0.5d0*(t(8) - t(6)), 0.0d0/)

endfunction skew

!>This function gives trace of a tensor and symtens 
!>@param[in]   t   -- tensor
!<
!############################################################################
real(dp) function tr(t)
!############################################################################
! function :  trace of a tensor and symtens 
!============================================================================
implicit none
real(dp), intent(in) :: t(:)

select case(size(t))
  case(6)
    tr=t(1)+t(4)+t(6)  ! symmetry tensor
  case(9)
    tr=t(1)+t(5)+t(9)  ! tensor
  case default
    tr=0.0
end select    
endfunction tr

!>This function gives deviatoric part of a tensor and symtens
!>@param[in]   t   -- tensor or symtens
!<
!############################################################################
function dev(t) result(ret)
!############################################################################
! function :  deviatoric part of a tensor and symtens
!============================================================================
implicit none
real(dp), intent(in) :: t(:)
real(dp)             :: ret(size(t))

select case(size(t))
  case(6)  ! symtens
    ret=t-((1.0d0/3.0d0)*tr(t))*symt_i
  case(9)  ! tensor
    ret=t-((1.0d0/3.0d0)*tr(t))*tens_i
end select
endfunction dev

!>This function gives deviatoric part of a tensor and symtens
!>@param[in]   t   -- tensor or symtens
!<
!############################################################################
function dev2(t) result(ret)
!############################################################################
! function :  deviatoric part of a tensor and symtens
!============================================================================
implicit none
real(dp), intent(in) :: t(:)
real(dp)             :: ret(size(t))

select case(size(t))
  case(6)  ! symtens
    ret=t-((2.0d0/3.0d0)*tr(t))*symt_i
  case(9)  ! tensor
    ret=t-((2.0d0/3.0d0)*tr(t))*tens_i
end select
endfunction dev2

!>This function gives diagonal of a tensor and symtens
!>@param[in]   t   -- tensor or symtens
!<
!############################################################################
function diag(t) result(tr)
!############################################################################
! function :  diagonal of a tensor and symtens
!============================================================================
implicit none
real(dp), intent(in) :: t(:)
real(dp)             :: tr(3)

select case(size(t))
  case(6)  ! symtens
    tr=(/t(1),t(4),t(6)/)
  case(9)  ! tensor
    tr=(/t(1),t(5),t(9)/)
end select
endfunction diag

!>This function gives determinant of a tensor and symtens
!>@param[in]   t   -- tensor or symtens
!<
!############################################################################
real(dp) function det(t)
!############################################################################
! return the determinant of a tensor and symtens
!============================================================================
implicit none
real(dp), intent(in) :: t(:)

select case(size(t))
  case(6)  ! symtens
    det = t(1)*t(4)*t(6) + t(2)*t(5)*t(3)&
         +t(3)*t(2)*t(5) - t(1)*t(5)*t(5)&
         -t(2)*t(2)*t(6) - t(3)*t(4)*t(3)
  case(9)  ! tensor
    det = t(1)*t(5)*t(9) + t(2)*t(6)*t(7)&
        + t(3)*t(4)*t(8) - t(1)*t(6)*t(8)&
        - t(2)*t(4)*t(9) - t(3)*t(5)*t(7)
  case default
     det = 0.0
end select  
endfunction det

!> This function gives transformation tensor which rotates from direcion 
!! n1 to direction n2
!>@param[in]   n1   -- starting direction vector 
!>@param[in]   n2   -- ending direction vector 
!<
!############################################################################
function vrot(n1,n2)
!############################################################################
! can be used to rotates the mesh and fields from the direcion 
! n1 to the direction n2.
!============================================================================
implicit none
real(dp),intent(in) :: n1(3),n2(3)
real(dp)            :: vrot(9)
real(dp)            :: v3(3)

v3   = crossprod(n1,n2)
vrot = vdot(n1,n2)*tens_i +(1.0d0-vdot(n1,n2))*outerprod(v3,v3)/(magsqr(v3)+vsmall)+&
       outerprod(n2,n1) -outerprod(n1,n2)
end function vrot

!> This function gives rotation tensor base on rodrigues' rotation formula
!>@param[in]   axis   -- rotation axis
!>@param[in]   angle  -- rotation angle
!>@param[in]   indeg  -- if angle in degree
!<
!############################################################################
function rodrigues(axis,angle,indeg) result(rottensor)
!############################################################################
! compute rotation tensor base on rodrigues' rotation formula
!============================================================================
implicit none
real(dp),intent(in) :: axis(3),angle
logical,optional    :: indeg
real(dp)            :: rottensor(9),un(3)

logical ::  deg
real(dp)::  theta,ss,cc,oneminuscc,magrotaxis 

deg = .true.
if(present(indeg)) deg=indeg

theta = angle
if(deg) theta=theta*pi/180.0

ss = sin(theta)
cc = cos(theta)
oneminuscc = 1.0 - cc

magrotaxis = mag(axis)

if(magrotaxis < small) then
  write(*,*) "ERROR : TENSOR RODRIGUESROTATION"
  stop
endif  

un = axis/magrotaxis

rottensor(1) = cc + un(1)**2.0*oneminuscc
rottensor(5) = cc + un(2)**2.0*oneminuscc
rottensor(9) = cc + un(3)**2.0*oneminuscc

rottensor(2) = un(1)*un(2)*oneminuscc - un(3)*ss
rottensor(3) = un(2)*ss + un(1)*un(3)*oneminuscc

rottensor(4) =  un(3)*ss + un(1)*un(2)*oneminuscc
rottensor(6) = -un(1)*ss + un(2)*un(3)*oneminuscc

rottensor(7) = -un(2)*ss + un(1)*un(3)*oneminuscc
rottensor(8) =  un(1)*ss + un(2)*un(3)*oneminuscc
end function rodrigues


!> This function inverses 3x3 matrix considering zero fillin
!>@param[in]   tf   -- 3x3 matrix
!<
!############################################################################
function inv(tf) result(tr)
!############################################################################
! inverse 3x3 matrix considering zero fillin
!============================================================================
implicit none
real(dp),intent(inout) :: tf(:)
real(dp)               :: tr(size(tf))

real(dp) :: tfplus(size(tf)),scale
integer  :: ndim 
logical  :: remove(3)

ndim = size(tf)
scale = tf(1)*tf(1)

if(ndim==6) then
   remove(1) = (tf(1)*tf(1)/scale) < small
   remove(2) = (tf(4)*tf(4)/scale) < small  
   remove(3) = (tf(6)*tf(6)/scale) < small
elseif(ndim==9) then
   remove(1) = (tf(1)*tf(1)/scale) < small
   remove(2) = (tf(5)*tf(5)/scale) < small 
   remove(3) = (tf(9)*tf(9)/scale) < small
else
    remove = .false.
endif

if(remove(1) .or. remove(2) .or. remove(3)) then
   tfplus       = tf

   if(ndim==6) then
      if(remove(1)) tfplus(1) = tfplus(1) +1.0d0
      if(remove(2)) tfplus(4) = tfplus(4) +1.0d0
      if(remove(3)) tfplus(6) = tfplus(6) +1.0d0
   elseif(ndim==9) then
      if(remove(1)) tfplus(1) = tfplus(1) +1.0d0
      if(remove(2)) tfplus(5) = tfplus(5) +1.0d0
      if(remove(3)) tfplus(9) = tfplus(9) +1.0d0
   endif
   
   tr = inverse(tfplus)

   if(ndim==6) then
      if(remove(1)) tr(1) = tr(1) -1.0d0
      if(remove(2)) tr(4) = tr(4) -1.0d0
      if(remove(3)) tr(6) = tr(6) -1.0d0
   elseif(ndim==9) then
      if(remove(1)) tr(1) = tr(1) -1.0d0
      if(remove(2)) tr(5) = tr(5) -1.0d0
      if(remove(3)) tr(9) = tr(9) -1.0d0
   endif    
else
   tr = inverse(tf)    
endif
end function inv

!> This function return the valid component tensor according to valid vector
!>@param[in]  ncmpt   -- varaible's component number
!>@param[in]  sold    -- valid vector
!<
!############################################################################
function validcmpt(ncmpt,sold)
!############################################################################
use precision,only:dp
implicit none
integer,intent(in)  :: ncmpt
real(dp),intent(in) :: sold(3)
real(dp)            :: validcmpt(ncmpt)

if(ncmpt==3) then
  validcmpt = sold
elseif(ncmpt==6) then
  validcmpt = vsqr(sold)
elseif(ncmpt==9) then    
  validcmpt = outerprod(sold,sold)
endif 
end function validcmpt 
end module vec3dlib



