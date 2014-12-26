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

!###########################################################################
subroutine pst_exp(imat,nm,vname,var)
!###########################################################################
!  bug : 2014-8-14     nHat =norm(sf(:,i))=>     nHat =norm(sf(:,faceI))
!===========================================================================
use precision,only:dp,delim
use scheme,only:caseDir
use comm0,only:ncell,ntvfac,ntface,nface,nbfac,nvrt,npatch,x,own,nei,face,face_p
implicit none
integer,intent(in)         :: imat
integer,intent(in)         :: nm
character(len=*),intent(in):: vname
real(dp),intent(in)        :: var(nm,ncell+nbfac)

integer   :: ifile,iff,n,ierr,istat,j
character :: cn*9='123456789'

call fopen(ifile,trim(caseDir)//delim//trim(vname)//".plt",'w',ierr)
!
!---number of vertices for each face(including boundary faces)
!---summary of number of vertices
!---set the faceNodes for tecplot 
!
write(ifile,'(a)') 'Title ="'//vname//'"'
write(ifile,'(a)') 'Variables="X"'
write(ifile,'(a)') '"Y"'
write(ifile,'(a)') '"Z"'
if(nm==1) then
  write(ifile,'(a)') '"'//trim(vname)//'"'
else
  do iff=1,nm
    write(ifile,'(a)') '"'//trim(vname)//cn(iff:iff)//'"'
  end do
endif

write(ifile,'(a)') ''

write(ifile,'(a)'   )   'Zone T="Zone1"'
write(ifile,'(a)'   )   'ZONETYPE=FEPOLYHEDRON'
write(ifile,'(a,i8)')   'NODES=',                       nvrt
write(ifile,'(a,i8)')   'FACES=',                       ntface
write(ifile,'(a,i8)')   'ELEMENTS=',                    ncell
write(ifile,'(a,i8)')   'TOTALNUMFACENODES  =',         ntvfac
write(ifile,'(a,i8)')   'NUMCONNECTEDBOUNDARYFACES  =', 0
write(ifile,'(a,i8)')   'TOTALNUMBOUNDARYCONNECTIONS=', 0

write(ifile,'(a)',advance='no')   'DT=(DOUBLE,DOUBLE,DOUBLE' 
do iff=1,nm
  write(ifile,'(a)',advance='no')  ',DOUBLE'
end do
write(ifile,'(a)') ')'
   
write(ifile,'(a)'   )   'DATAPACKING=BLOCK'
if(nm==1) then
  write(ifile,'(a)' )   'VARLOCATION=([1-3]=NODAL,[4]=CELLCENTERED)'
else
  write(ifile,'(a,i0,a)') 'VARLOCATION=([1-3]=NODAL,[4-',nm+3,']=CELLCENTERED)'
endif  
write(ifile,'(a)'   )   'STRANDID=1'
write(ifile,'(a)'   )   'SOLUTIONTIME=0'

write(ifile,'(10(es10.3,1x))')  (x(1,iff),iff=1,nvrt)
write(ifile,'(10(es10.3,1x))')  (x(2,iff),iff=1,nvrt)
write(ifile,'(10(es10.3,1x))')  (x(3,iff),iff=1,nvrt)
write(ifile,'(10(es10.3,1x))')  ((var(j,iff),iff=1,ncell),j=1,nm)

write(ifile,'(a)'   )   '# node count per face'
write(ifile,'(10i8)')  (face_p(iff+1)-face_p(iff),iff=1,ntface)
write(ifile,'(a)'   )   '# face nodes'
write(ifile,'(10i8)')  (face(iff),iff=1,ntvfac)
write(ifile,'(a)'   )   '# left cells'
write(ifile,'(10i8)')  (nei(iff),iff=1,nface),(own(iff),iff=nface+1,ntface)
write(ifile,'(a)'   )   '# right cells'
write(ifile,'(10i8)')  (own(iff),iff=1,nface),(nei(iff),iff=nface+1,ntface)
 
close(ifile)
end subroutine pst_exp

!###########################################################################
subroutine pst_msh
!###########################################################################
use precision,only:dp,delim
use scheme,only:caseDir
use comm0,only:ncell,ntface,nface,nbfac,nvrt,npatch,x,own,nei,face,face_p,ntvfac
implicit none

integer   :: ifile,iff,n,ierr,istat,j

call fopen(ifile,trim(caseDir)//delim//'mesh.plt','w',ierr)
!
!---number of vertices for each face(including boundary faces)
!---summary of number of vertices
!---set the faceNodes for tecplot 
!
write(ifile,'(a)') 'Title ="mesh"'
write(ifile,'(a)') 'Variables="X"'
write(ifile,'(a)') '"Y"'
write(ifile,'(a)') '"Z"'

write(ifile,'(a)') ''

write(ifile,'(a)'   )   'Zone T="Zone1"'
write(ifile,'(a)'   )   'ZONETYPE=FEPOLYHEDRON'
write(ifile,'(a,i8)')   'NODES=',                       nvrt
write(ifile,'(a,i8)')   'FACES=',                       ntface
write(ifile,'(a,i8)')   'ELEMENTS=',                    ncell
write(ifile,'(a,i8)')   'TOTALNUMFACENODES  =',         ntvfac
write(ifile,'(a,i8)')   'NUMCONNECTEDBOUNDARYFACES  =', 0
write(ifile,'(a,i8)')   'TOTALNUMBOUNDARYCONNECTIONS=', 0

write(ifile,'(a)',advance='no')   'DT=(DOUBLE,DOUBLE,DOUBLE)' 
   
write(ifile,'(a)'  )   'DATAPACKING=BLOCK'
write(ifile,'(a)'  )   'VARLOCATION=([1-3]=NODAL)'
write(ifile,'(a)'  )   'STRANDID=1'
write(ifile,'(a)'  )   'SOLUTIONTIME=0'

write(ifile,'(10(es10.3,1x))')  (x(1,iff),iff=1,nvrt)
write(ifile,'(10(es10.3,1x))')  (x(2,iff),iff=1,nvrt)
write(ifile,'(10(es10.3,1x))')  (x(3,iff),iff=1,nvrt)

write(ifile,'(a)'   )   '# node count per face'
write(ifile,'(10i8)')  (face_p(iff+1)-face_p(iff),iff=1,ntface)
write(ifile,'(a)'   )   '# face nodes'
write(ifile,'(10i8)')  (face(iff),iff=1,ntvfac)
write(ifile,'(a)'   )   '# left cells'
write(ifile,'(10i8)')  (nei(iff),iff=1,nface),(own(iff),iff=nface+1,ntface)
write(ifile,'(a)'   )   '# right cells'
write(ifile,'(10i8)')  (own(iff),iff=1,nface),(nei(iff),iff=nface+1,ntface)
 
close(ifile)
end subroutine pst_msh




!###########################################################################
subroutine pst_sfn(phi) 
!###########################################################################
!Description
!Calculates and writes the stream function of velocity field U at each time
!==========================================================================
use precision,only:dp,list,small,vsmall,delim
use comm0,only:nvrt,nface,ncell,nbfac,npatch,nsp,nep,ntface,&
               cpoint,cpoint_p,x,bityp,sf,own,nei,face_p,face,ntvfac
use scheme,only:bc_symmetry,bc_wedge,bc_empty,casedir
use vec3dlib,only:norm,mag
implicit none
real(dp),intent(in) :: phi(nface+nbfac)

real(dp),external :: sign_
integer,external :: whichPatch

real(dp) :: sfn(nvrt),curStr,curBStr,curStrPoint(3),curBStrPoint(3),edgeHat(3),nHat(3)
integer :: visitedPoint(nvrt),nVisited,nVisitedOld,i,nP,ic,ir,ib,patchNo,faceI
logical :: pointFound,bpointFound,finished,found
integer :: ifile,iff,n,ierr,istat,ist

sfn          = 0.0d0
visitedpoint = 0
nVisited     = 0
nVisitedOld  = 0

finished = .true.
!
!-- Find the boundary face with zero flux. set the stream function
!-- to zero on that face
!
loopF: do  
  found = .false.

  loopA3: do ir=1,npatch
	if(bityp(ir)==bc_empty) cycle
    
	loopA2: do ib=nsp(ir),nep(ir)
	   if((phi(nface+ib)*phi(nface+ib))>small) cycle
!
!      Zero flux face found
       found = .true.
          
	   ist = face_p(nface+ib)
	   nP  = face_p(nface+ib+1)-face_p(nface+ib)
		  
	   loopA1:do i=1,nP
	     if(visitedPoint(face(ist+i-1))==1) then
	  	   found=.false.
		    exit loopA1
		 end if
       end do loopA1

	   if(found) then
!	      write(stdout,'(2(a,i0))') "zero face : patch ",ir,", face ",ib
			 
		  ! set all the associated point to 0
		  do i=1,nP
		    sfn(face(ist+i-1))          = 0.0d0
			visitedPoint(face(ist+i-1)) = 1
			nvisited = nvisited +1
          enddo

		  exit loopA2
	   else
	   endif
    end do loopA2
    
	if(found) exit loopA3
  end do loopA3

  if(.not.found) then
!	write(stdout,'(a)')  "zero flux boundary face not found. "
!	write(stdout,'(a)')  "Using cell as a reference."

    loopB2:do ic=1,ncell
	  found=.true.

	  loopB1:do i=cpoint_p(ic),cpoint_p(ic+1)-1
		if(visitedPoint(cpoint(i)) == 1) then
          found = .false.
          
		  exit loopB1
        endif
      enddo loopB1

      if(found) then
		do i=cpoint_p(ic),cpoint_p(ic+1)-1
		  sfn(cpoint(i)) = 0.00d0
		  visitedPoint(cpoint(i)) = 1
		  nVisited=nVisited+1
        enddo
        exit loopB2
      else
		call disp('unable to find initialisation face or a cell.')
      endif
    end do loopB2
  endif

! Loop through all faces. If one of the points on
! the face has the streamfunction value different
! from -1, all points with -1 ont that face have the
! streamfunction value equal to the face flux in
!  that point plus the value in the visited point
  loopE:do 
    finished = .true.
    
	do faceI=nface+1,nface+nbfac
      ist = face_p(faceI)
      nP  = face_p(faceI+1)-face_p(faceI)

      bPointFound = .false.

      curBStr     = 0.0
      curBStrPoint= 0.0
		  
      loopC1:do i=1,nP
		                 
!     Check if the point has been visited
        if(visitedPoint(face(ist+i-1)) == 1) then
!            // The point has been visited
           curBStr      = sfn(face(ist+i-1))
           curBStrPoint = x(:,face(ist+i-1))

           bPointFound = .true.

		   exit loopC1
        endif
      end do loopC1

      if(bPointFound) then
!       // Sort out other points on the face
        do i=1,nP
!        // Check if the point has been visited
          if(visitedPoint(face(ist+i-1)) == 0) then
              patchNo =whichPatch(faceI) 

              if(bityp(PatchNo)/=bc_empty .and. bityp(PatchNo)/=bc_symmetry &
			     .and. bityp(PatchNo)/=bc_wedge) then

                 edgeHat    = x(:,face(ist+i-1))- curBStrPoint
                 edgeHat(3) = 0.0d0
                 edgeHat=norm(edgeHat)
				 
                 nHat = norm(sf(:,faceI))

                 if(edgeHat(2)>VSMALL) then
                   visitedPoint(face(ist+i-1)) = 1
                   nVisited=nVisited+1

                   sfn(face(ist+i-1))=curBStr+ phi(faceI)*sign_(nHat(1))
                 elseif(edgeHat(2)<-VSMALL) then
                   visitedPoint(face(ist+i-1)) = 1
                   nVisited=nVisited+1

                   sfn(face(ist+i-1))=curBStr- phi(faceI)*sign_(nHat(1))
                 else
                   if(edgeHat(1)> VSMALL) then
                     visitedPoint(face(ist+i-1)) = 1
                     nVisited=nVisited+1

                     sfn(face(ist+i-1))=curBStr+ phi(faceI)*sign_(nHat(2))
                   else if(edgeHat(1)< -VSMALL) then
                     visitedPoint(face(ist+i-1)) = 1
                     nVisited=nVisited+1

                     sfn(face(ist+i-1))=curBStr- phi(faceI)*sign_(nHat(2))
                   endif
                 endif
			   endif
          endif			   	  
        end do
      else
        finished = .false.
      endif

    end do

    do faceI=1,nface
! / Get the list of point labels for the face
       ist = face_p(faceI)
       nP  = face_p(faceI+1)-face_p(faceI)

       pointFound = .false.

       curStr = 0.0
       curStrPoint=0.0

	   loopD1:do i=1,nP   
!    / Check if the point has been visited
          if(visitedPoint(face(ist+i-1)) == 1) then
!    / The point has been visited
             curStr =sfn(face(ist+i-1))
             curStrPoint =x(:,face(ist+i-1))
             pointFound = .true.

             exit loopD1
          endif
       end do loopD1

       if(pointFound) then
!      // Sort out other points on the face
          do i=1,nP
!         // Check if the point has been visited
            if(visitedPoint(face(ist+i-1))== 0) then
              edgeHat =x(:,face(ist+i-1))- curStrPoint
              edgeHat(3)=0.0
              edgeHat=norm(edgeHat)
              nHat =norm(sf(:,faceI))

              if(edgeHat(2)> VSMALL) then
                 visitedPoint(face(ist+i-1)) = 1
                 nVisited=nVisited+1
                 sfn(face(ist+i-1)) = curStr+phi(faceI)*sign_(nHat(1))
              else if(edgeHat(2)< -VSMALL) then
                 visitedPoint(face(ist+i-1)) = 1
                 nVisited=nVisited+1
                 sfn(face(ist+i-1)) = curStr-phi(faceI)*sign_(nHat(1))
              endif
            endif
		  end do
	   else
          finished = .false.
       endif
    end do

!	write(stdout,'(a,i0)') "One pass, n visited ",nVisited

    if(nVisited == nVisitedOld) then
       call disp('Find new seed. This must be a multiply connected domain')
       call disp('Exhausted a seed. Looking for new seed ')
       call disp('this is correct for multiply connected domain')
       exit loopE
    else
       nVisitedOld = nVisited
    endif

	if(finished) exit loopE
  end do loopE
  
  if(finished) exit loopF
end do loopF 


call fopen(ifile,trim(caseDir)//delim//"sfn.plt",'w',ierr)

!
write(ifile,'(a)') 'Title ="stream"'
write(ifile,'(a)') 'Variables="X"'
write(ifile,'(a)') '"Y"'
write(ifile,'(a)') '"Z"'
write(ifile,'(a)') '"SFN"'

write(ifile,'(a)') ''

write(ifile,'(a)'   )   'Zone T="Zone1"'
write(ifile,'(a)'   )   'ZONETYPE=FEPOLYHEDRON'
write(ifile,'(a,i8)')   'NODES=',                       nvrt
write(ifile,'(a,i8)')   'FACES=',                       ntface
write(ifile,'(a,i8)')   'ELEMENTS=',                    ncell
write(ifile,'(a,i8)')   'TOTALNUMFACENODES  =',         ntvfac
write(ifile,'(a,i8)')   'NUMCONNECTEDBOUNDARYFACES  =', 0
write(ifile,'(a,i8)')   'TOTALNUMBOUNDARYCONNECTIONS=', 0

write(ifile,'(a)')   'DT=(DOUBLE,DOUBLE,DOUBLE,DOUBLE)' 
   
write(ifile,'(a)')   'DATAPACKING=BLOCK'
write(ifile,'(a)' )  'VARLOCATION=([1-4]=NODAL)'
write(ifile,'(a)'   )   'STRANDID=1'
write(ifile,'(a)'   )   'SOLUTIONTIME=0'

write(ifile,'(10(es10.3,1x))')  (x(1,iff),iff=1,nvrt)
write(ifile,'(10(es10.3,1x))')  (x(2,iff),iff=1,nvrt)
write(ifile,'(10(es10.3,1x))')  (x(3,iff),iff=1,nvrt)
write(ifile,'(10(es10.3,1x))')  (sfn(iff),iff=1,nvrt)

write(ifile,'(a)'   )   '# node count per face'
write(ifile,'(10i8)')  (face_p(iff+1)-face_p(iff),iff=1,ntface)
write(ifile,'(a)'   )   '# face nodes'
write(ifile,'(10i8)')  (face(iff),iff=1,ntvfac)
write(ifile,'(a)'   )   '# left cells'
write(ifile,'(10i8)')  (nei(iff),iff=1,nface),(own(iff),iff=nface+1,ntface)
write(ifile,'(a)'   )   '# right cells'
write(ifile,'(10i8)')  (own(iff),iff=1,nface),(nei(iff),iff=nface+1,ntface)

close(ifile)

end subroutine pst_sfn







