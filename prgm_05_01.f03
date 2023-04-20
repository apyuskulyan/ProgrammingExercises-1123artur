      program prgm_05_01
!
!Arthur

!     This program is written in atomic units.
!
!
!     Variable Declarations
      implicit none
      integer::i,j,k,NCmdLineArgs, nbasisfn,IError
      real::m,l,tMatrixElement,vMatrixElement,b
	  Real,Dimension(:),Allocatable::EVals,Temp_Vector, hlower
	  Real,Dimension(:,:),Allocatable::Matrix,EVecs,Temp_Matrix,PIB_1D_Modified_Hamiltonian
!      parameter (pi = pi)
      real,external::PIB_1D_T_Element
      real,external::PIB_1D_V_Element
      integer::n1,n2
      logical::fail
      character(len=1024)::cmd_buffer
	  real,parameter::pi=float(4)*atan(1.0)
!
!
!     Format Statements
!
 2000 format(1x,'Hamiltonian matrix element ',I5,',',I5,' is',F12.5, '.' ) 
 9000 format(1x,'Expected 5 command line arguments, but found ',i2,'.')
!
!
!     Read in m, l, n1, and n2 from the command line.
!
      NCmdLineArgs = command_argument_count()
      if(NCmdLineArgs.ne.1) then
        write(*,9000) NCmdLineArgs
        fail = .true.
      endIf
      if(fail) goto 999
      call Get_Command_Argument(1,cmd_buffer)
      read(cmd_buffer,*) nbasisfn
!
	  Allocate(hlower(nbasisfn*(nbasisfn+1)/2),EVals(nbasisfn),EVecs(nbasisfn,nbasisfn),Temp_Vector(3*nbasisfn))
!
	  m=1
	  b=1
	  l=1
!
	  k=1
	  do j=1, nbasisfn
		do i=1, nbasisfn
		 if(i.ge.j) then
			hlower(k)=PIB_1D_T_Element(m,l,i,j)+PIB_1D_V_Element(b,m,l,i,j)
			  k=k+1
			end if
		 end do
	   end do
!	  write(*,*) hlower
!
!
!     Given the input parameters, evaluate the kinetic energy integral
!     between
!     particle-in-a-box eigenfunctions n1 and n2.
!
!      tMatrixElement = PIB_1D_T_Element(m,l,n1,n2)
!     write(*,2000) n1,n2,tMatrixElement
!
!     The end of the job...
!

!
!     Given the input parameters, evaluate the potential energy integral
!     between
!     particle-in-a-box eigenfunctions n1 and n2.
!
!      vMatrixElement = PIB_1D_V_Element(b,m,l,n1,n2)
!      write(*,2000) n1,n2,vMatrixElement
!
!     The end of the job...
!
!      PIB_1D_Modified_Hamiltonian_Element() = 
      write(*,2000) n1,n2
     Write(*,*)' The matrix loaded (column) lower-triangle packed:'
	 write(*,*) hlower
!	 Call SymmetricPacked2Matrix_LowerPac(nbasisfn,hlower,Matrix)
!	 Call Print_Matrix_Full_Real(Matrix,nbasisfn,nbasisfn)
	 Call SSPEV('V','L',nbasisfn,hlower,EVals,EVecs,nbasisfn, &
	 Temp_Vector,IError)
	 If(IError.ne.0) then
	 Write(*,*)' Failure in DSPEV.'
	 STOP
	 endIf
	 Write(*,*)' EVals:'
	 Call Print_Matrix_Full_Real(RESHAPE(EVals,(/1,nbasisfn/)),1,nbasisfn)
	 Write(*,*)' EVecs:'
	 Call Print_Matrix_Full_Real(EVecs,nbasisfn,nbasisfn)
  999 continue   
      end program prgm_05_01


      real function PIB_1D_T_Element(m,l,n1,n2)
!
!     This function evaluates the kinetic energy matrix element < n1 | T
!     | n2 >,
!     where n1 and n2 are particle-in-a-box eigenstate labels and T is
!     the
!     kinetic energy operator.
!
!
!     Variable Declarations
      implicit none
      real,intent(in)::l,m
      integer,intent(in)::n1,n2
      real::prefactor
!
!     The case where n1=n2 is different than n1\=n2. For this reason, we
!     use an
!     if block to separate the evaluation of the kinetic energy integral
!     for
!     these two different cases.
!
	  real,parameter::pi=float(4)*atan(1.0)
      if(n1.eq.n2) then
      PIB_1D_T_Element=((pi**2)/m)*((n1**2)/(2*l**2))
      else
      PIB_1D_T_Element=0
      endIf
!
      end function PIB_1D_T_Element
!
      real function PIB_1D_V_Element(b,m,l,n1,n2)
!
!     This function evaluates the kinetic energy matrix element < n1 | T
!     | n2 >,
!     where n1 and n2 are particle-in-a-box eigenstate labels and T is
!     the
!     kinetic energy operator.
!
!
!     Variable Declarations
      implicit none
      real,intent(in)::b,l,m
      integer,intent(in)::n1,n2
      real::prefactor
!
!     The case where n1=n2 is different than n1\=n2. For this reason, we
!     use an
!     if block to separate the evaluation of the kinetic energy integral
!     for
!     these two different cases.
!

	  real,parameter::pi=float(4)*atan(1.0)
!
      if(n1.eq.n2) then
      PIB_1D_V_Element= (b*l)/2  !((-l)/(4*((pi)**2)*(n1**2))*(-1+Cos(2*pi*n1)+2*pi*n1*(Sin(2*pi*n1)-pi*n1)))
      else
      PIB_1D_V_Element=b*(l/pi**2)*((-1+Cos(pi*(n1-n2))/((n1-n2)**2))+((1-Cos(pi*(n1+n2)))/((n1+n2)**2)))
      endIf
!
      end function PIB_1D_V_Element
!
!
!
!
!
!


      Subroutine Print_Matrix_Full_Real(AMat,M,N)
!
!     This subroutine prints a real matrix that is fully dimension - i.e.,
!     not stored in packed form. AMat is the matrix, which is dimensioned
!     (M,N).
!
!     The output of this routine is sent to unit number 6 (set by the local
!     parameter integer IOut).
!
!
!     Variable Declarations
!

      implicit none
      integer,intent(in)::M,N
      real,dimension(M,N),intent(in)::AMat
!
!     Local variables
      integer,parameter::IOut=6,NColumns=5
      integer::i,j,IFirst,ILast
!
 1000 Format(1x,A)
 2000 Format(5x,5(7x,I7))
 2010 Format(1x,I7,5F14.6)
!
      Do IFirst = 1,N,NColumns
        ILast = Min(IFirst+NColumns-1,N)
        write(IOut,2000) (i,i=IFirst,ILast)
        Do i = 1,M
          write(IOut,2010) i,(AMat(i,j),j=IFirst,ILast)
        endDo
      endDo
!
      Return
      End Subroutine Print_Matrix_Full_Real
