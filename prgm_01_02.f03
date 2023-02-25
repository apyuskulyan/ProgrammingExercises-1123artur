      Program prgm_01_02
!
!     This program reads a 3x3 matrix from a user-provided input file. After the
!     file is opened and read, it is closed and then printed.
!
!
      implicit none
      integer,parameter::inFileUnitA=10
      integer,parameter::inFileUnitB=10      
      integer::errorFlag,i
      real,dimension(3,3)::matrixInA
      character(len=128)::fileNameA
      real,dimension(3,3)::matrixInB
      character(len=128)::fileNameB
!
!
!     Start by asking the user for the name of the data file.
!
      write(*,*)' What is the name of the input data file?'
      read(*,*) fileNameA
!
!     Open the data file and read matrixInA from that file.
!
      open(unit=inFileUnitA,file=TRIM(fileNameA),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do i = 1,3
        read(inFileUnitA,*) matrixInA(1,i),matrixInA(2,i),matrixInA(3,i)
      endDo
      close(inFileUnitA)
!
!
      write(*,*)' What is the name of the input data file?'
      read(*,*) fileNameB
!
!     Open the data file and read matrixInA from that file.
!
      open(unit=inFileUnitB,file=TRIM(fileNameB),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do i = 1,3
        read(inFileUnitB,*) matrixInB(1,i),matrixInB(2,i),matrixInB(3,i)
      endDo
      close(inFileUnitB)
!
!     Call the subroutine PrintMatrix to print matrixInA.
!
      call PrintMatrix3x3(matrixInA)
      call PrintMatrix3x3(matrixInB)
!
  999 continue
      End Program prgm_01_02


      Subroutine PrintMatrix3x3(matrix)
!
!     This subroutine prints a 3x3 real matrix. The output is written to StdOut.
!
      implicit none
      real,dimension(3,3),intent(in)::matrix
      real,dimension(3,3) ::StdOut
      integer::i
!
!     Format statements.
!
 1000 format(3(2x,f5.1))
!
!     Do the printing job.
!
      write(*,*)' Printing Matrix'
!
      do i= 1,3
               StdOut(3,i)=matrix(3,i)
               StdOut(2,i)=matrix(2,i)
               StdOut(1,i)=matrix(1,i)
          endDo  
      write(*,*)
      write(*,1000) transpose(StdOut)
      write(*,*)
!
      return
      End Subroutine PrintMatrix3x3
