program mpitest
    ! Demonstrates the need for correct alignment when using mpi_type_create. 
    ! The size of a real is half the size of the alignment of a double so you will 
    ! need to pad with an additional real ...
    !use mpi_f08 !fortran 2008 interface
    use mpi
    USE ISO_C_BINDING
    implicit none

    integer :: rank, size, ierror

    type :: Circle
        sequence
        real :: Radius
        !real :: Gab
        double precision :: DRadius
    END type Circle

    type (Circle) :: C, D

    integer i
    integer nfields
    integer tag
    integer source
    integer ierr
    integer nreals

    parameter(nfields=2)
    parameter(nreals=1)

    integer(KIND=MPI_ADDRESS_KIND) :: realsize
    integer(KIND=MPI_ADDRESS_KIND) :: doublesize    
    integer(KIND=MPI_ADDRESS_KIND) :: lowerbound
    integer(KIND=MPI_ADDRESS_KIND) :: offset(0:nfields-1)
    integer :: blockcounts(0:nfields-1)

    integer stat(MPI_STATUS_SIZE)
    integer request
    integer mpicircletype
    integer :: subtypes(0:nfields-1)

!     type(MPI_Status) :: stat
!     type(MPI_Request) :: request
!     type(MPI_Datatype) :: mpicircletype
!     type(MPI_Datatype) :: subtypes(0:nfields-1)

    tag = 0
    source = 0

    call MPI_Init(ierror)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

    call MPI_TYPE_GET_EXTENT(MPI_REAL, lowerbound, realsize, ierr)
    call MPI_TYPE_GET_EXTENT(MPI_DOUBLE, lowerbound, doublesize, ierr)

    offset(0) = 0
    offset(1) = nreals*realsize
    
    subtypes(0) = MPI_REAL
    subtypes(1) = MPI_DOUBLE

    blockcounts(0) = nreals
    blockcounts(1) = 1

    call MPI_Type_create_struct(nfields, blockcounts, offset, subtypes, mpicircletype, ierr)
    call MPI_TYPE_COMMIT(mpicircletype, ierr)


    if (nreals .eq. 2) then
        !C = Circle(123.123, 0.0, 999.123)
        !D = Circle(0., 0.0, 0.)
    else
        C = Circle(123.123, 999.123)
        D = Circle(0., 0.)
    endif


    call MPI_IRECV(D, 1, mpicircletype, source, tag, MPI_COMM_WORLD, request, ierr)

    if (rank .eq. 0) then
        do i=0,size-1
            call MPI_SEND(C, 1, mpicircletype, i, tag, MPI_COMM_WORLD, ierr)
        end do
    endif

    call MPI_Wait(request, stat, ierror)

    write(*,*) 'Hello World, I am ', rank, ' of ', size, 'and my circle is', C%Radius, C%DRadius, 'the other one is',  &
        D%Radius,  D%DRadius

    call MPI_Finalize(ierror)
end