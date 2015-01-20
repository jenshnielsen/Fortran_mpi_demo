program mpitest
    !use mpi_f08 !fortran 2008 interface
    use mpi
    implicit none

    integer :: rank, size, ierror

    type Point
        sequence
        real :: X, Y
    end type Point

    type Circle
        sequence
        type (Point) :: Center
        real :: Radius
    END type Circle

    type (Circle) :: C, D

    integer i
    integer nfields
    integer tag
    integer source
    integer ierr

    parameter(nfields=3)

    integer(KIND=MPI_ADDRESS_KIND) :: fieldsize
    integer(KIND=MPI_ADDRESS_KIND) :: lowerbound
    integer(KIND=MPI_ADDRESS_KIND) :: offset(0:nfields-1)
    integer :: blockcounts(0:nfields-1)

    integer stat(MPI_STATUS_SIZE)
    integer mpicircletype
    integer :: subtypes(0:nfields-1)

    !type(MPI_Status) :: stat
    !type(MPI_Datatype) :: mpicircletype
    !type(MPI_Datatype) :: subtypes(0:nfields-1)

    tag = 0
    source = 0

    call MPI_Init(ierror)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

    call MPI_TYPE_GET_EXTENT(MPI_REAL, lowerbound, fieldsize, ierr)

    do i = 0,nfields-1
        offset(i) =      i*fieldsize
        subtypes(i) =    MPI_REAL
        blockcounts(i) = 1
    end do

    call MPI_Type_create_struct(nfields, blockcounts, offset, subtypes, mpicircletype, ierr)
    call MPI_TYPE_COMMIT(mpicircletype, ierr)

    C = Circle(Point(0., 0.), rank+1)
    D = Circle(Point(0., 0.), 0.)



    if (rank .eq. 0) then
        do i=0,size-1
            call MPI_SEND(C, 1, mpicircletype, i, tag, MPI_COMM_WORLD, ierr)
        end do
    endif

    call MPI_RECV(D, 1, mpicircletype, source, tag, MPI_COMM_WORLD, stat, ierr)

    write(*,*) 'Hello World, I am ', rank, ' of ', size, 'and my circle is', C%Radius, &
        'wide centered at', C%Center%X, C%Center%Y, 'the other one is' ,  D%Radius, &
        'wide centered at', D%Center%X, D%Center%Y

    call MPI_Finalize(ierror)
end