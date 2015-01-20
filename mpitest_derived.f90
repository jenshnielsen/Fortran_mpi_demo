program mpitest

    include 'mpif.h'
    !use mpi_f08

    integer :: rank, size, ierror
    type Point
      real :: X, Y
    end type Point

    type Circle
      type (Point) :: Center
      real :: Radius
    END type Circle

    type (Circle) :: C, D
    integer i
    integer nfields
    integer tag
    integer source

    parameter(nfields=3)

    integer(KIND=MPI_ADDRESS_KIND) :: fieldsize
    integer(KIND=MPI_ADDRESS_KIND) :: lowerbound
    integer(KIND=MPI_ADDRESS_KIND) :: offset(0:nfields-1)
    integer :: blockcounts(0:nfields-1)
    integer stat(MPI_STATUS_SIZE)

    integer mpicircletype
    !type(MPI_Datatype) :: mpicircletype

    integer :: subtypes(0:nfields-1)
    !type(MPI_Datatype) :: subtypes(0:nfields-1)

    tag = 0
    source = 0

    call MPI_Init(ierror)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

    call MPI_TYPE_GET_EXTENT(MPI_INTEGER, lowerbound, fieldsize, ierr)

    do i = 0,nfields-1
        offset(i) =      i*fieldsize
        subtypes(i) =    MPI_INTEGER
        blockcounts(i) = 1
    end do

    call MPI_Type_create_struct(nfields, blockcounts, offsets, subtypes, mpicircletype, ierr)
    !call MPI_TYPE_STRUCT(pm, blockcounts, offsets, subtypes, mpicircletype, ierr)
    call MPI_TYPE_COMMIT(mpicircletype, ierr)


    !MPI_TYPE_STRUCT (count,  blocklens(),  offsets(),  old_type(),  newtype,  ierr)
    C = Circle(Point(0., 0.), rank+1)

    D = Circle(Point(0., 0.), 0.)



    if (rank .eq. 0) then
        do i=1,size-1
            write(*,*) "sending to ", i
            call MPI_SEND(C, 1, mpicircletype, i, tag, MPI_COMM_WORLD, ierr)
        end do
    endif

    write(*,*) "recieving on rank", rank
    call MPI_RECV(D, 1, mpicircletype, source, tag, MPI_COMM_WORLD, stat, ierr)

    write(*,*) 'Hello World, I am ', rank, ' of ', size, 'and my circle is ', C%Radius, ' wide centered at ', C%Center%X, C%Center%Y

    call MPI_Finalize(ierror)
end