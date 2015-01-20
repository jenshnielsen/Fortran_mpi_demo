program mpitest
    !use mpi_f08
    use mpi
    implicit none

    integer :: rank, size, ierror
    integer :: i,C,D,tag,source

    integer stat(MPI_STATUS_SIZE)
    integer request

!     type(MPI_Status) :: stat
!     type(MPI_Request) :: request

    tag = 0
    source = 0

    call MPI_Init(ierror)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

    C = rank
    D = rank

    call MPI_IRECV(D, 1, MPI_INTEGER, source, tag, MPI_COMM_WORLD, request, ierror)

    if (rank .eq. 0) then
        do i=0,size-1
            C = 1234
            call MPI_SEND(C, 1, MPI_INTEGER, i, tag, MPI_COMM_WORLD, ierror)
        end do
    endif

    call MPI_Wait(request, stat, ierror)

    write(*,*) 'Hello World, I am ', rank, ' of ', size, 'and my C and D are ', C, 'and', D
    call MPI_Finalize(ierror)
end