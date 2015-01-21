program mpitest
    !use mpi_f08
    use mpi
    implicit none

    integer :: rank, size, ierror
    integer :: i,tag,source

    real :: A,B,C(2),buffer(2)

    integer stat(MPI_STATUS_SIZE)
    integer request
    integer position
    integer buffsize

!     type(MPI_Status) :: stat
!     type(MPI_Request) :: request

    tag = 0
    source = 0
    position = 0

    call MPI_Init(ierror)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

    A = 123.456
    B = 789.123
    buffsize = sizeof(A)*2

    call MPI_IRECV(C, 2, MPI_REAL, source, tag, MPI_COMM_WORLD, request, ierror)

    if (rank .eq. 0) then
        call MPI_Pack( A, 1, MPI_REAL, buffer, buffsize, position, MPI_COMM_WORLD, ierror)
        call MPI_Pack( B, 1, MPI_REAL, buffer, buffsize, position, MPI_COMM_WORLD, ierror)
        do i=0,size-1
            call MPI_Send( buffer, position, MPI_PACKED, i, tag, MPI_COMM_WORLD, ierror)
        end do
    endif

    call MPI_Wait(request, stat, ierror)

    write(*,*) 'Hello World, I am ', rank, ' of ', size, 'and my values are', C(1), 'and', C(2)
    call MPI_Finalize(ierror)
end