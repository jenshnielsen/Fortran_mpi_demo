program mpitest

    include 'mpif.h'
    !use mpi_f08

    integer :: rank, size, ierror
    integer :: C,D,tag,source
    integer stat(MPI_STATUS_SIZE)

    tag = 0
    source = 0


    call MPI_Init(ierror)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

    C = rank
    D = rank

    write(*,*) 'Hello World, I am ', rank, ' of ', size, 'and my C and D are ', C, 'and', D
    
    if (rank .eq. 0) then
        do i=0,size-1
            C = 1234
            call MPI_SEND(C, 1, MPI_INTEGER, i, tag, MPI_COMM_WORLD, ierr)
        end do
    endif

    call MPI_RECV(D, 1, MPI_INTEGER, source, tag, MPI_COMM_WORLD, stat, ierr)

    write(*,*) 'Hello World, I am ', rank, ' of ', size, 'and my C and D are ', C, 'and', D
    call MPI_Finalize(ierror)
end