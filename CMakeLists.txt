CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
project ("Dummy fortran mpi example" Fortran)
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/")

include(CheckMPI)
set(F90_SOURCES mpitest.f90)

set(FREELINE_SOURCES mpitest.f90)

enable_language(Fortran)

if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  SET_SOURCE_FILES_PROPERTIES( ${FREELINE_SOURCES} PROPERTIES COMPILE_FLAGS
    -ffree-line-length-0 )
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "NAG")
  set (CMAKE_Fortran_FLAGS "-kind=byte -ieee=full")
endif()

set(CMAKE_LIBRARY_PATH "/usr/lib64")
message( STATUS "LIBPATH ${CMAKE_LIBRARY_PATH}")
fortran_check_MPI()


include_directories(${MPI_Fortran_INCLUDE_PATH})

add_executable(mpitest.x mpitest.f90)

if(MPI_Fortran_LIBRARIES)
  set_target_properties(mpitest.x PROPERTIES
    LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
  message( STATUS "Adding Cmake flags")
endif()

if(MPI_Fortran_COMPILE_FLAGS)
  set_target_properties(mpitest.x PROPERTIES
    COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}")
endif()

target_link_libraries(mpitest.x ${MPI_Fortran_LIBRARIES})

