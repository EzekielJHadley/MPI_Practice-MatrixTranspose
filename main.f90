      program main
      use my_constants
      implicit none

      INTEGER:: ierr, nproc, rank
      INTEGER:: stat(MPI_STATUS_SIZE)
      INTEGER, ALLOCATABLE, DIMENSION(:,:):: matrix
      INTEGER, ALLOCATABLE, DIMENSION(:,:):: subMat, matrixT
      INTEGER, DIMENSION(2):: nBym
      INTEGER:: i, j, k, x, y

      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

      OPEN (unit = 1, file='input.txt', status='old', action='read')
      READ(1, *) nBym(1), nBym(2)
      !2 FORMAT (2I3)
      ALLOCATE(matrix(1:nBym(1), 1:nBym(2)))
      
      IF (rank == 0) THEN
            PRINT *, nBym(1), nBym(2)
      END IF

      DO i=1, nBym(1)
            READ(1, *) matrix(i,:)
      END DO
      3 FORMAT (<nBym(1)>I2)

      !break up the large matrix into chunks and transpose it
      !nBym = shape(matrix)
      ALLOCATE(matrixT(1:nBym(2), 1:nBym(1)))
      x = nBym(1) / 8
      y = nBym(2) / 8
      ALLOCATE(subMat(1:y, 1:x))
      DO i = 1, y
            DO j =1, x
                  subMat(i,j) = matrix(MOD(rank,8)*x + j, &
                        rank/8*y + i)
            END DO
      END DO

      !now to make sure all the proc catch up before sharing
      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      !PRINT *, "Point 1, i am rank:", rank

      IF(rank == 0) THEN
            DO k = 0, 63
                  IF (k .ne. 0) THEN
                        call  MPI_RECV(subMat, x*y, MPI_INTEGER, k, k, &
                              MPI_COMM_WORLD, stat, ierr)
                              PRINT *, "Rank 0 just recieved msg from:", k
                              Do j=1, y
                                    print *, (subMat(j, i), i=1, x)
                              END DO
                  END IF
                  DO j = 1, y
                        DO i = 1, x
                              matrixT(k/8*y+j, MOD(k,8)*x+i) = subMat(j, i)
                        END DO
                  END DO
            END DO
            DO i=1, nBym(2)
                  PRINT 4, (matrixT(i, j), j=1, nBym(1))
                  4 FORMAT(<nBym(2)>I2)
            END DO
            PRINT *,""
            DO i=1, nBym(1)
                  PRINT 4, (matrix(i, j), j=1, nBym(2))
            END DO
      ELSE
            call  MPI_SEND(subMat, x*y, MPI_INTEGER, 0, rank, &
                  MPI_COMM_WORLD, ierr)
      END IF

      call MPI_FINALIZE(ierr)
      end program main

