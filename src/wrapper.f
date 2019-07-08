      SUBROUTINE csgesdd(jobz, m, n, a, lda, s, u, ldu, vt, ldvt,
     $                    work, lwork, iwork, info)
     +     BIND(C, name = 'Csgesdd')
        use iso_c_binding, only : c_char, c_int, c_float
        character(c_char):: jobz
        integer(c_int):: info, lda, ldu, ldvt, lwork, m, n, iwork( * )
        real(c_float):: a( lda, * ), s( * ), u( ldu, * )
        real(c_float):: vt( ldvt, * ), work( * )
        CALL sgesdd(jobz, m, n, a, lda, s, u, ldu, vt, ldvt, work,
     $              lwork, iwork, info)
      END SUBROUTINE csgesdd
      SUBROUTINE csgemm(transa, transb, m, n, k, alpha, a, lda, b,
     $                   ldb, beta, c, ldc)
     +     BIND(C, name = 'Csgemm')
        use iso_c_binding, only : c_char, c_int, c_float
        character(c_char):: transa, transb
        integer(c_int):: m, n, k, lda, ldb, ldc
        real(c_float):: alpha, a( lda, * ), b( ldb,  * ), beta
        real(c_float):: c( ldc, *)
        CALL sgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb,
     $             beta, c, ldc)
      END SUBROUTINE csgemm
