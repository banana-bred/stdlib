module stdlib_lapack_base
  use stdlib_linalg_constants
  use stdlib_linalg_lapack_aux
  use stdlib_linalg_blas
  implicit none

interface 
     pure real(sp) module function stdlib_slamch( cmach )
           character, intent(in) :: cmach
     end function stdlib_slamch

     pure real(dp) module function stdlib_dlamch( cmach )
           character, intent(in) :: cmach
     end function stdlib_dlamch

     pure real(xdp) module function stdlib_xlamch( cmach )
           character, intent(in) :: cmach
     end function stdlib_xlamch

     pure real(qp) module function stdlib_qlamch( cmach )
           character, intent(in) :: cmach
     end function stdlib_qlamch


end interface 


interface 
     pure real(sp) module function stdlib_slamc3( a, b )
           real(sp), intent(in) :: a, b
     end function stdlib_slamc3

     pure real(dp) module function stdlib_dlamc3( a, b )
           real(dp), intent(in) :: a, b
     end function stdlib_dlamc3

     pure real(xdp) module function stdlib_xlamc3( a, b )
           real(xdp), intent(in) :: a, b
     end function stdlib_xlamc3

     pure real(qp) module function stdlib_qlamc3( a, b )
           real(qp), intent(in) :: a, b
     end function stdlib_qlamc3


end interface 


interface 
     pure module subroutine stdlib_slabad( small, large )
           real(sp), intent(inout) :: large, small
     end subroutine stdlib_slabad

     pure module subroutine stdlib_dlabad( small, large )
           real(dp), intent(inout) :: large, small
     end subroutine stdlib_dlabad

     pure module subroutine stdlib_xlabad( small, large )
           real(xdp), intent(inout) :: large, small
     end subroutine stdlib_xlabad

     pure module subroutine stdlib_qlabad( small, large )
           real(qp), intent(inout) :: large, small
     end subroutine stdlib_qlabad


end interface 


interface 
     pure real(sp) module function stdlib_scsum1( n, cx, incx )
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(in) :: cx(*)
     end function stdlib_scsum1

end interface 


interface 
     pure real(dp) module function stdlib_dzsum1( n, cx, incx )
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(in) :: cx(*)
     end function stdlib_dzsum1

     pure real(xdp) module function stdlib_xzsum1( n, cx, incx )
           integer(ilp), intent(in) :: incx, n
           complex(xdp), intent(in) :: cx(*)
     end function stdlib_xzsum1

     pure real(qp) module function stdlib_qzsum1( n, cx, incx )
           integer(ilp), intent(in) :: incx, n
           complex(qp), intent(in) :: cx(*)
     end function stdlib_qzsum1


end interface 


interface 
     pure module subroutine stdlib_slaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: amax, scond
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(in) :: s(*)
     end subroutine stdlib_slaqsb

     pure module subroutine stdlib_dlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: amax, scond
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(in) :: s(*)
     end subroutine stdlib_dlaqsb

     pure module subroutine stdlib_xlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(xdp), intent(in) :: amax, scond
           real(xdp), intent(inout) :: ab(ldab,*)
           real(xdp), intent(in) :: s(*)
     end subroutine stdlib_xlaqsb

     pure module subroutine stdlib_qlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(qp), intent(in) :: amax, scond
           real(qp), intent(inout) :: ab(ldab,*)
           real(qp), intent(in) :: s(*)
     end subroutine stdlib_qlaqsb


     pure module subroutine stdlib_claqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: amax, scond
           real(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_claqsb

     pure module subroutine stdlib_zlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: amax, scond
           real(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_zlaqsb

     pure module subroutine stdlib_ylaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(xdp), intent(in) :: amax, scond
           real(xdp), intent(in) :: s(*)
           complex(xdp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_ylaqsb

     pure module subroutine stdlib_wlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(qp), intent(in) :: amax, scond
           real(qp), intent(in) :: s(*)
           complex(qp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_wlaqsb


end interface 


interface 
     pure module subroutine stdlib_sladiv1( a, b, c, d, p, q )
           real(sp), intent(inout) :: a
           real(sp), intent(in) :: b, c, d
           real(sp), intent(out) :: p, q
     end subroutine stdlib_sladiv1

     pure module subroutine stdlib_dladiv1( a, b, c, d, p, q )
           real(dp), intent(inout) :: a
           real(dp), intent(in) :: b, c, d
           real(dp), intent(out) :: p, q
     end subroutine stdlib_dladiv1

     pure module subroutine stdlib_xladiv1( a, b, c, d, p, q )
           real(xdp), intent(inout) :: a
           real(xdp), intent(in) :: b, c, d
           real(xdp), intent(out) :: p, q
     end subroutine stdlib_xladiv1

     pure module subroutine stdlib_qladiv1( a, b, c, d, p, q )
           real(qp), intent(inout) :: a
           real(qp), intent(in) :: b, c, d
           real(qp), intent(out) :: p, q
     end subroutine stdlib_qladiv1


end interface 


interface 
     pure real(sp) module function stdlib_sladiv2( a, b, c, d, r, t )
           real(sp), intent(in) :: a, b, c, d, r, t
     end function stdlib_sladiv2

     pure real(dp) module function stdlib_dladiv2( a, b, c, d, r, t )
           real(dp), intent(in) :: a, b, c, d, r, t
     end function stdlib_dladiv2

     pure real(xdp) module function stdlib_xladiv2( a, b, c, d, r, t )
           real(xdp), intent(in) :: a, b, c, d, r, t
     end function stdlib_xladiv2

     pure real(qp) module function stdlib_qladiv2( a, b, c, d, r, t )
           real(qp), intent(in) :: a, b, c, d, r, t
     end function stdlib_qladiv2


end interface 


interface 
     pure module subroutine stdlib_crot( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: c
           complex(sp), intent(in) :: s
           complex(sp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_crot

     pure module subroutine stdlib_zrot( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(in) :: c
           complex(dp), intent(in) :: s
           complex(dp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_zrot

     pure module subroutine stdlib_yrot( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           real(xdp), intent(in) :: c
           complex(xdp), intent(in) :: s
           complex(xdp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_yrot

     pure module subroutine stdlib_wrot( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           real(qp), intent(in) :: c
           complex(qp), intent(in) :: s
           complex(qp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_wrot


end interface 


interface 
     pure module subroutine stdlib_slaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: alpha, beta
           real(sp), intent(out) :: a(lda,*)
     end subroutine stdlib_slaset

     pure module subroutine stdlib_dlaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: alpha, beta
           real(dp), intent(out) :: a(lda,*)
     end subroutine stdlib_dlaset

     pure module subroutine stdlib_xlaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           real(xdp), intent(in) :: alpha, beta
           real(xdp), intent(out) :: a(lda,*)
     end subroutine stdlib_xlaset

     pure module subroutine stdlib_qlaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           real(qp), intent(in) :: alpha, beta
           real(qp), intent(out) :: a(lda,*)
     end subroutine stdlib_qlaset


     pure module subroutine stdlib_claset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(in) :: alpha, beta
           complex(sp), intent(out) :: a(lda,*)
     end subroutine stdlib_claset

     pure module subroutine stdlib_zlaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(in) :: alpha, beta
           complex(dp), intent(out) :: a(lda,*)
     end subroutine stdlib_zlaset

     pure module subroutine stdlib_ylaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           complex(xdp), intent(in) :: alpha, beta
           complex(xdp), intent(out) :: a(lda,*)
     end subroutine stdlib_ylaset

     pure module subroutine stdlib_wlaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           complex(qp), intent(in) :: alpha, beta
           complex(qp), intent(out) :: a(lda,*)
     end subroutine stdlib_wlaset


end interface 


interface 
     pure module subroutine stdlib_slarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(sp), intent(out) :: x(*)
     end subroutine stdlib_slarnv

     pure module subroutine stdlib_dlarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(dp), intent(out) :: x(*)
     end subroutine stdlib_dlarnv

     pure module subroutine stdlib_xlarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(xdp), intent(out) :: x(*)
     end subroutine stdlib_xlarnv

     pure module subroutine stdlib_qlarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(qp), intent(out) :: x(*)
     end subroutine stdlib_qlarnv


     pure module subroutine stdlib_clarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           complex(sp), intent(out) :: x(*)
     end subroutine stdlib_clarnv

     pure module subroutine stdlib_zlarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           complex(dp), intent(out) :: x(*)
     end subroutine stdlib_zlarnv

     pure module subroutine stdlib_ylarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           complex(xdp), intent(out) :: x(*)
     end subroutine stdlib_ylarnv

     pure module subroutine stdlib_wlarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           complex(qp), intent(out) :: x(*)
     end subroutine stdlib_wlarnv


end interface 


interface 
     pure module subroutine stdlib_slaruv( iseed, n, x )
           integer(ilp), intent(in) :: n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(sp), intent(out) :: x(n)
     end subroutine stdlib_slaruv

     pure module subroutine stdlib_dlaruv( iseed, n, x )
           integer(ilp), intent(in) :: n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(dp), intent(out) :: x(n)
     end subroutine stdlib_dlaruv

     pure module subroutine stdlib_xlaruv( iseed, n, x )
           integer(ilp), intent(in) :: n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(xdp), intent(out) :: x(n)
     end subroutine stdlib_xlaruv

     pure module subroutine stdlib_qlaruv( iseed, n, x )
           integer(ilp), intent(in) :: n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(qp), intent(out) :: x(n)
     end subroutine stdlib_qlaruv


end interface 


interface 
     pure module subroutine stdlib_slacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: b(ldb,*)
     end subroutine stdlib_slacpy

     pure module subroutine stdlib_dlacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: b(ldb,*)
     end subroutine stdlib_dlacpy

     pure module subroutine stdlib_xlacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(xdp), intent(in) :: a(lda,*)
           real(xdp), intent(out) :: b(ldb,*)
     end subroutine stdlib_xlacpy

     pure module subroutine stdlib_qlacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(qp), intent(in) :: a(lda,*)
           real(qp), intent(out) :: b(ldb,*)
     end subroutine stdlib_qlacpy


     pure module subroutine stdlib_clacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: b(ldb,*)
     end subroutine stdlib_clacpy

     pure module subroutine stdlib_zlacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: b(ldb,*)
     end subroutine stdlib_zlacpy

     pure module subroutine stdlib_ylacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           complex(xdp), intent(in) :: a(lda,*)
           complex(xdp), intent(out) :: b(ldb,*)
     end subroutine stdlib_ylacpy

     pure module subroutine stdlib_wlacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           complex(qp), intent(in) :: a(lda,*)
           complex(qp), intent(out) :: b(ldb,*)
     end subroutine stdlib_wlacpy


end interface 


interface 
     pure module subroutine stdlib_clacp2( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: b(ldb,*)
     end subroutine stdlib_clacp2

     pure module subroutine stdlib_zlacp2( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: b(ldb,*)
     end subroutine stdlib_zlacp2

     pure module subroutine stdlib_ylacp2( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(xdp), intent(in) :: a(lda,*)
           complex(xdp), intent(out) :: b(ldb,*)
     end subroutine stdlib_ylacp2

     pure module subroutine stdlib_wlacp2( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(qp), intent(in) :: a(lda,*)
           complex(qp), intent(out) :: b(ldb,*)
     end subroutine stdlib_wlacp2


end interface 


interface 
     pure module subroutine stdlib_stfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: ap(0_ilp:*)
           real(sp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_stfttp

     pure module subroutine stdlib_dtfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: ap(0_ilp:*)
           real(dp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_dtfttp

     pure module subroutine stdlib_xtfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(xdp), intent(out) :: ap(0_ilp:*)
           real(xdp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_xtfttp

     pure module subroutine stdlib_qtfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(qp), intent(out) :: ap(0_ilp:*)
           real(qp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_qtfttp


     pure module subroutine stdlib_ctfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(sp), intent(out) :: ap(0_ilp:*)
           complex(sp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ctfttp

     pure module subroutine stdlib_ztfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(dp), intent(out) :: ap(0_ilp:*)
           complex(dp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ztfttp

     pure module subroutine stdlib_ytfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(xdp), intent(out) :: ap(0_ilp:*)
           complex(xdp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ytfttp

     pure module subroutine stdlib_wtfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(qp), intent(out) :: ap(0_ilp:*)
           complex(qp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_wtfttp


end interface 


interface 
     pure module subroutine stdlib_stfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(sp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           real(sp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_stfttr

     pure module subroutine stdlib_dtfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(dp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           real(dp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_dtfttr

     pure module subroutine stdlib_xtfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(xdp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           real(xdp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_xtfttr

     pure module subroutine stdlib_qtfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(qp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           real(qp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_qtfttr


     pure module subroutine stdlib_ctfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(sp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           complex(sp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ctfttr

     pure module subroutine stdlib_ztfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(dp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           complex(dp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ztfttr

     pure module subroutine stdlib_ytfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(xdp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           complex(xdp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ytfttr

     pure module subroutine stdlib_wtfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(qp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           complex(qp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_wtfttr


end interface 


interface 
     pure module subroutine stdlib_stpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: ap(0_ilp:*)
           real(sp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_stpttf

     pure module subroutine stdlib_dtpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: ap(0_ilp:*)
           real(dp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_dtpttf

     pure module subroutine stdlib_xtpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(xdp), intent(in) :: ap(0_ilp:*)
           real(xdp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_xtpttf

     pure module subroutine stdlib_qtpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(qp), intent(in) :: ap(0_ilp:*)
           real(qp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_qtpttf


     pure module subroutine stdlib_ctpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(sp), intent(in) :: ap(0_ilp:*)
           complex(sp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ctpttf

     pure module subroutine stdlib_ztpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(dp), intent(in) :: ap(0_ilp:*)
           complex(dp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ztpttf

     pure module subroutine stdlib_ytpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(xdp), intent(in) :: ap(0_ilp:*)
           complex(xdp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ytpttf

     pure module subroutine stdlib_wtpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(qp), intent(in) :: ap(0_ilp:*)
           complex(qp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_wtpttf


end interface 


interface 
     pure module subroutine stdlib_stpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(sp), intent(out) :: a(lda,*)
           real(sp), intent(in) :: ap(*)
     end subroutine stdlib_stpttr

     pure module subroutine stdlib_dtpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(dp), intent(out) :: a(lda,*)
           real(dp), intent(in) :: ap(*)
     end subroutine stdlib_dtpttr

     pure module subroutine stdlib_xtpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(xdp), intent(out) :: a(lda,*)
           real(xdp), intent(in) :: ap(*)
     end subroutine stdlib_xtpttr

     pure module subroutine stdlib_qtpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(qp), intent(out) :: a(lda,*)
           real(qp), intent(in) :: ap(*)
     end subroutine stdlib_qtpttr


     pure module subroutine stdlib_ctpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(sp), intent(out) :: a(lda,*)
           complex(sp), intent(in) :: ap(*)
     end subroutine stdlib_ctpttr

     pure module subroutine stdlib_ztpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(dp), intent(out) :: a(lda,*)
           complex(dp), intent(in) :: ap(*)
     end subroutine stdlib_ztpttr

     pure module subroutine stdlib_ytpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(xdp), intent(out) :: a(lda,*)
           complex(xdp), intent(in) :: ap(*)
     end subroutine stdlib_ytpttr

     pure module subroutine stdlib_wtpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(qp), intent(out) :: a(lda,*)
           complex(qp), intent(in) :: ap(*)
     end subroutine stdlib_wtpttr


end interface 


interface 
     pure module subroutine stdlib_strttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(sp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           real(sp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_strttf

     pure module subroutine stdlib_dtrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(dp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           real(dp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_dtrttf

     pure module subroutine stdlib_xtrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(xdp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           real(xdp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_xtrttf

     pure module subroutine stdlib_qtrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(qp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           real(qp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_qtrttf


     pure module subroutine stdlib_ctrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(sp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           complex(sp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ctrttf

     pure module subroutine stdlib_ztrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(dp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           complex(dp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ztrttf

     pure module subroutine stdlib_ytrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(xdp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           complex(xdp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ytrttf

     pure module subroutine stdlib_wtrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(qp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           complex(qp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_wtrttf


end interface 


interface 
     pure module subroutine stdlib_strttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: ap(*)
     end subroutine stdlib_strttp

     pure module subroutine stdlib_dtrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: ap(*)
     end subroutine stdlib_dtrttp

     pure module subroutine stdlib_xtrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(xdp), intent(in) :: a(lda,*)
           real(xdp), intent(out) :: ap(*)
     end subroutine stdlib_xtrttp

     pure module subroutine stdlib_qtrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(qp), intent(in) :: a(lda,*)
           real(qp), intent(out) :: ap(*)
     end subroutine stdlib_qtrttp


     pure module subroutine stdlib_ctrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: ap(*)
     end subroutine stdlib_ctrttp

     pure module subroutine stdlib_ztrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: ap(*)
     end subroutine stdlib_ztrttp

     pure module subroutine stdlib_ytrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(xdp), intent(in) :: a(lda,*)
           complex(xdp), intent(out) :: ap(*)
     end subroutine stdlib_ytrttp

     pure module subroutine stdlib_wtrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(qp), intent(in) :: a(lda,*)
           complex(qp), intent(out) :: ap(*)
     end subroutine stdlib_wtrttp


end interface 


interface 
     pure module subroutine stdlib_dlag2s( m, n, a, lda, sa, ldsa, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, m, n
           real(sp), intent(out) :: sa(ldsa,*)
           real(dp), intent(in) :: a(lda,*)
     end subroutine stdlib_dlag2s

     pure module subroutine stdlib_xlag2s( m, n, a, lda, sa, ldsa, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, m, n
           real(dp), intent(out) :: sa(ldsa,*)
           real(xdp), intent(in) :: a(lda,*)
     end subroutine stdlib_xlag2s

     pure module subroutine stdlib_qlag2s( m, n, a, lda, sa, ldsa, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, m, n
           real(dp), intent(out) :: sa(ldsa,*)
           real(qp), intent(in) :: a(lda,*)
     end subroutine stdlib_qlag2s


end interface 


interface 
     pure module subroutine stdlib_dlat2s( uplo, n, a, lda, sa, ldsa, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, n
           real(sp), intent(out) :: sa(ldsa,*)
           real(dp), intent(in) :: a(lda,*)
     end subroutine stdlib_dlat2s

     pure module subroutine stdlib_xlat2s( uplo, n, a, lda, sa, ldsa, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, n
           real(dp), intent(out) :: sa(ldsa,*)
           real(xdp), intent(in) :: a(lda,*)
     end subroutine stdlib_xlat2s

     pure module subroutine stdlib_qlat2s( uplo, n, a, lda, sa, ldsa, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, n
           real(dp), intent(out) :: sa(ldsa,*)
           real(qp), intent(in) :: a(lda,*)
     end subroutine stdlib_qlat2s


end interface 


interface 
     pure module subroutine stdlib_slag2d( m, n, sa, ldsa, a, lda, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, m, n
           real(sp), intent(in) :: sa(ldsa,*)
           real(dp), intent(out) :: a(lda,*)
     end subroutine stdlib_slag2d

end interface 


interface 
     pure logical(lk) module function stdlib_sisnan( sin )
           real(sp), intent(in) :: sin
     end function stdlib_sisnan

     pure logical(lk) module function stdlib_disnan( din )
           real(dp), intent(in) :: din
     end function stdlib_disnan

     pure logical(lk) module function stdlib_xisnan( din )
           real(xdp), intent(in) :: din
     end function stdlib_xisnan

     pure logical(lk) module function stdlib_qisnan( din )
           real(qp), intent(in) :: din
     end function stdlib_qisnan


end interface 


interface 
     pure logical(lk) module function stdlib_slaisnan( sin1, sin2 )
           real(sp), intent(in) :: sin1, sin2
     end function stdlib_slaisnan

     pure logical(lk) module function stdlib_dlaisnan( din1, din2 )
           real(dp), intent(in) :: din1, din2
     end function stdlib_dlaisnan

     pure logical(lk) module function stdlib_xlaisnan( din1, din2 )
           real(xdp), intent(in) :: din1, din2
     end function stdlib_xlaisnan

     pure logical(lk) module function stdlib_qlaisnan( din1, din2 )
           real(qp), intent(in) :: din1, din2
     end function stdlib_qlaisnan


end interface 


interface 
     pure module subroutine stdlib_sladiv( a, b, c, d, p, q )
           real(sp), intent(in) :: a, b, c, d
           real(sp), intent(out) :: p, q
     end subroutine stdlib_sladiv

     pure module subroutine stdlib_dladiv( a, b, c, d, p, q )
           real(dp), intent(in) :: a, b, c, d
           real(dp), intent(out) :: p, q
     end subroutine stdlib_dladiv

     pure module subroutine stdlib_xladiv( a, b, c, d, p, q )
           real(xdp), intent(in) :: a, b, c, d
           real(xdp), intent(out) :: p, q
     end subroutine stdlib_xladiv

     pure module subroutine stdlib_qladiv( a, b, c, d, p, q )
           real(qp), intent(in) :: a, b, c, d
           real(qp), intent(out) :: p, q
     end subroutine stdlib_qladiv


     pure complex(sp) module function stdlib_cladiv( x, y )
           complex(sp), intent(in) :: x, y
     end function stdlib_cladiv

     pure complex(dp)     module function stdlib_zladiv( x, y )
           complex(dp), intent(in) :: x, y
     end function stdlib_zladiv

     pure complex(xdp)     module function stdlib_yladiv( x, y )
           complex(xdp), intent(in) :: x, y
     end function stdlib_yladiv

     pure complex(qp)     module function stdlib_wladiv( x, y )
           complex(qp), intent(in) :: x, y
     end function stdlib_wladiv


end interface 


interface 
     pure real(sp) module function stdlib_slapy2( x, y )
           real(sp), intent(in) :: x, y
     end function stdlib_slapy2

     pure real(dp) module function stdlib_dlapy2( x, y )
           real(dp), intent(in) :: x, y
     end function stdlib_dlapy2

     pure real(xdp) module function stdlib_xlapy2( x, y )
           real(xdp), intent(in) :: x, y
     end function stdlib_xlapy2

     pure real(qp) module function stdlib_qlapy2( x, y )
           real(qp), intent(in) :: x, y
     end function stdlib_qlapy2


end interface 


interface 
     pure real(sp) module function stdlib_slapy3( x, y, z )
           real(sp), intent(in) :: x, y, z
     end function stdlib_slapy3

     pure real(dp) module function stdlib_dlapy3( x, y, z )
           real(dp), intent(in) :: x, y, z
     end function stdlib_dlapy3

     pure real(xdp) module function stdlib_xlapy3( x, y, z )
           real(xdp), intent(in) :: x, y, z
     end function stdlib_xlapy3

     pure real(qp) module function stdlib_qlapy3( x, y, z )
           real(qp), intent(in) :: x, y, z
     end function stdlib_qlapy3


end interface 


interface 
     pure module subroutine stdlib_clacgv( n, x, incx )
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_clacgv

     pure module subroutine stdlib_zlacgv( n, x, incx )
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_zlacgv

     pure module subroutine stdlib_ylacgv( n, x, incx )
           integer(ilp), intent(in) :: incx, n
           complex(xdp), intent(inout) :: x(*)
     end subroutine stdlib_ylacgv

     pure module subroutine stdlib_wlacgv( n, x, incx )
           integer(ilp), intent(in) :: incx, n
           complex(qp), intent(inout) :: x(*)
     end subroutine stdlib_wlacgv


end interface 


interface 
     pure module subroutine stdlib_slasrt( id, n, d, info )
           character, intent(in) :: id
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: d(*)
     end subroutine stdlib_slasrt

     pure module subroutine stdlib_dlasrt( id, n, d, info )
           character, intent(in) :: id
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: d(*)
     end subroutine stdlib_dlasrt

     pure module subroutine stdlib_xlasrt( id, n, d, info )
           character, intent(in) :: id
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(xdp), intent(inout) :: d(*)
     end subroutine stdlib_xlasrt

     pure module subroutine stdlib_qlasrt( id, n, d, info )
           character, intent(in) :: id
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(qp), intent(inout) :: d(*)
     end subroutine stdlib_qlasrt


end interface 


interface 
     pure module subroutine stdlib_slassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(sp), intent(inout) :: scl, sumsq
        real(sp), intent(in) :: x(*)
     end subroutine stdlib_slassq

     pure module subroutine stdlib_dlassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(dp), intent(inout) :: scl, sumsq
        real(dp), intent(in) :: x(*)
     end subroutine stdlib_dlassq

     pure module subroutine stdlib_xlassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(xdp), intent(inout) :: scl, sumsq
        real(xdp), intent(in) :: x(*)
     end subroutine stdlib_xlassq

     pure module subroutine stdlib_qlassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(qp), intent(inout) :: scl, sumsq
        real(qp), intent(in) :: x(*)
     end subroutine stdlib_qlassq


     pure module subroutine stdlib_classq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(sp), intent(inout) :: scl, sumsq
        complex(sp), intent(in) :: x(*)
     end subroutine stdlib_classq

     pure module subroutine stdlib_zlassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(dp), intent(inout) :: scl, sumsq
        complex(dp), intent(in) :: x(*)
     end subroutine stdlib_zlassq

     pure module subroutine stdlib_ylassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(xdp), intent(inout) :: scl, sumsq
        complex(xdp), intent(in) :: x(*)
     end subroutine stdlib_ylassq

     pure module subroutine stdlib_wlassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(qp), intent(inout) :: scl, sumsq
        complex(qp), intent(in) :: x(*)
     end subroutine stdlib_wlassq


end interface 


interface 
     pure module subroutine stdlib_srscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(in) :: sa
           real(sp), intent(inout) :: sx(*)
     end subroutine stdlib_srscl

     pure module subroutine stdlib_drscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(in) :: sa
           real(dp), intent(inout) :: sx(*)
     end subroutine stdlib_drscl

     pure module subroutine stdlib_xrscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(xdp), intent(in) :: sa
           real(xdp), intent(inout) :: sx(*)
     end subroutine stdlib_xrscl

     pure module subroutine stdlib_qrscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(qp), intent(in) :: sa
           real(qp), intent(inout) :: sx(*)
     end subroutine stdlib_qrscl


end interface 


interface 
     pure module subroutine stdlib_csrscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(in) :: sa
           complex(sp), intent(inout) :: sx(*)
     end subroutine stdlib_csrscl

end interface 


interface 
     pure module subroutine stdlib_zdrscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(in) :: sa
           complex(dp), intent(inout) :: sx(*)
     end subroutine stdlib_zdrscl

     pure module subroutine stdlib_ydrscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(xdp), intent(in) :: sa
           complex(xdp), intent(inout) :: sx(*)
     end subroutine stdlib_ydrscl

     pure module subroutine stdlib_wdrscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(qp), intent(in) :: sa
           complex(qp), intent(inout) :: sx(*)
     end subroutine stdlib_wdrscl


end interface 


interface 
     pure module subroutine stdlib_slascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(sp), intent(in) :: cfrom, cto
           real(sp), intent(inout) :: a(lda,*)
     end subroutine stdlib_slascl

     pure module subroutine stdlib_dlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(dp), intent(in) :: cfrom, cto
           real(dp), intent(inout) :: a(lda,*)
     end subroutine stdlib_dlascl

     pure module subroutine stdlib_xlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(xdp), intent(in) :: cfrom, cto
           real(xdp), intent(inout) :: a(lda,*)
     end subroutine stdlib_xlascl

     pure module subroutine stdlib_qlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(qp), intent(in) :: cfrom, cto
           real(qp), intent(inout) :: a(lda,*)
     end subroutine stdlib_qlascl


     pure module subroutine stdlib_clascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(sp), intent(in) :: cfrom, cto
           complex(sp), intent(inout) :: a(lda,*)
     end subroutine stdlib_clascl

     pure module subroutine stdlib_zlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(dp), intent(in) :: cfrom, cto
           complex(dp), intent(inout) :: a(lda,*)
     end subroutine stdlib_zlascl

     pure module subroutine stdlib_ylascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(xdp), intent(in) :: cfrom, cto
           complex(xdp), intent(inout) :: a(lda,*)
     end subroutine stdlib_ylascl

     pure module subroutine stdlib_wlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(qp), intent(in) :: cfrom, cto
           complex(qp), intent(inout) :: a(lda,*)
     end subroutine stdlib_wlascl


end interface 


interface 
     module subroutine stdlib_sla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n, trans
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_sla_geamv

     module subroutine stdlib_dla_geamv ( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n, trans
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dla_geamv

     module subroutine stdlib_xla_geamv ( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(xdp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n, trans
           real(xdp), intent(in) :: a(lda,*), x(*)
           real(xdp), intent(inout) :: y(*)
     end subroutine stdlib_xla_geamv

     module subroutine stdlib_qla_geamv ( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(qp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n, trans
           real(qp), intent(in) :: a(lda,*), x(*)
           real(qp), intent(inout) :: y(*)
     end subroutine stdlib_qla_geamv


     module subroutine stdlib_cla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           integer(ilp), intent(in) :: trans
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_cla_geamv

     module subroutine stdlib_zla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           integer(ilp), intent(in) :: trans
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_zla_geamv

     module subroutine stdlib_yla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(xdp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           integer(ilp), intent(in) :: trans
           complex(xdp), intent(in) :: a(lda,*), x(*)
           real(xdp), intent(inout) :: y(*)
     end subroutine stdlib_yla_geamv

     module subroutine stdlib_wla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(qp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           integer(ilp), intent(in) :: trans
           complex(qp), intent(in) :: a(lda,*), x(*)
           real(qp), intent(inout) :: y(*)
     end subroutine stdlib_wla_geamv


end interface 


interface 
     module subroutine stdlib_sla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           real(sp), intent(in) :: ab(ldab,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_sla_gbamv

     module subroutine stdlib_dla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           real(dp), intent(in) :: ab(ldab,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dla_gbamv

     module subroutine stdlib_xla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(xdp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           real(xdp), intent(in) :: ab(ldab,*), x(*)
           real(xdp), intent(inout) :: y(*)
     end subroutine stdlib_xla_gbamv

     module subroutine stdlib_qla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(qp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           real(qp), intent(in) :: ab(ldab,*), x(*)
           real(qp), intent(inout) :: y(*)
     end subroutine stdlib_qla_gbamv


     module subroutine stdlib_cla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           complex(sp), intent(in) :: ab(ldab,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_cla_gbamv

     module subroutine stdlib_zla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           complex(dp), intent(in) :: ab(ldab,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_zla_gbamv

     module subroutine stdlib_yla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(xdp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           complex(xdp), intent(in) :: ab(ldab,*), x(*)
           real(xdp), intent(inout) :: y(*)
     end subroutine stdlib_yla_gbamv

     module subroutine stdlib_wla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(qp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           complex(qp), intent(in) :: ab(ldab,*), x(*)
           real(qp), intent(inout) :: y(*)
     end subroutine stdlib_wla_gbamv


end interface 


interface 
     module subroutine stdlib_cla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_cla_heamv

     module subroutine stdlib_zla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_zla_heamv

     module subroutine stdlib_yla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(xdp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           complex(xdp), intent(in) :: a(lda,*), x(*)
           real(xdp), intent(inout) :: y(*)
     end subroutine stdlib_yla_heamv

     module subroutine stdlib_wla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(qp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           complex(qp), intent(in) :: a(lda,*), x(*)
           real(qp), intent(inout) :: y(*)
     end subroutine stdlib_wla_heamv


end interface 


interface 
     pure module subroutine stdlib_sla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: x(*), y(*)
           real(sp), intent(in) :: w(*)
     end subroutine stdlib_sla_wwaddw

     pure module subroutine stdlib_dla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: x(*), y(*)
           real(dp), intent(in) :: w(*)
     end subroutine stdlib_dla_wwaddw

     pure module subroutine stdlib_xla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           real(xdp), intent(inout) :: x(*), y(*)
           real(xdp), intent(in) :: w(*)
     end subroutine stdlib_xla_wwaddw

     pure module subroutine stdlib_qla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           real(qp), intent(inout) :: x(*), y(*)
           real(qp), intent(in) :: w(*)
     end subroutine stdlib_qla_wwaddw


     pure module subroutine stdlib_cla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           complex(sp), intent(inout) :: x(*), y(*)
           complex(sp), intent(in) :: w(*)
     end subroutine stdlib_cla_wwaddw

     pure module subroutine stdlib_zla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           complex(dp), intent(inout) :: x(*), y(*)
           complex(dp), intent(in) :: w(*)
     end subroutine stdlib_zla_wwaddw

     pure module subroutine stdlib_yla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           complex(xdp), intent(inout) :: x(*), y(*)
           complex(xdp), intent(in) :: w(*)
     end subroutine stdlib_yla_wwaddw

     pure module subroutine stdlib_wla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           complex(qp), intent(inout) :: x(*), y(*)
           complex(qp), intent(in) :: w(*)
     end subroutine stdlib_wla_wwaddw


end interface 


interface 
     pure module subroutine stdlib_cspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: alpha, beta
           complex(sp), intent(in) :: ap(*), x(*)
           complex(sp), intent(inout) :: y(*)
     end subroutine stdlib_cspmv

     pure module subroutine stdlib_zspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: alpha, beta
           complex(dp), intent(in) :: ap(*), x(*)
           complex(dp), intent(inout) :: y(*)
     end subroutine stdlib_zspmv

     pure module subroutine stdlib_yspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, n
           complex(xdp), intent(in) :: alpha, beta
           complex(xdp), intent(in) :: ap(*), x(*)
           complex(xdp), intent(inout) :: y(*)
     end subroutine stdlib_yspmv

     pure module subroutine stdlib_wspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, n
           complex(qp), intent(in) :: alpha, beta
           complex(qp), intent(in) :: ap(*), x(*)
           complex(qp), intent(inout) :: y(*)
     end subroutine stdlib_wspmv


end interface 


interface 
     pure module subroutine stdlib_cspr( uplo, n, alpha, x, incx, ap )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(in) :: alpha
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*)
     end subroutine stdlib_cspr

     pure module subroutine stdlib_zspr( uplo, n, alpha, x, incx, ap )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(in) :: alpha
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*)
     end subroutine stdlib_zspr

     pure module subroutine stdlib_yspr( uplo, n, alpha, x, incx, ap )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, n
           complex(xdp), intent(in) :: alpha
           complex(xdp), intent(inout) :: ap(*)
           complex(xdp), intent(in) :: x(*)
     end subroutine stdlib_yspr

     pure module subroutine stdlib_wspr( uplo, n, alpha, x, incx, ap )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, n
           complex(qp), intent(in) :: alpha
           complex(qp), intent(inout) :: ap(*)
           complex(qp), intent(in) :: x(*)
     end subroutine stdlib_wspr


end interface 


interface 
     pure module subroutine stdlib_csymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, lda, n
           complex(sp), intent(in) :: alpha, beta
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
     end subroutine stdlib_csymv

     pure module subroutine stdlib_zsymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, lda, n
           complex(dp), intent(in) :: alpha, beta
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
     end subroutine stdlib_zsymv

     pure module subroutine stdlib_ysymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, lda, n
           complex(xdp), intent(in) :: alpha, beta
           complex(xdp), intent(in) :: a(lda,*), x(*)
           complex(xdp), intent(inout) :: y(*)
     end subroutine stdlib_ysymv

     pure module subroutine stdlib_wsymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, lda, n
           complex(qp), intent(in) :: alpha, beta
           complex(qp), intent(in) :: a(lda,*), x(*)
           complex(qp), intent(inout) :: y(*)
     end subroutine stdlib_wsymv


end interface 


interface 
     pure module subroutine stdlib_csyr( uplo, n, alpha, x, incx, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, lda, n
           complex(sp), intent(in) :: alpha
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*)
     end subroutine stdlib_csyr

     pure module subroutine stdlib_zsyr( uplo, n, alpha, x, incx, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, lda, n
           complex(dp), intent(in) :: alpha
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*)
     end subroutine stdlib_zsyr

     pure module subroutine stdlib_ysyr( uplo, n, alpha, x, incx, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, lda, n
           complex(xdp), intent(in) :: alpha
           complex(xdp), intent(inout) :: a(lda,*)
           complex(xdp), intent(in) :: x(*)
     end subroutine stdlib_ysyr

     pure module subroutine stdlib_wsyr( uplo, n, alpha, x, incx, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, lda, n
           complex(qp), intent(in) :: alpha
           complex(qp), intent(inout) :: a(lda,*)
           complex(qp), intent(in) :: x(*)
     end subroutine stdlib_wsyr


end interface 


interface 
     pure module subroutine stdlib_slagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(in) :: alpha, beta
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_slagtm

     pure module subroutine stdlib_dlagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(in) :: alpha, beta
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_dlagtm

     pure module subroutine stdlib_xlagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(xdp), intent(in) :: alpha, beta
           real(xdp), intent(inout) :: b(ldb,*)
           real(xdp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_xlagtm

     pure module subroutine stdlib_qlagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(qp), intent(in) :: alpha, beta
           real(qp), intent(inout) :: b(ldb,*)
           real(qp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_qlagtm


     pure module subroutine stdlib_clagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(in) :: alpha, beta
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_clagtm

     pure module subroutine stdlib_zlagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(in) :: alpha, beta
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_zlagtm

     pure module subroutine stdlib_ylagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(xdp), intent(in) :: alpha, beta
           complex(xdp), intent(inout) :: b(ldb,*)
           complex(xdp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_ylagtm

     pure module subroutine stdlib_wlagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(qp), intent(in) :: alpha, beta
           complex(qp), intent(inout) :: b(ldb,*)
           complex(qp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_wlagtm


end interface 


interface 
     pure module subroutine stdlib_clacrm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(sp), intent(in) :: b(ldb,*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: c(ldc,*)
     end subroutine stdlib_clacrm

     pure module subroutine stdlib_zlacrm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(dp), intent(in) :: b(ldb,*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: c(ldc,*)
     end subroutine stdlib_zlacrm

     pure module subroutine stdlib_ylacrm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(xdp), intent(in) :: b(ldb,*)
           real(xdp), intent(out) :: rwork(*)
           complex(xdp), intent(in) :: a(lda,*)
           complex(xdp), intent(out) :: c(ldc,*)
     end subroutine stdlib_ylacrm

     pure module subroutine stdlib_wlacrm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(qp), intent(in) :: b(ldb,*)
           real(qp), intent(out) :: rwork(*)
           complex(qp), intent(in) :: a(lda,*)
           complex(qp), intent(out) :: c(ldc,*)
     end subroutine stdlib_wlacrm


end interface 


interface 
     pure module subroutine stdlib_clarcm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: b(ldb,*)
           complex(sp), intent(out) :: c(ldc,*)
     end subroutine stdlib_clarcm

     pure module subroutine stdlib_zlarcm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: b(ldb,*)
           complex(dp), intent(out) :: c(ldc,*)
     end subroutine stdlib_zlarcm

     pure module subroutine stdlib_ylarcm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(xdp), intent(in) :: a(lda,*)
           real(xdp), intent(out) :: rwork(*)
           complex(xdp), intent(in) :: b(ldb,*)
           complex(xdp), intent(out) :: c(ldc,*)
     end subroutine stdlib_ylarcm

     pure module subroutine stdlib_wlarcm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(qp), intent(in) :: a(lda,*)
           real(qp), intent(out) :: rwork(*)
           complex(qp), intent(in) :: b(ldb,*)
           complex(qp), intent(out) :: c(ldc,*)
     end subroutine stdlib_wlarcm


end interface 


interface 
     pure module subroutine stdlib_chfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: c(*)
     end subroutine stdlib_chfrk

     pure module subroutine stdlib_zhfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: c(*)
     end subroutine stdlib_zhfrk

     pure module subroutine stdlib_yhfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(xdp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           complex(xdp), intent(in) :: a(lda,*)
           complex(xdp), intent(inout) :: c(*)
     end subroutine stdlib_yhfrk

     pure module subroutine stdlib_whfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(qp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           complex(qp), intent(in) :: a(lda,*)
           complex(qp), intent(inout) :: c(*)
     end subroutine stdlib_whfrk


end interface 


interface 
     pure module subroutine stdlib_stfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           real(sp), intent(in) :: alpha
           real(sp), intent(in) :: a(0_ilp:*)
           real(sp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_stfsm

     pure module subroutine stdlib_dtfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           real(dp), intent(in) :: alpha
           real(dp), intent(in) :: a(0_ilp:*)
           real(dp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_dtfsm

     pure module subroutine stdlib_xtfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           real(xdp), intent(in) :: alpha
           real(xdp), intent(in) :: a(0_ilp:*)
           real(xdp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_xtfsm

     pure module subroutine stdlib_qtfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           real(qp), intent(in) :: alpha
           real(qp), intent(in) :: a(0_ilp:*)
           real(qp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_qtfsm


     pure module subroutine stdlib_ctfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           complex(sp), intent(in) :: alpha
           complex(sp), intent(in) :: a(0_ilp:*)
           complex(sp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_ctfsm

     pure module subroutine stdlib_ztfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           complex(dp), intent(in) :: alpha
           complex(dp), intent(in) :: a(0_ilp:*)
           complex(dp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_ztfsm

     pure module subroutine stdlib_ytfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           complex(xdp), intent(in) :: alpha
           complex(xdp), intent(in) :: a(0_ilp:*)
           complex(xdp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_ytfsm

     pure module subroutine stdlib_wtfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           complex(qp), intent(in) :: alpha
           complex(qp), intent(in) :: a(0_ilp:*)
           complex(qp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_wtfsm


end interface 


interface 
     pure module subroutine stdlib_ssfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: c(*)
     end subroutine stdlib_ssfrk

     pure module subroutine stdlib_dsfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: c(*)
     end subroutine stdlib_dsfrk

     pure module subroutine stdlib_xsfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(xdp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           real(xdp), intent(in) :: a(lda,*)
           real(xdp), intent(inout) :: c(*)
     end subroutine stdlib_xsfrk

     pure module subroutine stdlib_qsfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(qp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           real(qp), intent(in) :: a(lda,*)
           real(qp), intent(inout) :: c(*)
     end subroutine stdlib_qsfrk


end interface 


interface 
     real(sp) module function stdlib_slange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slange

     real(dp) module function stdlib_dlange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlange

     real(xdp) module function stdlib_xlange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(xdp), intent(in) :: a(lda,*)
           real(xdp), intent(out) :: work(*)
     end function stdlib_xlange

     real(qp) module function stdlib_qlange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(qp), intent(in) :: a(lda,*)
           real(qp), intent(out) :: work(*)
     end function stdlib_qlange


     real(sp) module function stdlib_clange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clange

     real(dp) module function stdlib_zlange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlange

     real(xdp) module function stdlib_ylange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: a(lda,*)
     end function stdlib_ylange

     real(qp) module function stdlib_wlange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: a(lda,*)
     end function stdlib_wlange


end interface 


interface 
     real(sp) module function stdlib_slangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slangb

     real(dp) module function stdlib_dlangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlangb

     real(xdp) module function stdlib_xlangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(xdp), intent(in) :: ab(ldab,*)
           real(xdp), intent(out) :: work(*)
     end function stdlib_xlangb

     real(qp) module function stdlib_qlangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(qp), intent(in) :: ab(ldab,*)
           real(qp), intent(out) :: work(*)
     end function stdlib_qlangb


     real(sp) module function stdlib_clangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
     end function stdlib_clangb

     real(dp) module function stdlib_zlangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
     end function stdlib_zlangb

     real(xdp) module function stdlib_ylangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: ab(ldab,*)
     end function stdlib_ylangb

     real(qp) module function stdlib_wlangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: ab(ldab,*)
     end function stdlib_wlangb


end interface 


interface 
     pure real(sp) module function stdlib_slangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_slangt

     pure real(dp) module function stdlib_dlangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_dlangt

     pure real(xdp) module function stdlib_xlangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(xdp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_xlangt

     pure real(qp) module function stdlib_qlangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(qp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_qlangt


     pure real(sp) module function stdlib_clangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           complex(sp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_clangt

     pure real(dp) module function stdlib_zlangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           complex(dp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_zlangt

     pure real(xdp) module function stdlib_ylangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           complex(xdp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_ylangt

     pure real(qp) module function stdlib_wlangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           complex(qp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_wlangt


end interface 


interface 
     real(sp) module function stdlib_slanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slanhs

     real(dp) module function stdlib_dlanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlanhs

     real(xdp) module function stdlib_xlanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(xdp), intent(in) :: a(lda,*)
           real(xdp), intent(out) :: work(*)
     end function stdlib_xlanhs

     real(qp) module function stdlib_qlanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(qp), intent(in) :: a(lda,*)
           real(qp), intent(out) :: work(*)
     end function stdlib_qlanhs


     real(sp) module function stdlib_clanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clanhs

     real(dp) module function stdlib_zlanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlanhs

     real(xdp) module function stdlib_ylanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: a(lda,*)
     end function stdlib_ylanhs

     real(qp) module function stdlib_wlanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: a(lda,*)
     end function stdlib_wlanhs


end interface 


interface 
     real(sp) module function stdlib_clanhf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: work(0_ilp:*)
           complex(sp), intent(in) :: a(0_ilp:*)
     end function stdlib_clanhf

     real(dp) module function stdlib_zlanhf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: work(0_ilp:*)
           complex(dp), intent(in) :: a(0_ilp:*)
     end function stdlib_zlanhf

     real(xdp) module function stdlib_ylanhf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(xdp), intent(out) :: work(0_ilp:*)
           complex(xdp), intent(in) :: a(0_ilp:*)
     end function stdlib_ylanhf

     real(qp) module function stdlib_wlanhf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(qp), intent(out) :: work(0_ilp:*)
           complex(qp), intent(in) :: a(0_ilp:*)
     end function stdlib_wlanhf


end interface 


interface 
     real(sp) module function stdlib_slansf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: a(0_ilp:*)
           real(sp), intent(out) :: work(0_ilp:*)
     end function stdlib_slansf

     real(dp) module function stdlib_dlansf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: a(0_ilp:*)
           real(dp), intent(out) :: work(0_ilp:*)
     end function stdlib_dlansf

     real(xdp) module function stdlib_xlansf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(xdp), intent(in) :: a(0_ilp:*)
           real(xdp), intent(out) :: work(0_ilp:*)
     end function stdlib_xlansf

     real(qp) module function stdlib_qlansf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(qp), intent(in) :: a(0_ilp:*)
           real(qp), intent(out) :: work(0_ilp:*)
     end function stdlib_qlansf


end interface 


interface 
     real(sp) module function stdlib_clanhp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ap(*)
     end function stdlib_clanhp

     real(dp) module function stdlib_zlanhp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ap(*)
     end function stdlib_zlanhp

     real(xdp) module function stdlib_ylanhp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: ap(*)
     end function stdlib_ylanhp

     real(qp) module function stdlib_wlanhp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: ap(*)
     end function stdlib_wlanhp


end interface 


interface 
     real(sp) module function stdlib_slansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slansp

     real(dp) module function stdlib_dlansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlansp

     real(xdp) module function stdlib_xlansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(xdp), intent(in) :: ap(*)
           real(xdp), intent(out) :: work(*)
     end function stdlib_xlansp

     real(qp) module function stdlib_qlansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(qp), intent(in) :: ap(*)
           real(qp), intent(out) :: work(*)
     end function stdlib_qlansp


     real(sp) module function stdlib_clansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ap(*)
     end function stdlib_clansp

     real(dp) module function stdlib_zlansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ap(*)
     end function stdlib_zlansp

     real(xdp) module function stdlib_ylansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: ap(*)
     end function stdlib_ylansp

     real(qp) module function stdlib_wlansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: ap(*)
     end function stdlib_wlansp


end interface 


interface 
     real(sp) module function stdlib_clanhb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
     end function stdlib_clanhb

     real(dp) module function stdlib_zlanhb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
     end function stdlib_zlanhb

     real(xdp) module function stdlib_ylanhb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: ab(ldab,*)
     end function stdlib_ylanhb

     real(qp) module function stdlib_wlanhb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: ab(ldab,*)
     end function stdlib_wlanhb


end interface 


interface 
     real(sp) module function stdlib_slansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slansb

     real(dp) module function stdlib_dlansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlansb

     real(xdp) module function stdlib_xlansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(xdp), intent(in) :: ab(ldab,*)
           real(xdp), intent(out) :: work(*)
     end function stdlib_xlansb

     real(qp) module function stdlib_qlansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(qp), intent(in) :: ab(ldab,*)
           real(qp), intent(out) :: work(*)
     end function stdlib_qlansb


     real(sp) module function stdlib_clansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
     end function stdlib_clansb

     real(dp) module function stdlib_zlansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
     end function stdlib_zlansb

     real(xdp) module function stdlib_ylansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: ab(ldab,*)
     end function stdlib_ylansb

     real(qp) module function stdlib_wlansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: ab(ldab,*)
     end function stdlib_wlansb


end interface 


interface 
     pure real(sp) module function stdlib_clanht( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: d(*)
           complex(sp), intent(in) :: e(*)
     end function stdlib_clanht

     pure real(dp) module function stdlib_zlanht( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: d(*)
           complex(dp), intent(in) :: e(*)
     end function stdlib_zlanht

     pure real(xdp) module function stdlib_ylanht( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(xdp), intent(in) :: d(*)
           complex(xdp), intent(in) :: e(*)
     end function stdlib_ylanht

     pure real(qp) module function stdlib_wlanht( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(qp), intent(in) :: d(*)
           complex(qp), intent(in) :: e(*)
     end function stdlib_wlanht


end interface 


interface 
     pure real(sp) module function stdlib_slanst( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: d(*), e(*)
     end function stdlib_slanst

     pure real(dp) module function stdlib_dlanst( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: d(*), e(*)
     end function stdlib_dlanst

     pure real(xdp) module function stdlib_xlanst( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(xdp), intent(in) :: d(*), e(*)
     end function stdlib_xlanst

     pure real(qp) module function stdlib_qlanst( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(qp), intent(in) :: d(*), e(*)
     end function stdlib_qlanst


end interface 


interface 
     real(sp) module function stdlib_slantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slantr

     real(dp) module function stdlib_dlantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlantr

     real(xdp) module function stdlib_xlantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(xdp), intent(in) :: a(lda,*)
           real(xdp), intent(out) :: work(*)
     end function stdlib_xlantr

     real(qp) module function stdlib_qlantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(qp), intent(in) :: a(lda,*)
           real(qp), intent(out) :: work(*)
     end function stdlib_qlantr


     real(sp) module function stdlib_clantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clantr

     real(dp) module function stdlib_zlantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlantr

     real(xdp) module function stdlib_ylantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: a(lda,*)
     end function stdlib_ylantr

     real(qp) module function stdlib_wlantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: a(lda,*)
     end function stdlib_wlantr


end interface 


interface 
     real(sp) module function stdlib_slantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slantp

     real(dp) module function stdlib_dlantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlantp

     real(xdp) module function stdlib_xlantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(xdp), intent(in) :: ap(*)
           real(xdp), intent(out) :: work(*)
     end function stdlib_xlantp

     real(qp) module function stdlib_qlantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(qp), intent(in) :: ap(*)
           real(qp), intent(out) :: work(*)
     end function stdlib_qlantp


     real(sp) module function stdlib_clantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ap(*)
     end function stdlib_clantp

     real(dp) module function stdlib_zlantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ap(*)
     end function stdlib_zlantp

     real(xdp) module function stdlib_ylantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: ap(*)
     end function stdlib_ylantp

     real(qp) module function stdlib_wlantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: ap(*)
     end function stdlib_wlantp


end interface 


interface 
     real(sp) module function stdlib_slantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slantb

     real(dp) module function stdlib_dlantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlantb

     real(xdp) module function stdlib_xlantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(xdp), intent(in) :: ab(ldab,*)
           real(xdp), intent(out) :: work(*)
     end function stdlib_xlantb

     real(qp) module function stdlib_qlantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(qp), intent(in) :: ab(ldab,*)
           real(qp), intent(out) :: work(*)
     end function stdlib_qlantb


     real(sp) module function stdlib_clantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
     end function stdlib_clantb

     real(dp) module function stdlib_zlantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
     end function stdlib_zlantb

     real(xdp) module function stdlib_ylantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: ab(ldab,*)
     end function stdlib_ylantb

     real(qp) module function stdlib_wlantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: ab(ldab,*)
     end function stdlib_wlantb


end interface 


interface 
     real(sp) module function stdlib_slansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slansy

     real(dp) module function stdlib_dlansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlansy

     real(xdp) module function stdlib_xlansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(xdp), intent(in) :: a(lda,*)
           real(xdp), intent(out) :: work(*)
     end function stdlib_xlansy

     real(qp) module function stdlib_qlansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(qp), intent(in) :: a(lda,*)
           real(qp), intent(out) :: work(*)
     end function stdlib_qlansy


     real(sp) module function stdlib_clansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clansy

     real(dp) module function stdlib_zlansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlansy

     real(xdp) module function stdlib_ylansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: a(lda,*)
     end function stdlib_ylansy

     real(qp) module function stdlib_wlansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: a(lda,*)
     end function stdlib_wlansy


end interface 


interface 
     real(sp) module function stdlib_clanhe( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clanhe

     real(dp) module function stdlib_zlanhe( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlanhe

     real(xdp) module function stdlib_ylanhe( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(xdp), intent(out) :: work(*)
           complex(xdp), intent(in) :: a(lda,*)
     end function stdlib_ylanhe

     real(qp) module function stdlib_wlanhe( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(qp), intent(out) :: work(*)
           complex(qp), intent(in) :: a(lda,*)
     end function stdlib_wlanhe


end interface 


interface 
     pure module subroutine stdlib_slartg( f, g, c, s, r )
        real(sp), intent(out) :: c, r, s
        real(sp), intent(in) :: f, g
     end subroutine stdlib_slartg

     pure module subroutine stdlib_dlartg( f, g, c, s, r )
        real(dp), intent(out) :: c, r, s
        real(dp), intent(in) :: f, g
     end subroutine stdlib_dlartg

     pure module subroutine stdlib_xlartg( f, g, c, s, r )
        real(xdp), intent(out) :: c, r, s
        real(xdp), intent(in) :: f, g
     end subroutine stdlib_xlartg

     pure module subroutine stdlib_qlartg( f, g, c, s, r )
        real(qp), intent(out) :: c, r, s
        real(qp), intent(in) :: f, g
     end subroutine stdlib_qlartg


     pure module subroutine stdlib_clartg( f, g, c, s, r )
        real(sp), intent(out) :: c
        complex(sp), intent(in) :: f, g
        complex(sp), intent(out) :: r, s
     end subroutine stdlib_clartg

     pure module subroutine stdlib_zlartg( f, g, c, s, r )
        real(dp), intent(out) :: c
        complex(dp), intent(in) :: f, g
        complex(dp), intent(out) :: r, s
     end subroutine stdlib_zlartg

     pure module subroutine stdlib_ylartg( f, g, c, s, r )
        real(xdp), intent(out) :: c
        complex(xdp), intent(in) :: f, g
        complex(xdp), intent(out) :: r, s
     end subroutine stdlib_ylartg

     pure module subroutine stdlib_wlartg( f, g, c, s, r )
        real(qp), intent(out) :: c
        complex(qp), intent(in) :: f, g
        complex(qp), intent(out) :: r, s
     end subroutine stdlib_wlartg


end interface 


interface 
     pure module subroutine stdlib_slartgp( f, g, cs, sn, r )
           real(sp), intent(out) :: cs, r, sn
           real(sp), intent(in) :: f, g
     end subroutine stdlib_slartgp

     pure module subroutine stdlib_dlartgp( f, g, cs, sn, r )
           real(dp), intent(out) :: cs, r, sn
           real(dp), intent(in) :: f, g
     end subroutine stdlib_dlartgp

     pure module subroutine stdlib_xlartgp( f, g, cs, sn, r )
           real(xdp), intent(out) :: cs, r, sn
           real(xdp), intent(in) :: f, g
     end subroutine stdlib_xlartgp

     pure module subroutine stdlib_qlartgp( f, g, cs, sn, r )
           real(qp), intent(out) :: cs, r, sn
           real(qp), intent(in) :: f, g
     end subroutine stdlib_qlartgp


end interface 


interface 
     pure module subroutine stdlib_slasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: c(*), s(*)
     end subroutine stdlib_slasr

     pure module subroutine stdlib_dlasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: c(*), s(*)
     end subroutine stdlib_dlasr

     pure module subroutine stdlib_xlasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(xdp), intent(inout) :: a(lda,*)
           real(xdp), intent(in) :: c(*), s(*)
     end subroutine stdlib_xlasr

     pure module subroutine stdlib_qlasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(qp), intent(inout) :: a(lda,*)
           real(qp), intent(in) :: c(*), s(*)
     end subroutine stdlib_qlasr


     pure module subroutine stdlib_clasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: c(*), s(*)
           complex(sp), intent(inout) :: a(lda,*)
     end subroutine stdlib_clasr

     pure module subroutine stdlib_zlasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: c(*), s(*)
           complex(dp), intent(inout) :: a(lda,*)
     end subroutine stdlib_zlasr

     pure module subroutine stdlib_ylasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(xdp), intent(in) :: c(*), s(*)
           complex(xdp), intent(inout) :: a(lda,*)
     end subroutine stdlib_ylasr

     pure module subroutine stdlib_wlasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(qp), intent(in) :: c(*), s(*)
           complex(qp), intent(inout) :: a(lda,*)
     end subroutine stdlib_wlasr


end interface 


interface 
     pure module subroutine stdlib_slargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(sp), intent(out) :: c(*)
           real(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_slargv

     pure module subroutine stdlib_dlargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(dp), intent(out) :: c(*)
           real(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_dlargv

     pure module subroutine stdlib_xlargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(xdp), intent(out) :: c(*)
           real(xdp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_xlargv

     pure module subroutine stdlib_qlargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(qp), intent(out) :: c(*)
           real(qp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_qlargv


     pure module subroutine stdlib_clargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(sp), intent(out) :: c(*)
           complex(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_clargv

     pure module subroutine stdlib_zlargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(dp), intent(out) :: c(*)
           complex(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_zlargv

     pure module subroutine stdlib_ylargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(xdp), intent(out) :: c(*)
           complex(xdp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_ylargv

     pure module subroutine stdlib_wlargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(qp), intent(out) :: c(*)
           complex(qp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_wlargv


end interface 


interface 
     pure module subroutine stdlib_slartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(sp), intent(in) :: c(*), s(*)
           real(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_slartv

     pure module subroutine stdlib_dlartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(dp), intent(in) :: c(*), s(*)
           real(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_dlartv

     pure module subroutine stdlib_xlartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(xdp), intent(in) :: c(*), s(*)
           real(xdp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_xlartv

     pure module subroutine stdlib_qlartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(qp), intent(in) :: c(*), s(*)
           real(qp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_qlartv


     pure module subroutine stdlib_clartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(sp), intent(in) :: c(*)
           complex(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_clartv

     pure module subroutine stdlib_zlartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(dp), intent(in) :: c(*)
           complex(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_zlartv

     pure module subroutine stdlib_ylartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(xdp), intent(in) :: c(*)
           complex(xdp), intent(in) :: s(*)
           complex(xdp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_ylartv

     pure module subroutine stdlib_wlartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(qp), intent(in) :: c(*)
           complex(qp), intent(in) :: s(*)
           complex(qp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_wlartv


end interface 


interface 
     pure module subroutine stdlib_slar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(sp), intent(in) :: c(*), s(*)
           real(sp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_slar2v

     pure module subroutine stdlib_dlar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(dp), intent(in) :: c(*), s(*)
           real(dp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_dlar2v

     pure module subroutine stdlib_xlar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(xdp), intent(in) :: c(*), s(*)
           real(xdp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_xlar2v

     pure module subroutine stdlib_qlar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(qp), intent(in) :: c(*), s(*)
           real(qp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_qlar2v


     pure module subroutine stdlib_clar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(sp), intent(in) :: c(*)
           complex(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_clar2v

     pure module subroutine stdlib_zlar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(dp), intent(in) :: c(*)
           complex(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_zlar2v

     pure module subroutine stdlib_ylar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(xdp), intent(in) :: c(*)
           complex(xdp), intent(in) :: s(*)
           complex(xdp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_ylar2v

     pure module subroutine stdlib_wlar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(qp), intent(in) :: c(*)
           complex(qp), intent(in) :: s(*)
           complex(qp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_wlar2v


end interface 


interface 
     pure module subroutine stdlib_clacrt( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: c, s
           complex(sp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_clacrt

     pure module subroutine stdlib_zlacrt( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: c, s
           complex(dp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_zlacrt

     pure module subroutine stdlib_ylacrt( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           complex(xdp), intent(in) :: c, s
           complex(xdp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_ylacrt

     pure module subroutine stdlib_wlacrt( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           complex(qp), intent(in) :: c, s
           complex(qp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_wlacrt


end interface 


interface 
     pure module subroutine stdlib_slarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           real(sp), intent(in) :: tau
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slarf

     pure module subroutine stdlib_dlarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           real(dp), intent(in) :: tau
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlarf

     pure module subroutine stdlib_xlarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           real(xdp), intent(in) :: tau
           real(xdp), intent(inout) :: c(ldc,*)
           real(xdp), intent(in) :: v(*)
           real(xdp), intent(out) :: work(*)
     end subroutine stdlib_xlarf

     pure module subroutine stdlib_qlarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           real(qp), intent(in) :: tau
           real(qp), intent(inout) :: c(ldc,*)
           real(qp), intent(in) :: v(*)
           real(qp), intent(out) :: work(*)
     end subroutine stdlib_qlarf


     pure module subroutine stdlib_clarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           complex(sp), intent(in) :: tau
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_clarf

     pure module subroutine stdlib_zlarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           complex(dp), intent(in) :: tau
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zlarf

     pure module subroutine stdlib_ylarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           complex(xdp), intent(in) :: tau
           complex(xdp), intent(inout) :: c(ldc,*)
           complex(xdp), intent(in) :: v(*)
           complex(xdp), intent(out) :: work(*)
     end subroutine stdlib_ylarf

     pure module subroutine stdlib_wlarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           complex(qp), intent(in) :: tau
           complex(qp), intent(inout) :: c(ldc,*)
           complex(qp), intent(in) :: v(*)
           complex(qp), intent(out) :: work(*)
     end subroutine stdlib_wlarf


end interface 


interface 
     pure module subroutine stdlib_slarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           real(sp), intent(in) :: tau
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slarfx

     pure module subroutine stdlib_dlarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           real(dp), intent(in) :: tau
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlarfx

     pure module subroutine stdlib_xlarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           real(xdp), intent(in) :: tau
           real(xdp), intent(inout) :: c(ldc,*)
           real(xdp), intent(in) :: v(*)
           real(xdp), intent(out) :: work(*)
     end subroutine stdlib_xlarfx

     pure module subroutine stdlib_qlarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           real(qp), intent(in) :: tau
           real(qp), intent(inout) :: c(ldc,*)
           real(qp), intent(in) :: v(*)
           real(qp), intent(out) :: work(*)
     end subroutine stdlib_qlarfx


     pure module subroutine stdlib_clarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           complex(sp), intent(in) :: tau
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_clarfx

     pure module subroutine stdlib_zlarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           complex(dp), intent(in) :: tau
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zlarfx

     pure module subroutine stdlib_ylarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           complex(xdp), intent(in) :: tau
           complex(xdp), intent(inout) :: c(ldc,*)
           complex(xdp), intent(in) :: v(*)
           complex(xdp), intent(out) :: work(*)
     end subroutine stdlib_ylarfx

     pure module subroutine stdlib_wlarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           complex(qp), intent(in) :: tau
           complex(qp), intent(inout) :: c(ldc,*)
           complex(qp), intent(in) :: v(*)
           complex(qp), intent(out) :: work(*)
     end subroutine stdlib_wlarfx


end interface 


interface 
     pure module subroutine stdlib_slarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           real(sp), intent(in) :: tau
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slarfy

     pure module subroutine stdlib_dlarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           real(dp), intent(in) :: tau
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlarfy

     pure module subroutine stdlib_xlarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           real(xdp), intent(in) :: tau
           real(xdp), intent(inout) :: c(ldc,*)
           real(xdp), intent(in) :: v(*)
           real(xdp), intent(out) :: work(*)
     end subroutine stdlib_xlarfy

     pure module subroutine stdlib_qlarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           real(qp), intent(in) :: tau
           real(qp), intent(inout) :: c(ldc,*)
           real(qp), intent(in) :: v(*)
           real(qp), intent(out) :: work(*)
     end subroutine stdlib_qlarfy


     pure module subroutine stdlib_clarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           complex(sp), intent(in) :: tau
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_clarfy

     pure module subroutine stdlib_zlarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           complex(dp), intent(in) :: tau
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zlarfy

     pure module subroutine stdlib_ylarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           complex(xdp), intent(in) :: tau
           complex(xdp), intent(inout) :: c(ldc,*)
           complex(xdp), intent(in) :: v(*)
           complex(xdp), intent(out) :: work(*)
     end subroutine stdlib_ylarfy

     pure module subroutine stdlib_wlarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           complex(qp), intent(in) :: tau
           complex(qp), intent(inout) :: c(ldc,*)
           complex(qp), intent(in) :: v(*)
           complex(qp), intent(out) :: work(*)
     end subroutine stdlib_wlarfy


end interface 


interface 
     pure module subroutine stdlib_slarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_slarfb

     pure module subroutine stdlib_dlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_dlarfb

     pure module subroutine stdlib_xlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           real(xdp), intent(inout) :: c(ldc,*)
           real(xdp), intent(in) :: t(ldt,*), v(ldv,*)
           real(xdp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_xlarfb

     pure module subroutine stdlib_qlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           real(qp), intent(inout) :: c(ldc,*)
           real(qp), intent(in) :: t(ldt,*), v(ldv,*)
           real(qp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_qlarfb


     pure module subroutine stdlib_clarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     work, ldwork )
               
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_clarfb

     pure module subroutine stdlib_zlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_zlarfb

     pure module subroutine stdlib_ylarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           complex(xdp), intent(inout) :: c(ldc,*)
           complex(xdp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(xdp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_ylarfb

     pure module subroutine stdlib_wlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           complex(qp), intent(inout) :: c(ldc,*)
           complex(qp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(qp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_wlarfb


end interface 


interface 
     pure module subroutine stdlib_slarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(inout) :: alpha
           real(sp), intent(out) :: tau
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_slarfg

     pure module subroutine stdlib_dlarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(inout) :: alpha
           real(dp), intent(out) :: tau
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dlarfg

     pure module subroutine stdlib_xlarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(xdp), intent(inout) :: alpha
           real(xdp), intent(out) :: tau
           real(xdp), intent(inout) :: x(*)
     end subroutine stdlib_xlarfg

     pure module subroutine stdlib_qlarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(qp), intent(inout) :: alpha
           real(qp), intent(out) :: tau
           real(qp), intent(inout) :: x(*)
     end subroutine stdlib_qlarfg


     pure module subroutine stdlib_clarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: alpha
           complex(sp), intent(out) :: tau
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_clarfg

     pure module subroutine stdlib_zlarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: alpha
           complex(dp), intent(out) :: tau
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_zlarfg

     pure module subroutine stdlib_ylarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(xdp), intent(inout) :: alpha
           complex(xdp), intent(out) :: tau
           complex(xdp), intent(inout) :: x(*)
     end subroutine stdlib_ylarfg

     pure module subroutine stdlib_wlarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(qp), intent(inout) :: alpha
           complex(qp), intent(out) :: tau
           complex(qp), intent(inout) :: x(*)
     end subroutine stdlib_wlarfg


end interface 


interface 
     module subroutine stdlib_slarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(inout) :: alpha
           real(sp), intent(out) :: tau
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_slarfgp

     module subroutine stdlib_dlarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(inout) :: alpha
           real(dp), intent(out) :: tau
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dlarfgp

     module subroutine stdlib_xlarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(xdp), intent(inout) :: alpha
           real(xdp), intent(out) :: tau
           real(xdp), intent(inout) :: x(*)
     end subroutine stdlib_xlarfgp

     module subroutine stdlib_qlarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(qp), intent(inout) :: alpha
           real(qp), intent(out) :: tau
           real(qp), intent(inout) :: x(*)
     end subroutine stdlib_qlarfgp


     module subroutine stdlib_clarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: alpha
           complex(sp), intent(out) :: tau
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_clarfgp

     module subroutine stdlib_zlarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: alpha
           complex(dp), intent(out) :: tau
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_zlarfgp

     module subroutine stdlib_ylarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(xdp), intent(inout) :: alpha
           complex(xdp), intent(out) :: tau
           complex(xdp), intent(inout) :: x(*)
     end subroutine stdlib_ylarfgp

     module subroutine stdlib_wlarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(qp), intent(inout) :: alpha
           complex(qp), intent(out) :: tau
           complex(qp), intent(inout) :: x(*)
     end subroutine stdlib_wlarfgp


end interface 


interface 
     pure module subroutine stdlib_slarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           real(sp), intent(out) :: t(ldt,*)
           real(sp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_slarft

     pure module subroutine stdlib_dlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           real(dp), intent(out) :: t(ldt,*)
           real(dp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_dlarft

     pure module subroutine stdlib_xlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           real(xdp), intent(out) :: t(ldt,*)
           real(xdp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_xlarft

     pure module subroutine stdlib_qlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           real(qp), intent(out) :: t(ldt,*)
           real(qp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_qlarft


     pure module subroutine stdlib_clarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           complex(sp), intent(out) :: t(ldt,*)
           complex(sp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_clarft

     pure module subroutine stdlib_zlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           complex(dp), intent(out) :: t(ldt,*)
           complex(dp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_zlarft

     pure module subroutine stdlib_ylarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           complex(xdp), intent(out) :: t(ldt,*)
           complex(xdp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_ylarft

     pure module subroutine stdlib_wlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           complex(qp), intent(out) :: t(ldt,*)
           complex(qp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_wlarft


end interface 

end module stdlib_lapack_base
