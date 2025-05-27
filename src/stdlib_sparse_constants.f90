module stdlib_sparse_constants
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp

    implicit none
    public

    enum, bind(C)
        enumerator :: sparse_full  !! Full Sparse matrix (no symmetry considerations)
        enumerator :: sparse_lower !! Symmetric Sparse matrix with triangular inferior storage
        enumerator :: sparse_upper !! Symmetric Sparse matrix with triangular supperior storage
    end enum

    character(1), parameter :: sparse_op_none = 'N' !! no transpose
    character(1), parameter :: sparse_op_transpose = 'T' !! transpose
    character(1), parameter :: sparse_op_hermitian = 'H' !! conjugate or hermitian transpose
    
    ! Integer size support for ILP64 builds should be done here
    integer, parameter :: ilp = int32

    real(sp), parameter :: zero_sp = 0._sp
    real(sp), parameter :: one_sp = 1._sp
    real(dp), parameter :: zero_dp = 0._dp
    real(dp), parameter :: one_dp = 1._dp
    real(qp), parameter :: zero_qp = 0._qp
    real(qp), parameter :: one_qp = 1._qp
    complex(sp), parameter :: zero_csp = (0._sp,0._sp)
    complex(sp), parameter :: one_csp = (1._sp,1._sp)
    complex(dp), parameter :: zero_cdp = (0._dp,0._dp)
    complex(dp), parameter :: one_cdp = (1._dp,1._dp)
    complex(qp), parameter :: zero_cqp = (0._qp,0._qp)
    complex(qp), parameter :: one_cqp = (1._qp,1._qp)

end module stdlib_sparse_constants
