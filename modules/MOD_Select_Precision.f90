module MOD_Select_Precision
    implicit none

    !Define the precision for the real numbers
    !For single precision, use pv = selected_real_kind(6, 37)
    !For double precision, use pv = selected_real_kind(15, 307)
    !For quadruple precision, use pv = selected_real_kind(33, 4931)
    
    integer, parameter, public :: pv = selected_real_kind(15, 307)
    integer, parameter, public :: iv = selected_int_kind(9)

end module MOD_Select_Precision

