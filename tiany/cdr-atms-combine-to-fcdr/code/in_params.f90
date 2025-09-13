! in_params.f90
module in_params
  implicit none

  type :: Limit_A_TYPE
    real :: Temp_lower(4), Temp_upper(4)
    real :: RR_lower, RR_upper
    real :: TPW_lower, TPW_upper
    real :: CLW_lower, CLW_upper
    real :: SIce_lower, SIce_upper
    real :: OWS_lower, OWS_upper
    real :: STemp_lower, STemp_upper
    real :: SWet_lower, SWet_upper
    real :: SNowC_lower, SNowC_upper
    real :: Em23_lower, Em23_upper
    real :: Em31_lower, Em31_upper
    real :: Em50_lower, Em50_upper
    real :: Tsfc_lower, Tsfc_upper
  end type Limit_A_TYPE

end module in_params
