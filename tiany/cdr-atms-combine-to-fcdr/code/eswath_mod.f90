! eswath_mod.f90
module eswath_mod
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, real32, real64
  use sys_time
  use in_params
  use geo_time
  use constants 
  use ama1b
  implicit none
  private
  public :: &
       year, doy, month, dom, hour, minute, second, time_tai93, str_scantime, &
       mask, qc_flag, orb_mode, qa_prod, scanneed, &
       lza, sza, lat_a1_1, lon_a1_1, lza_a1_1, ssa_a1_1, lat_a1_2, lon_a1_2, lza_a1_2, ssa_a1_2, &
       lat_a2, lon_a2, lza_a2, ssa_a2, &
       stype_a1_1, stype_a1_2, stype_a2, &
       t_r_central_wave_number, rfs_corr, mu_coef, mu15, t_cold, rad_cold, cc, &
       dcoef_a1, dcoef_a2, scoef_a1, scoef_a2, twd_a1, twd_a2, t_rfs_a1, t_rfs_a2, twd_weight, &
       dtw, t_warm, rad_warm, cw, S, Z, Rl, &
       count, countc, countw, clcoef, calrad, calradc, calradw, calrade, at, rat, rad, ssmi_r, &
       clw, tpw, sice, ts, em1, em2, em3, rr, snow, &
       num_avn, avnhr, avnset, windu_avn, windv_avn, ts2m_avn, ts_avn, tpw_avn, &
       apdata, eta, &
       latdata, londata, eiadata, &
       FN_length_AMA, bad_at, i_ssmi, j_ssmi, latbox_up, latbox_down, &
       A0, A1, A2, A3, A4, A5, D0, D1, D2, D3, D4, D5, &
       sid, snum, asym_a2, asym_a1, asym_a0, mu, dr, k, &
       nc_fname, platform, hdf_fname, &
       scanline, Sys_times, limit_A, hblock, geo_scantime, geohead, geoline

  !========================
  ! Time
  !========================
  integer(int16)          :: year     (MAXSCANLINE_A)
  integer(int16)          :: doy      (MAXSCANLINE_A)
  character(len=1)        :: month    (MAXSCANLINE_A)
  character(len=1)        :: dom      (MAXSCANLINE_A)
  character(len=1)        :: hour     (MAXSCANLINE_A)
  character(len=1)        :: minute   (MAXSCANLINE_A)
  character(len=1)        :: second   (MAXSCANLINE_A)
  real(real64)            :: time_tai93 (MAXSCANLINE_A)
  character(len=NCHAR)    :: str_scantime (MAXSCANLINE_A)

  !========================
  ! Ancillary Data
  !========================
  integer(int8)           :: mask     (MAP_ROWS, MAP_COLS)
  integer(int8)           :: qc_flag  (MAXSCANLINE_A)
  integer(int8)           :: orb_mode (MAXSCANLINE_A)
  integer(int32)          :: qa_prod  (MAXSCANLINE_A, NUMCHAN_A)  ! was unsigned char
  integer(int16)          :: scanneed (MAXSCANLINE_A)

  real(real32)            :: lza      (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: sza      (MAXSCANLINE_A, NUMSPOT_A)

  real(real32)            :: lat_a1_1 (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: lon_a1_1 (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: lza_a1_1 (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: ssa_a1_1 (MAXSCANLINE_A, NUMSPOT_A)

  real(real32)            :: lat_a1_2 (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: lon_a1_2 (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: lza_a1_2 (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: ssa_a1_2 (MAXSCANLINE_A, NUMSPOT_A)

  real(real32)            :: lat_a2   (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: lon_a2   (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: lza_a2   (MAXSCANLINE_A, NUMSPOT_A)
  real(real32)            :: ssa_a2   (MAXSCANLINE_A, NUMSPOT_A)

  ! C had: char** stype_* ; commented fixed arrays exist in header.
  ! Practical Fortran mapping as 2D array of 1-char codes:
  integer(1)        :: stype_a1_1 (MAXSCANLINE_A, NUMSPOT_A)
  integer(1)        :: stype_a1_2 (MAXSCANLINE_A, NUMSPOT_A)
  integer(1)        :: stype_a2   (MAXSCANLINE_A, NUMSPOT_A)

  real(real32)            :: t_r_central_wave_number (NUMCHAN_A)
  real(real32)            :: rfs_corr (NUMCHAN_A, 3)
  real(real32)            :: mu_coef  (3)
  real(real32)            :: mu15     (MAXSCANLINE_A)
  real(real32)            :: t_cold   (NUMCHAN_A)
  real(real32)            :: rad_cold (NUMCHAN_A)
  real(real32)            :: cc       (MAXSCANLINE_A, NUMCHAN_A)

  real(real32)            :: dcoef_a1 (10, 4)
  real(real32)            :: dcoef_a2 (7,  4)
  real(real32)            :: scoef_a1 (2,  4)
  real(real32)            :: scoef_a2 (4)

  real(real32)            :: twd_a1   (MAXSCANLINE_A, 10)
  real(real32)            :: twd_a2   (MAXSCANLINE_A, 7)
  real(real32)            :: t_rfs_a1 (MAXSCANLINE_A, 2)
  real(real32)            :: t_rfs_a2 (MAXSCANLINE_A)
  real(real32)            :: twd_weight (MAXSCANLINE_A, 3)

  real(real32)            :: dtw      (MAXSCANLINE_A, NUMCHAN_A)
  real(real32)            :: t_warm   (MAXSCANLINE_A, NUMCHAN_A)
  real(real32)            :: rad_warm (MAXSCANLINE_A, NUMCHAN_A)
  real(real32)            :: cw       (MAXSCANLINE_A, NUMCHAN_A)
  real(real32)            :: S        (MAXSCANLINE_A, NUMCHAN_A)
  real(real32)            :: Z        (MAXSCANLINE_A, NUMSPOT_A, NUMCHAN_A)
  real(real32)            :: Rl       (MAXSCANLINE_A, NUMSPOT_A, NUMCHAN_A)

  !========================
  ! Antenna Temperature etc.
  !========================
  integer(int32)          :: count    (NUMCHAN_A, NUMSPOT_A)   ! was unsigned short
  integer(int32)          :: countc   (NUMCHAN_A, 2)           ! was unsigned short
  integer(int32)          :: countw   (NUMCHAN_A, 2)           ! was unsigned short

  real(real32)            :: clcoef   (NUMCHAN_A, 3)
  real(real32)            :: calrad   (NUMCHAN_A, NUMSPOT_A)
  real(real32)            :: calradc  (NUMCHAN_A, 2)
  real(real32)            :: calradw  (NUMCHAN_A, 2)
  real(real32)            :: calrade  (NUMCHAN_A, NUMSPOT_A)

  real(real32)            :: at       (MAXSCANLINE_A, NUMSPOT_A, NUMCHAN_A)
  real(real32)            :: rat      (NUMSPOT_A)
  real(real32)            :: rad      (NUMSPOT_A)
  real(real32)            :: ssmi_r   (SSMI_LON, SSMI_LAT)

  !========================
  ! Products
  !========================
  integer(int16)          :: clw      (MAXSCANLINE_A, NUMSPOT_A)
  integer(int16)          :: tpw      (MAXSCANLINE_A, NUMSPOT_A)
  integer(int16)          :: sice     (MAXSCANLINE_A, NUMSPOT_A)
  integer(int16)          :: ts       (MAXSCANLINE_A, NUMSPOT_A)
  integer(int16)          :: em1      (MAXSCANLINE_A, NUMSPOT_A)
  integer(int16)          :: em2      (MAXSCANLINE_A, NUMSPOT_A)
  integer(int16)          :: em3      (MAXSCANLINE_A, NUMSPOT_A)
  integer(int16)          :: rr       (MAXSCANLINE_A, NUMSPOT_A)
  integer(int16)          :: snow     (MAXSCANLINE_A, NUMSPOT_A)

  !========================
  ! AVN data
  !========================
  integer(int16)          :: num_avn
  integer(int16)          :: avnhr    (3)
  integer(int16)          :: avnset   (3)
  real(real32)            :: windu_avn(3, NUMROW_AVN, NUMCOL_AVN)
  real(real32)            :: windv_avn(3, NUMROW_AVN, NUMCOL_AVN)
  real(real32)            :: ts2m_avn (3, NUMROW_AVN, NUMCOL_AVN)
  real(real32)            :: ts_avn   (3, NUMROW_AVN, NUMCOL_AVN)
  real(real32)            :: tpw_avn  (3, NUMROW_AVN, NUMCOL_AVN)

  !========================
  ! APC data
  !========================
  real(real32)            :: apdata   (NUMSPOT_A, NUMCHAN_A, 3)
  real(real32)            :: eta      (NUMCHAN_A)

  !========================
  ! GEO data
  !========================
  real(real64)            :: latdata  (NUMSPOT_A, 2, 3, 3)
  real(real64)            :: londata  (NUMSPOT_A, 2, 3, 3)
  real(real64)            :: eiadata  (NUMSPOT_A, 2, 3, 3)

  !========================
  ! Misc. Data
  !========================
  integer(int16)          :: FN_length_AMA
  integer(int16)          :: bad_at
  integer(int16)          :: i_ssmi, j_ssmi
  integer(int32)          :: latbox_up   (NUMSPOT_A)
  integer(int32)          :: latbox_down (NUMSPOT_A)

  real(real32)            :: A0(4), A1(4), A2(4), A3(4), A4(4), A5(4)
  real(real32)            :: D0(4), D1(4), D2(4), D3(4), D4(4), D5(4)

  character(len=4)        :: sid
  integer(int16)          :: snum

  real(real32)            :: asym_a2 (NUMSPOT_A, NUMCHAN_A)
  real(real32)            :: asym_a1 (NUMSPOT_A, NUMCHAN_A)
  real(real32)            :: asym_a0 (NUMSPOT_A, NUMCHAN_A)

  real(real32)            :: mu      (NUMCHAN_A)
  real(real32)            :: dr      (NUMCHAN_A)
  real(real32)            :: k       (NUMCHAN_A)

  character(len=1080)     :: nc_fname
  character(len=1080)     :: platform
  character(len=1080)     :: hdf_fname

  !========================
  ! Structures
  !========================
  type(SCANLINE_TYPE)          :: scanline    (MAXSCANLINE_A)
  type(Sys_Time_TYPE)          :: Sys_times
  type(Limit_A_TYPE)           :: limit_A
  type(HBLOCK_TYPE)            :: hblock
  type(GEO_STIME_TYPE)         :: geo_scantime(MAXSCANLINE_A)
  type(GEOHEAD_TYPE)           :: geohead
  type(GEOLINE_TYPE)           :: geoline

  save
end module eswath_mod
