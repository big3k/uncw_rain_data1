! ama1b.f90
module ama1b
  implicit none

  type :: HBLOCK_TYPE
    character(len=72) :: dummy0
    integer :: sat_id
    character(len=10) :: dummy1
    integer :: start_year, start_day_of_year
    integer :: start_milliseconds_of_day
    character(len=4) :: dummy2
    integer :: end_year, end_day_of_year
    integer :: end_milliseconds_of_day
    character(len=40) :: dummy3
    integer :: last_scan_record
    character(len=70) :: dummy4
    integer :: rfs(3,3)
    character(len=30) :: dummy41
    integer :: corr(15,4)
    character(len=36) :: dummy42
    integer :: mu(15,3)
    character(len=88) :: dummy43
    integer :: t_r_central_wave_number_1
    character(len=8) :: dummy5
    integer :: t_r_central_wave_number_2
    character(len=8) :: dummy6
    integer :: t_r_central_wave_number_3
    character(len=8) :: dummy7
    integer :: t_r_central_wave_number_4
    character(len=8) :: dummy8
    integer :: t_r_central_wave_number_5
    character(len=8) :: dummy9
    integer :: t_r_central_wave_number_6
    character(len=8) :: dummy10
    integer :: t_r_central_wave_number_7
    character(len=8) :: dummy11
    integer :: t_r_central_wave_number_8
    character(len=8) :: dummy12
    integer :: t_r_central_wave_number_9
    character(len=8) :: dummy13
    integer :: t_r_central_wave_number_10
    character(len=8) :: dummy14
    integer :: t_r_central_wave_number_11
    character(len=8) :: dummy15
    integer :: t_r_central_wave_number_12
    character(len=8) :: dummy16
    integer :: t_r_central_wave_number_13
    character(len=8) :: dummy17
    integer :: t_r_central_wave_number_14
    character(len=8) :: dummy18
    integer :: t_r_central_wave_number_15
    character(len=40) :: dummy19
    integer :: orbit_vector_epoch_year, orbit_vector_day_of_year
    integer :: orbit_vector_utc_time_of_day
    integer :: semimajor_axis, eccentricity, inclination
    integer :: argument_of_perigee, right_ascension, mean_anomaly
    integer :: posision_x, posision_y, posision_z
    integer :: velocity_x, velocity_y, velocity_z
    integer :: eart_sun_dist_ratio
    character(len=528) :: dummy20
    integer :: rfs_coeffs_a1(2,4)
    character(len=16) :: dummy21
    integer :: warm_digital_coeffs_a1(10,4)
    character(len=384) :: dummy22
    integer :: rfs_coeffs_a2(4)
    character(len=16) :: dummy23
    integer :: warm_digital_coeffs_a2(7,4)
    character(len=336) :: dummy24
  end type HBLOCK_TYPE

  type :: SCANLINE_TYPE
    character(len=8) :: dummy1
    integer :: time_of_day_of_scan
    integer :: orbit_node
    character(len=10) :: dummy2
    integer :: quality_indicator_bit_field
    character :: dummy3
    integer :: qa_time, qa_cal, qa_eloc
    integer :: qa_cal_chan(15)
    character(len=18) :: dummy31
    integer :: pri_cal_coeffs(15,3)
    character(len=212) :: dummy4
    integer :: angular_relationships(90)
    integer :: earth_location(60)
    character(len=12) :: dummy5
    integer :: scene_telemetry_a1(510)
    integer :: cold_cal_telemetry_a1(30)
    character(len=64) :: dummy61
    integer :: count_rfs_a1(2)
    character(len=2) :: dummy62
    integer :: countwd_a1(10)
    character(len=2) :: dummy63
    integer :: warm_cal_telemetry_a1(30)
    character(len=56) :: dummy6
    integer :: scene_telemetry_a2(120)
    integer :: cold_cal_telemetry_a2(6)
    character(len=20) :: dummy71
    integer :: count_rfs_a2
    character(len=2) :: dummy72
    integer :: countwd_a2(7)
    character(len=2) :: dummy73
    integer :: warm_cal_telemetry_a2(6)
    character(len=32) :: dummy7
    integer :: countc_corr(15)
    character(len=17) :: dummy74
  end type SCANLINE_TYPE

  type :: GEOHEAD_TYPE
    integer :: pitch, roll, yaw, nscan
  end type GEOHEAD_TYPE

  type :: GEOLINE_TYPE
    integer :: lat, lon, lza, ssa
  end type GEOLINE_TYPE

end module ama1b
