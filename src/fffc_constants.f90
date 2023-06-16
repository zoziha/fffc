!> 常数
module fffc_constants

    use fffc_kinds, only: fffc_real_kind
    implicit none

    private
    public :: pi, g, rad2deg, deg2rad, sqrt_eps

    real(kind=fffc_real_kind), parameter :: pi = acos(-1.0_fffc_real_kind)  !! 圆周率
    real(kind=fffc_real_kind), parameter :: g = 9.80665_fffc_real_kind  !! 重力加速度, m/s^2
    real(kind=fffc_real_kind), parameter :: rad2deg = 180.0_fffc_real_kind/pi  !! 弧度转角度
    real(kind=fffc_real_kind), parameter :: deg2rad = pi/180.0_fffc_real_kind  !! 角度转弧度
    real(kind=fffc_real_kind), parameter :: sqrt_eps = sqrt(epsilon(1.0_fffc_real_kind))  !! 根号机器精度

end module fffc_constants
