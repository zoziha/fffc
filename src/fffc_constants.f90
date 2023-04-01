module fffc_constants
    use fffc_kinds
    real(kind=fffc_real_kind), parameter :: pi = acos(-1.0_fffc_real_kind)
    real(kind=fffc_real_kind), parameter :: g = 9.80665_fffc_real_kind
    real(kind=fffc_real_kind), parameter :: rad2deg = 180.0_fffc_real_kind/pi
    real(kind=fffc_real_kind), parameter :: deg2rad = pi/180.0_fffc_real_kind
end module fffc_constants
