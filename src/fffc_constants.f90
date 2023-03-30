module fffc_constants
    use fffc_kinds
    real(kind=real_kind), parameter :: pi = acos(-1.0_real_kind)
    real(kind=real_kind), parameter :: g = 9.80665_real_kind
    real(kind=real_kind), parameter :: rad2deg = 180.0_real_kind/pi
    real(kind=real_kind), parameter :: deg2rad = pi/180.0_real_kind
end module fffc_constants
