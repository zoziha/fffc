# Fortran 免费函数集合

将本库代码编译为链接库，免费使用功能，如果觉得功能不好用，欢迎更新。

## 依赖

* OpenBLAS。

## 使用 Meson 构建链接库

```sh
> meson setup _build
> meson compile -C _build
```

## 使用 fpm 构建链接库

```sh
> fpm build
```

## 示例

```fortran
program main
    use fffc_module
    real :: a(2, 2) = reshape([1., 2., 3., 4.], shape(a)), b(2, 2)
    b = a
    call inv(b)
110 format(f8.3, ", ", f8.3, "; ")
111 format(a, t12, ' =')
    print 111, 'inv(a)'
    print 110, b(1, :)
    print 110, b(2, :)
    print 111, 'a * inv(a)'
    b = gemm(a, b)
    print 110, b(1, :)
    print 110, b(2, :)
end program main
!inv(a)      =
!  -2.000,    1.500;
!   1.000,   -0.500;
!a * inv(a)  =
!   1.000,    0.000;
!   0.000,    1.000;
```