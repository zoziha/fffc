<div align='center'>

# Fortran 免费函数集合

![FFFC](https://img.shields.io/badge/FFFC-v1.3.20230622-blueviolet)
![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)
[![license](https://img.shields.io/badge/License-BSD--3-brightgreen)](LICENSE)
![Actions Status](https://github.com/zoziha/fffc/workflows/msys2-build/badge.svg)

</div>

将本库代码编译为链接库，免费使用功能，如果觉得功能不好用，欢迎更新。

## 依赖

* BLAS (GFortran: OpenBLAS; IFort: OneMKL)。

## 使用 Meson 构建链接库

```sh
> meson setup _build -Dprefix=/mingw64
> meson install --destdir 'C:\msys64\' -C _build
> meson test -C _build func_api -v  # 单元测试
```

理论上，Meson 支持 Windows/macOS/Linux 下的 gfortran, macOS/Linux 下的 ifort。
若需要支持 Windows 下的 ifort, 可搭配 VS2022 进行编译。

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