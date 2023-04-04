---
title: Linear Algebra
---

[TOC]

# 线性代数

`fffc_linalg` 模块提供了常用的线性代数求解函数。

## `inv`

矩阵求逆。

### 语法

`call [[fffc_linalg(module):inv(interface)]](a)`

### 例子

```fortran
program demo_inv
    use fffc_module
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    call inv(x)
end program demo_inv
```

## `gemm`

矩阵相乘。

### 语法

`c = [[fffc_linalg(module):gemm(interface)]](a, b)`

### 例子

```fortran
program demo_gemm
    use fffc_module
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    real :: y(2, 1) = reshape([1, 1], [2, 1]), z(2, 1)
    z = gemm(x, y)
end program demo_gemm
```

## `solve`

线性方程组求解。

### 语法

`call [[fffc_linalg(module):solve(interface)]](a, b)`

### 例子

```fortran
program demo_solve
    use fffc_module
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    real :: y(2, 1) = reshape([1, 2], [2, 1])
    call solve(x, y)
end program demo_solve
```

## `diag`

对角矩阵与矩阵对角向量的相互转换。

### 语法

`b = [[fffc_linalg(module):diag(interface)]](a)`

### 例子

```fortran
program demo_diag
    use fffc_module
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    real :: v(2)
    v = diag(x)
end program demo_diag
```

## `det`

矩阵行列式值。

### 语法

`b = [[fffc_linalg(module):det(interface)]](a)`

### 例子

```fortran
program demo_det
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2]), v
    v = det(x)
end program demo_det
```

## `eye`

构造单位矩阵。

### 语法

`b = [[fffc_linalg(module):eye(interface)]](m, n)`

### 例子

```fortran
program demo_eye
    use fffc_module
    real :: x(2, 2)
    x = eye(2, 2)
end program demo_eye
```
