---
title: Linear Algebra
---

[TOC]

# ���Դ���

`fffc_linalg` ģ���ṩ�˳��õ����Դ�����⺯����

## `inv`

�������档

### �﷨

`call [[fffc_linalg(module):inv(interface)]](a)`

@note
û��ʹ�� `b = inv(a)` ����ʽ�ǿ��ǵ���������ֵ�Ĳ��������û��Լ���

### ����

```fortran
program demo_inv
    use fffc_module
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    call inv(x)
end program demo_inv
```

## `gemm`

������ˡ�

### �﷨

`c = [[fffc_linalg(module):gemm(interface)]](a, b)`

### ����

```fortran
program demo_gemm
    use fffc_module
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    real :: y(2, 1) = reshape([1, 1], [2, 1]), z(2, 1)
    z = gemm(x, y)
end program demo_gemm
```

## `solve`

���Է�������⡣

### �﷨

`call [[fffc_linalg(module):solve(interface)]](a, b)`

@note
û��ʹ�� `b = solve(a, b)` ����ʽ�ǿ��ǵ���������ֵ�Ĳ��������û��Լ���

### ����

```fortran
program demo_solve
    use fffc_module
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    real :: y(2, 1) = reshape([1, 2], [2, 1])
    call solve(x, y)
end program demo_solve
```

## `diag`

�ԽǾ��������Խ��������໥ת����

### �﷨

`b = [[fffc_linalg(module):diag(interface)]](a)`

### ����

```fortran
program demo_diag
    use fffc_module
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    real :: v(2)
    v = diag(x)
end program demo_diag
```

## `det`

��������ʽֵ��

### �﷨

`b = [[fffc_linalg(module):det(interface)]](a)`

### ����

```fortran
program demo_det
    real :: x(2, 2) = reshape([1, 2, 3, 4], [2, 2]), v
    v = det(x)
end program demo_det
```

## `eye`

���쵥λ����

### �﷨

`b = [[fffc_linalg(module):eye(interface)]](m, n)`

### ����

```fortran
program demo_eye
    use fffc_module
    real :: x(2, 2)
    x = eye(2, 2)
end program demo_eye
```
