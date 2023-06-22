---
title: Time
---

# 时间函数

Fortran 自带的时间函数已经够用，在此基础上，增加了一些常用的时间函数：

* `nowtime`: 返回当前时间，格式为 `yyyy-mm-dd hh:mm:ss`;
* `sec2hms`: 将秒数转换为时分秒格式，如 `sec2hms(3661)` 返回 `01:01:01`;
* `timer`: 计时器，返回从上次调用 `timer%tic()` 到现在`timer%toc()`的时间差，单位为秒。
* 