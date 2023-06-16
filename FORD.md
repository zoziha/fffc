---
project: Fortran free function collection
summary: Fortran免费函数集合
src_dir: src
preprocess: false
project_website: https://gitee.com/ship-stack/fffc
project_github: https://github.com/zoziha/fffc
output_dir: _build/ford
page_dir: ford
author: 左志华
author_description: 哈尔滨工程大学-船舶与海洋结构物设计制造
email: zuo.zhihua@qq.com
website: https://gitee.com/zoziha
graph: false
source: true
md_extensions: markdown.extensions.toc
parallel: 4
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
---

# FFFC

这主要是我个人编程用到的**通用**的函数集合，包括一些常用的数学函数、字符串操作、文件系统操作、随机数生成、统计函数、线性代数等等。

除此之外，我还编制了用于船海专业的函数库，它对其它领域的人可能没有什么帮助，没有包含在这里。
并且，我还常常使用一些函数库，它们可以作为本库在数值算法上的补充：

* [FFTW](http://www.fftw.org/)：快速傅里叶变换
* [LAPACK](http://www.netlib.org/lapack/)：线性代数
* [OpenBLAS](https://www.openblas.net/)：线性代数
* [minpack](https://github.com/fortran-lang/minpack)：最小二乘法
* [argparse-f](https://github.com/0382/argparse-f)：命令行参数解析
* [hdf5](https://github.com/HDFGroup/hdf5)：高性能数据格式
* [toml-f](https://github.com/toml-f/toml-f)：TOML格式文件解析
* [json-fortran](https://github.com/jacobwilliams/json-fortran)：JSON格式文件解析
* [fhash](https://github.com/LKedward/fhash)：哈希表
* [fortran-lua](https://github.com/interkosmos/fortran-lua53)：Lua脚本解析

别忘了，Fortran语言的内置函数库也是很强大的，配置文件也可以采用Namelist格式，也可以借助接口使用C库。

一些有用的Fortran编程工具：

* [fpm](https://github.com/fortran-lang/fpm)：Fortran包管理器
* [meson](https://mesonbuild.com/)：构建系统
* [cmake](https://cmake.org/)：构建系统
* [gfortran](https://gcc.gnu.org/fortran/)：Fortran编译器
* [ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html)：Fortran编译器
* [vtune](https://software.intel.com/content/www/us/en/develop/tools/vtune-profiler.html)：性能分析工具
* [gprof](https://www.gnu.org/software/gprof/)：性能分析工具
* [valgrind](https://valgrind.org/)：内存泄漏检测工具
* [gdb](https://www.gnu.org/software/gdb/)：调试工具
* [doxygen](https://www.doxygen.nl/index.html)：文档生成工具
* [ford](https://github.com/Fortran-FOSS-Programmers/ford)：文档生成工具
* [fprettify](https://github.com/pseewald/fprettify)：Fortran代码格式化工具
* [vscode](https://code.visualstudio.com/)：编辑器
* [notepad3](https://www.rizonesoft.com/downloads/notepad3/)：编辑器
* [vim](https://www.vim.org/)：编辑器
* [emacs](https://www.gnu.org/software/emacs/)：编辑器
* [git](https://git-scm.com/)：版本控制工具
* [github](https://github.com/)：代码托管平台
* [gitee](https://gitee.com/)：代码托管平台
* [fortls](https://github.com/fortran-lang/fortls)：Fortran语言服务器
* [msys2](https://www.msys2.org/)：Windows下的Linux环境
* [cygwin](https://www.cygwin.com/)：Windows下的Linux环境
* [wsl](https://docs.microsoft.com/en-us/windows/wsl/install-win10)：Windows下的Linux环境
* [vscode-fortran-support](https://github.com/fortran-lang/vscode-fortran-support)：VSCode插件
* [fortran-discourse](https://fortran-lang.discourse.group/)：Fortran讨论组
* [fortran网站](https://fortran-lang.org/)：Fortran网站
* [octave](https://octave.org/)：Matlab的开源替代品
* [powershell](https://docs.microsoft.com/en-us/powershell/)：Windows下的脚本语言
* [starship](https://starship.rs/)：命令行提示符
* [fish](https://fishshell.com/)：命令行提示符
* [visual-studio](https://visualstudio.microsoft.com/)：IDE
* [codeblocks](http://www.codeblocks.org/)：IDE
* [lfortran](https://lfortran.org/)：Fortran编译器
* [j3-fortran](https://j3-fortran.org/)：Fortran标准化组织
* [flang](https://flang.llvm.org/docs/)：Fortran编译器
* [nvfortran](https://developer.nvidia.com/hpc-sdk)：Fortran编译器
