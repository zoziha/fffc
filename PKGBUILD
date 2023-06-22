# Maintainer: ZUO Zhihua <zuo.zhihua@qq.com>

_realname=fffc
pkgbase=mingw-w64-${_realname}
pkgname="${MINGW_PACKAGE_PREFIX}-${_realname}"
pkgver=1.3.20230622
pkgrel=1
arch=('any')
mingw_arch=('mingw32' 'mingw64' 'ucrt64')
pkgdesc="Fortran free function collection (mingw-w64)"
url="https://gitee.com/fortran-stack/fffc"
license=('spdx:BSD-3-Clause')
depends=("${MINGW_PACKAGE_PREFIX}-gcc-libs"
         $([[ ${MINGW_PACKAGE_PREFIX} == *-clang-* ]] || echo "${MINGW_PACKAGE_PREFIX}-gcc-libgfortran")
         "${MINGW_PACKAGE_PREFIX}-openblas")
makedepends=("${MINGW_PACKAGE_PREFIX}-fc"
             "${MINGW_PACKAGE_PREFIX}-meson"
             "${MINGW_PACKAGE_PREFIX}-ninja"
             "${MINGW_PACKAGE_PREFIX}-pkg-config"
             "${MINGW_PACKAGE_PREFIX}-openblas")
source=(${_realname}-${pkgver}.zip::"${url}/repository/archive/master.zip")
sha256sums=('0f0043c126ebb67d2651a1f46bdac8ec572ed68c3c95c6ff8d3c1afbc1d357fc')

build() {
    mkdir -p build-${MSYSTEM} && cd build-${MSYSTEM}

    MSYS2_ARG_CONV_EXCL="--prefix" \
      meson setup \
        --prefix="${MINGW_PREFIX}" \
        --buildtype=release \
        ../${_realname}-master

    meson compile
}

package() {
    cd "${srcdir}/build-${MSYSTEM}"

    DESTDIR="${pkgdir}" meson install
}
