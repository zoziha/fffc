# Maintainer: ZUO Zhihua <zuo.zhihua@qq.com>

_realname=fffc
pkgbase=mingw-w64-${_realname}
pkgname="${MINGW_PACKAGE_PREFIX}-${_realname}"
pkgver=1.1.0
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
source=(${_realname}-${pkgver}.zip::"${url}/repository/archive/v${pkgver}.zip")
sha256sums=('2e7aab55b4c857b3db98a07afb55d338d63948cc7c7941b5ece0c6ee46d8dfe6')

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
