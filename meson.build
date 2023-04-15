project(
    'fffc',
    'fortran',
    version: '1.0.0',
    default_options: [
        'buildtype=debugoptimized',
        'fortran_std=f2008',
        'default_library=both',
    ]
)
fffc_src = files()
subdir('src')
fffc_lib = library(
    meson.project_name(),
    fffc_src,
    version: meson.project_version(),
    dependencies: [
        dependency('openblas'),
    ],
    install: true,
)
fffc_inc = fffc_lib.private_dir_include()
fffc_dep = declare_dependency(
    include_directories: fffc_inc,
    link_with: fffc_lib,
)
example_random = executable(
    'random',
    'example/test_random.f90',
    link_with: fffc_lib,
)
fffc_lic = files(
    'LICENSE',
)
install_data(
    fffc_lic,
    install_dir: get_option('datadir')/'licenses'/meson.project_name(),
)
pkg = import('pkgconfig')
pkg.generate(
    fffc_lib,
    name: meson.project_name(),
    description: 'Fortran free function collection',
    version: meson.project_version(),
    requires: 'openblas',
)
