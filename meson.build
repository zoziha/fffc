project(
    'fffc',
    'fortran',
    version: '0.3.0',
    default_options: [
        'buildtype=debugoptimized',
        'fortran_std=f2008',
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
        dependency('openmp'),
    ],
    install: true,
)
fffc_inc = fffc_lib.private_dir_include()
fffc_dep = declare_dependency(
    include_directories: fffc_inc,
    link_with: fffc_lib,
)
