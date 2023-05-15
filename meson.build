project(
    'fffc',
    'fortran',
    version: '1.1.20230515',
    default_options: [
        'buildtype=debugoptimized',
        'fortran_std=f2008',
        'default_library=both',
    ]
)
fffc_src = files()
fffc_src_filenames = []
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
subdir('test')
subdir('example')
fffc_lic = files(
    'LICENSE',
)
install_data(
    fffc_lic,
    install_dir: get_option('datadir')/'licenses'/meson.project_name(),
)
if host_machine.system() == 'windows'
    symbols_file = 'lib'+meson.project_name()+'-'+meson.project_version().split('.')[0]+'.dll.symbols'
    obj_suffix = '.obj'
else
    symbols_file = 'lib'+meson.project_name()+'.so.'+meson.project_version()+'.symbols'
    obj_suffix = '.o'
endif
fffc_obj = []
foreach source: fffc_src_filenames
    fffc_obj += 'src_'+source+obj_suffix
endforeach
install_subdir(fffc_lib.path()+'.p',
    install_dir: 'include'/meson.project_name(),
    strip_directory: true,
    exclude_files: [
        'depscan.dd',
        meson.project_name()+'-deps.json',
        symbols_file,
        meson.project_name()+'.dat',
        fffc_obj,
    ]
)
pkg = import('pkgconfig')
pkg.generate(
    fffc_lib,
    name: meson.project_name(),
    description: 'Fortran free function collection',
    version: meson.project_version(),
    requires: 'openblas',
    subdirs: meson.project_name(),
)
