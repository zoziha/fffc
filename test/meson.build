testdrive_dep = dependency('test-drive', required: false, method: 'pkg-config')
if not testdrive_dep.found()
    testdrive_dep = subproject('test-drive').get_variable('testdrive_dep')
endif

test_src = files(
    'test_fffc_filesystem.F90',
    'test_fffc_time.f90',
    'test_fffc_io.f90',
    'test_fffc_terminal.f90',
)

test_lib = library(
    'test_fffc',
    sources: test_src,
    link_with: fffc_lib,
    dependencies: testdrive_dep,
)

test(
    'func_api',
    executable(
        'test_func_api',
        'test_func_api.f90',
        link_with: test_lib,
        dependencies: [
            fffc_dep,
            testdrive_dep,
        ]
    )
)

test(
    'example_dirname',
    executable(
        'example_dirname',
        'example_dirname.f90',
        dependencies:[
            fffc_dep,
        ],
    ),
)
test(
    'example_basename',
    executable(
        'example_basename',
        'example_basename.f90',
        dependencies:[
            fffc_dep,
        ],
    ),
)
test(
    'example_linv',
    executable(
        'example_linv',
        'example_linv.f90',
        dependencies:[
            fffc_dep,
        ],
    ),
)
test(
    'example_rinv',
    executable(
        'example_rinv',
        'example_rinv.f90',
        dependencies:[
            fffc_dep,
        ],
    ),
)

test(
    'example_progress_bar',
    executable(
        'example_progress_bar',
        'example_progress_bar.f90',
        dependencies:[
            fffc_dep,
        ],
    ),
)
