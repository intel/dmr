#!/usr/bin/env python3

import sys, os, jinja2, argparse
from shutil import copyfile

def render(tpl_path, context):
    path, filename = os.path.split(tpl_path)
    return jinja2.Environment(undefined=jinja2.StrictUndefined,
        loader=jinja2.FileSystemLoader(path or './')
    ).get_template(filename).render(context)

####

parser = argparse.ArgumentParser(description='Generate tests source files')
parser.add_argument('lib_path', metavar='/absolute_path', nargs=1, default='tests',
                    help='The destination path of source generation process (default: tests)')
args = parser.parse_args()

types=['integer', 'real', 'complex', 'logical']

b_1 ={'name':'8',   'key':'1P' }
b_2 ={'name':'16',  'key':'2P' }
b_4 ={'name':'32',  'key':'4P' }
b_8 ={'name':'64',  'key':'8P' }
b_16={'name':'128', 'key':'16P'}

kinds={'integer': [b_1, b_2, b_4, b_8],
       'real'   : [          b_4, b_8, b_16],
       'complex': [          b_4, b_8, b_16],
       'logical': [          b_4]}

keywords={'integer': ['Integer', 'int',   'I'],
          'real'   : ['Real',    'real',  'R'],
          'complex': ['Complex', 'cmplx', 'R'],
          'logical': ['Logical', 'lgcl',  'I']}

dimensions=7

with open('%s/test_dmr_target_free.F90' % args.lib_path[0], 'w') as f:
    f.write(render('tests/test_dmr_target_free.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('%s/test_dmr_target_is_present.F90' % args.lib_path[0], 'w') as f:
    f.write(render('tests/test_dmr_target_is_present.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('%s/test_dmr_target_alloc.F90' % args.lib_path[0], 'w') as f:
    f.write(render('tests/test_dmr_target_alloc.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

#with open('tests/dmr_correctly_mapped.F90', 'w') as f:
#    f.write(render('tests/dmr_correctly_mapped.jf90',
#      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
#                  ))
#
#with open('tests/dmr_get_mapped_ptr.F90', 'w') as f:
#    f.write(render('tests/dmr_get_mapped_ptr.jf90',
#      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
#                  ))
#
#with open('tests/dmr_target_init.F90', 'w') as f:
#    f.write(render('tests/dmr_target_init.jf90',
#      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
#                  ))
#
#with open('tests/dmr_device_memcpy.F90', 'w') as f:
#    f.write(render('tests/dmr_device_memcpy.jf90',
#      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
#                  ))
#
#with open('tests/dmr_target_memcpy.F90', 'w') as f:
#    f.write(render('tests/dmr_target_memcpy.jf90',
#      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
#                  ))
#
#with open('tests/dmr_target_memcpy_rect.F90', 'w') as f:
#    f.write(render('tests/dmr_target_memcpy_rect.jf90',
#      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
#                  ))
#
#with open('tests/dmr_target_memcpy_scalar.F90', 'w') as f:
#    f.write(render('tests/dmr_target_memcpy_scalar.jf90',
#      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
#                  ))
#
#with open('tests/dmr_environment.F90', 'w') as f:
#    f.write(render('tests/dmr_environment.jf90',
#      {'types' : types[0:2], 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
#                  ))
#
#with open('tests/dmr.F90', 'w') as f:
#    f.write(render('tests/dmr.jf90',
#      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
#                  ))
