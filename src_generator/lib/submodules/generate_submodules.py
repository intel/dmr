#!/usr/bin/env python3

import sys, os, jinja2
from shutil import copyfile

def render(tpl_path, context):
    path, filename = os.path.split(tpl_path)
    return jinja2.Environment(undefined=jinja2.StrictUndefined,
        loader=jinja2.FileSystemLoader(path or './')
    ).get_template(filename).render(context)

####

types=['integer', 'real', 'complex']

b_1 ={'name':'8',   'key':'1P'}
b_2 ={'name':'16',  'key':'2P'}
b_4 ={'name':'32',  'key':'4P'}
b_8 ={'name':'64',  'key':'8P'}
b_16={'name':'128', 'key':'16P'}

kinds={'integer': [b_1, b_2, b_4, b_8],
       'real'   : [          b_4, b_8, b_16],
       'complex': [          b_4, b_8, b_16]}

keywords={'integer': ['Integer', 'int',   'I'],
          'real'   : ['Real',    'real',  'R'],
          'complex': ['Complex', 'cmplx', 'R']}

dimensions=7

with open('dmr_target_free.F90', 'w') as f:
    f.write(render('dmr_target_free.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords}
                  ))

with open('dmr_target_is_present.F90', 'w') as f:
    f.write(render('dmr_target_is_present.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords}
                  ))

with open('dmr_target_alloc.F90', 'w') as f:
    f.write(render('dmr_target_alloc.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('dmr_correctly_mapped.F90', 'w') as f:
    f.write(render('dmr_correctly_mapped.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('dmr_get_mapped_ptr.F90', 'w') as f:
    f.write(render('dmr_get_mapped_ptr.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('dmr_target_init.F90', 'w') as f:
    f.write(render('dmr_target_init.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('dmr_device_memcpy.F90', 'w') as f:
    f.write(render('dmr_device_memcpy.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('dmr_target_memcpy.F90', 'w') as f:
    f.write(render('dmr_target_memcpy.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('dmr_target_memcpy_rect.F90', 'w') as f:
    f.write(render('dmr_target_memcpy_rect.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('dmr_target_memcpy_scalar.F90', 'w') as f:
    f.write(render('dmr_target_memcpy_scalar.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))