#!/usr/bin/env python3

import sys, os, jinja2
from shutil import copyfile

def render(tpl_path, context):
    path, filename = os.path.split(tpl_path)
    return jinja2.Environment(undefined=jinja2.StrictUndefined,
        loader=jinja2.FileSystemLoader(path or './')
    ).get_template(filename).render(context)

####

types=['integer', 'real', 'complex', 'logical']

b_1 ={'name':'8',   'key':'1P',  'name_n':8}
b_2 ={'name':'16',  'key':'2P',  'name_n':16}
b_4 ={'name':'32',  'key':'4P',  'name_n':32}
b_8 ={'name':'64',  'key':'8P',  'name_n':64}
b_16={'name':'128', 'key':'16P', 'name_n':128}

kinds={'integer': [b_1, b_2, b_4, b_8],
       'real'   : [          b_4, b_8, b_16],
       'complex': [          b_4, b_8, b_16],
       'logical': [          b_4]}

keywords={'integer': ['Integer', 'int',   'I'],
          'real'   : ['Real',    'real',  'R'],
          'complex': ['Complex', 'cmplx', 'R'],
          'logical': ['Logical', 'lgcl',  'I']}

dimensions=7

with open('dmr_environment.F90', 'w') as f:
    f.write(render('dmr_environment.jf90',
      {'types' : types[0:2], 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))

with open('dmr.F90', 'w') as f:
    f.write(render('dmr.jf90',
      {'types' : types, 'kinds' : kinds, 'keywords' : keywords, 'dimensions' : dimensions}
                  ))
