# -*- Mode: python; py-indent-offset: 4; indent-tabs-mode: nil; coding: utf-8; -*-

# def options(opt):
#     pass

def configure(conf):
    conf.recurse('lib')

def build(bld):
    #build the library first
    bld.recurse('lib')

    module = bld.create_ns3_module('if77', ['core', 'propagation'])
    module.source = [
        'model/if77-propagation-loss-model.cc',
        ]

    module.use.append ('if77cpp')

    module_test = bld.create_ns3_module_test_library('if77')
    module_test.source = [
        'test/if77-propagation-loss-model-test-suite.cc',
        ]
    # Tests encapsulating example programs should be listed here
    if (bld.env['ENABLE_EXAMPLES']):
        module_test.source.extend([
        #    'test/if77-examples-test-suite.cc',
             ])

    headers = bld(features='ns3header')
    headers.module = 'if77'
    headers.source = [
        'model/if77-propagation-loss-model.h',
        ]

    if bld.env.ENABLE_EXAMPLES:
        bld.recurse('examples')


    # bld.ns3_python_bindings()

