#!/usr/bin/env python3

from aiparse import ExperimentParser
from aiparse import WorldParser
import re
import os
import sys


exp = sys.argv[1]
expfile = exp + '.csv'
ep = ExperimentParser(expfile)
basedir = os.path.dirname(exp)
ep.parse()
ep.store()

mydf = ep.df
numruns = ep.numruns

print("Process each runs csv file into a dataframe and adjust time sequence")
def storeworld(plotcsv):
    p = WorldParser(plotcsv)
    p.parse()
    p.timeadj()
    p.store()

flist = os.popen('ls ' + basedir + '/plots/world*.csv').read().strip().split('\n')
for r in flist:
    res = storeworld(r)

exit()

