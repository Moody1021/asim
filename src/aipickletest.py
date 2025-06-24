#!/usr/bin/env python3
from aiparse import ExperimentParser
from aiparse import WorldParser
import pickle
import sys
import os
import argparse
import pprint
import pandas as pd


argp = argparse.ArgumentParser(description="Test Netlogo Data Processing Programs")
argp.add_argument('-d', action='store_true', help="Pickle a dataframe and print pickle file name")
argp.add_argument('-t', action='store_true', help="Test a pickle / unpickle and verify results are the same")
argp.add_argument('-p', action='store_true', help="Pickle an object and print pickle file name")
argp.add_argument('-w', action='store_true', help="Pickle a world and print pickle file name")
argp.add_argument('-u', action='store_true', help="Unpickle an object and print pickle file name")
argp.add_argument("-c", dest='csvfile', help="csv file to process")
argp.add_argument("-m", dest='modfile', help="modifier file", default="exp.mod")
argp.add_argument("-i", dest='picklefile', help="pickle file", default="exp.pickle")
argp.add_argument("-n", dest='plotname', help="Plot to extract", default="GroupCounter")

argp.add_argument("-o", action='store_true', help="output pretty print of object from pickling")


args = argp.parse_args()

picklefilep = "/tmp/file.picklep"
pickletest = "/tmp/filetest.pickle"
pickletestp = "/tmp/filetest.picklep"
outcsv = "out.csv"

picklefile = args.picklefile
csvfile = args.csvfile
plotname = args.plotname

print('Csv pickle plot', csvfile, picklefile, plotname)

if args.d:
    p = PlotParser(args.csvfile, args.modfile)
    p.parse()
    df = p.getdf()
    with open(picklefile,'wb') as pf:
        pickle.dump(df, pf)
        print("Pickle File: ", picklefile)

if args.p:
    p = ExperimentParser(args.csvfile, args.modfile)
    p.parse()
    df = p.getdf()
    with open(picklefile,'wb') as pf:
        pickle.dump(p, pf)
        print("Pickle File: ", picklefile)

if args.w:
    p = WorldParser(args.csvfile, args.modfile)
    p.parse()
    df = p.getdf()
    with open(picklefilep,'wb') as pf:
        pickle.dump(p, pf)
        print("Pickle File: ", picklefilep)

if args.t:
    with open(picklefile, 'rb') as pf:
        ro = pickle.load(pf)
    if df.equals(ro):
        print("Pickle is successful")
    else:
        print("Pickle Failure on: ", csvfile, " ", modfile)

if args.u:
    wo = WorldParser(csvfile)
    rw = wo.restore(picklefile)
    del wo
    s = rw.plots[plotname].data.to_csv()
    with open(outcsv, 'w') as pf:
        pf.write(s)

if args.o:
    with open(picklefile, 'rb') as pf:
        ro = pickle.load(pf)
    print('Pretty Print Object')
    pp = pprint.PrettyPrinter()
    pp.pprint(vars(ro))

