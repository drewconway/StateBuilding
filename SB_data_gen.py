#!/usr/bin/env python
# encoding: utf-8
"""
SB_data_gen.py

Purpose:  Generates various data sets from ABM

Author:   Drew Conway
Email:    drew.conway@nyu.edu
Date:     2010-07-27

Copyright (c) 2010, under the Simplified BSD License.  
For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
All rights reserved.
"""

import sys
import os
import StateBuilding as SB  
import networkx as nx
import zipfile
import csv


'''
Archiving functions taken from: http://bytes.com/topic/python/answers/845051-backup-zips
'''
def makeArchive(fileList, archive):
    """
    'fileList' is a list of file names - full path each name
    'archive' is the file name for the archive with a full path
    """
    try:
        a = zipfile.ZipFile(archive, 'w', zipfile.ZIP_DEFLATED)
        for f in fileList:
            print "archiving file %s" % (f)
            a.write(f)
        a.close()
        return True
    except: return False
 
def dirEntries(dir_name, subdir, *args):
    '''Return a list of file names found in directory 'dir_name'
    If 'subdir' is True, recursively access subdirectories under 'dir_name'.
    Additional arguments, if any, are file extensions to match filenames. Matched
        file names are added to the list.
    If there are no additional arguments, all files found in the directory are
        added to the list.
    Example usage: fileList = dirEntries(r'H:\TEMP', False, 'txt', 'py')
        Only files with 'txt' and 'py' extensions will be added to the list.
    Example usage: fileList = dirEntries(r'H:\TEMP', True)
        All files and all the files in subdirectories under H:\TEMP will be added
        to the list.
    '''
    fileList = []
    for file in os.listdir(dir_name):
        dirfile = os.path.join(dir_name, file)
        if os.path.isfile(dirfile):
            if not args:
                fileList.append(dirfile)
            else:
                if os.path.splitext(dirfile)[1][1:] in args:
                    fileList.append(dirfile)
        # recursively access file names in subdirectories
        elif os.path.isdir(dirfile) and subdir:
            print "Accessing directory:", dirfile
            fileList.extend(dirEntries(dirfile, subdir, *args))
    return fileList

def main():
    # Base parameters
    num_runs=500
    num_agents=150
    
    # Set up directory structure for data storage
    data_dir="ABM_data" # Directory for all ABM data outout
    sub_dirs={"binom":"binomial","uni":"uniform","pref":"pref_attach","par":"pareto","pl":"power_law"}   # All sub-directries
    # Create directories
    try:
        os.mkdir(data_dir)
    except(OSError):
        pass
    for d in sub_dirs.values():
        try:
            os.mkdir(data_dir+"/"+d)
        except(OSError):
            pass
    
    ###### SIMULATION RUNS ######
    for r in xrange(num_runs):
        ### Group 1: Binomial random networks ###
        # Create random GNP network, and get degree sequence
        gnp=nx.generators.gnp_random_graph(num_agents,p=.5)
        binom_ds=gnp.degree()   
        E_BINOM=SB.Environment(population=num_agents,degree_seq=binom_ds)
        E_BINOM.get_data(data_dir+"/"+sub_dirs["binom"]+"/"+str(r)+"_"+sub_dirs["binom"]+".csv")
    
        ### Group 2: Uniform degree distirbution random networks ###
        # Create unidform degree sequence
        uni_ds=nx.create_degree_sequence(num_agents,nx.utils.uniform_sequence)
        E_UNI=SB.Environment(population=num_agents,degree_seq=uni_ds)
        E_UNI.get_data(data_dir+"/"+sub_dirs["uni"]+"/"+str(r)+"_"+sub_dirs["uni"]+".csv")
        
        ### Group 3: Prefential attachment by agent wealth parameter (class default) ###
        E_PREF=SB.Environment(population=num_agents)
        E_PREF.get_data(data_dir+"/"+sub_dirs["pref"]+"/"+str(r)+"_"+sub_dirs["pref"]+".csv")
        
        ### Group 4: Pareto degree distribution random networks ###
        par_ds=nx.create_degree_sequence(num_agents,nx.utils.pareto_sequence)
        E_PAR=SB.Environment(population=num_agents,degree_seq=par_ds)
        E_PAR.get_data(data_dir+"/"+sub_dirs["par"]+"/"+str(r)+"_"+sub_dirs["par"]+".csv")
        
        ### Group 5: Pareto degree distribution random networks ###
        pl_ds=nx.create_degree_sequence(num_agents,nx.utils.powerlaw_sequence)
        E_PL=SB.Environment(population=num_agents,degree_seq=pl_ds)
        E_PL.get_data(data_dir+"/"+sub_dirs["pl"]+"/"+str(r)+"_"+sub_dirs["pl"]+".csv")
        
        # Update the stdout on progress. Dumb method, but straightforward
        if r==num_runs*.25:
            # 25% complete
            print "Simulation "+str((float(r)/num_runs)*100)+"% complete"
        else:
            if r==num_runs*.5:
                print "Simulation "+str((float(r)/num_runs)*100)+"% complete"
            else:
                if r==num_runs*.75:
                    print "Simulation "+str((float(r)/num_runs)*100)+"% complete"
    
    print "SIMULATION COMPLETE"
    print ""
    
    # Create single CSV file from all saved files for each network type
    for d in sub_dirs.values():
        for r in xrange(num_runs):
            dr=csv.DictReader(open(data_dir+"/"+d+"/"+str(r)+"_"+d+".csv","r"))
            if r==0:
                dw=csv.DictWriter(open(data_dir+"/"+d+"/FULL_"+d+".csv", "w"),fieldnames=dr.fieldnames)
                header=dict(zip(dr.fieldnames,dr.fieldnames))
                dw.writerow(header)
            for row in dr:
                dw.writerow(row)
    
    ### Finally, archive data ###
    print "Archiving data"
    # Zip data files into single file
    makeArchive(dirEntries(data_dir,True),data_dir+".zip")

if __name__ == '__main__':
    main()

