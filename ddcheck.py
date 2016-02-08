# for file listing
import sqlite3 as sq,pdb,os,subprocess;


"""
Stuff to move to a config file if we keep using this script
"""

# where data dictionary files are to be found
dddir = '.';
# match data dictionary pattern
ddmatch = '_dd.csv.csv';
# replace data dictionary string to get site name
ddnmrep = ddmatch;
# prefix for sqlite dd tables
ddprfx = 'dd_';
# name of sqlite script created
ddsqlscr = 'dd_sqlscript.sql';
ddsqlscr = dddir+"/"+ddsqlscr;
# name of sqlite output database
ddsqldb = 'ddcheck.db';
ddsqldb = dddir+"/"+ddsqldb;
# REDcap columns that need to be used
rccols = ["`Variable / Field Name`","`Field Type`"
,"`Field Label`","`Choices, Calculations, OR Slider Labels`"
,"`Field Note`","`Text Validation Type OR Show Slider Number`"
,"`Identifier?`","`Branching Logic (Show field only if...)`"];

# initialize sql file
sqscr = open(ddsqlscr,'w');
sqscr.truncate();
sqscr.write('.mode csv\n');

""" 
find files and create tables, the assumption is a consistant naming pattern for tables
where the site name is the prefix and the rest is recognized by ddmatch, and can be
removed using ddnmrep
"""

# create a list of paired lists where the first one is the file name and the second is the table name
dds = [[ff,ddprfx+ff.replace(ddnmrep,'')] for ff in os.listdir(dddir) if os.path.isfile(ff) and ff.find(ddmatch) > 0];
# now write table import commands to a SQL script
[sqscr.write(gg) for gg in [".import "+" ".join(ff)+"\n" for ff in dds]];
# create a single column of all unique field names to later join things onto
sqscr.write("create table scaffold as select distinct vfn from ("+\
  " union all ".join(["select `Variable / Field Name` vfn from "+\
    xx[1] for xx in dds])+");\n");

sqscr.write("create table allsites as "+
  " union all ".join([(" select "+",".join(["'{0}' site"]+rccols)+
    " from {0}").format(xx[1]) for xx in dds])+";\n");
  
"""
gigantic ugly statement that creates a table of all possible misalignments
(based on data dictionaries) between the site surveys.
Createed in TWO lines for... uh.. maintainability?
"""
diffqry01 = "select ' ' status,vfn,"+",".join(rccols[1:])+" from scaffold "+" ".join([(" left join (select {0} var, group_concat(distinct '('||{1}||')') {1} from allsites group by {0}) {1} on vfn = {1}.var".format(rccols[0],"{0}")).format(xx) for xx in rccols[1:]]);
diffqry02 = "create table diffs as "+diffqry01+" where "+"||".join(rccols[1:])+" like '%),(%';\n";
sqscr.write(diffqry02);

# TODO: group concat distinct the above subquery, group by field name
# TODO: iterate over all rccols with above
# done

# save the in-memory database
sqscr.write(".backup "+ddsqldb+"\n");
sqscr.close();

pdb.set_trace();

# screw whatever the sqlite3 module wants us to do for running scripts
# we'll just run it from the native client via shell and cross the t's
# later

subprocess.call("sqlite3 < "+ddsqlscr,shell=True);

