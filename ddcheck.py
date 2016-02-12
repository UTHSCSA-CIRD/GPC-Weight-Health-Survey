# for file listing
import sqlite3 as sq,pdb,os,subprocess,re;
# to create rxs object, where order matters
from collections import OrderedDict;

# where data dictionary files are to be found
from rccheck_shared import dddir;
# match repsective file patterns
from rccheck_shared import ddmatch,svmatch,dbmatch;
# replace strings to get site names
from rccheck_shared import ddnmrep,svnmrep,dbnmrep;
# prefixs for sqlite data dictionary and survey tables
from rccheck_shared import ddprfx,svprfx;
# name of sqlite script created, its path, database created, its path
from rccheck_shared import ddsqlscr,pthddsqlscr,ddsqldb,pthddsqldb;
# REDcap columns that need to be used
from rccheck_shared import rccols;
# regexes, an OrderedDict object
from rccheck_shared import rxs;
# SQLite UDFs
from rccheck_shared import typect,valct;
# functions
from rccheck_shared import unq;

# initialize sql file
sqscr = open(pthddsqlscr,'w');
sqscr.truncate();
sqscr.write('.mode csv\n');

""" 
find files and create tables, the assumption is a consistant naming pattern for tables
where the site name is the prefix and the rest is recognized by ddmatch, and can be
removed using ddnmrep
"""

# create a list of paired lists where the first one is the file name and the second is the table name
dds = [[ff,ddprfx+ff.replace(ddnmrep,'')] for ff in os.listdir(dddir) if os.path.isfile(ff) and ff.find(ddmatch) > 0];
# same, but for survey files
svs = [[ff,svprfx+ff.replace(svnmrep,'')] for ff in os.listdir(dddir) if os.path.isfile(ff) and ff.find(svmatch) > 0];
# ...and database files
dbs = [[ff,ff.replace(dbnmrep,'')] 
       for ff in os.listdir(dddir) if os.path.isfile(ff) and ff.find(dbmatch) > 0 and ff != ddsqldb];
# now write table import commands to a SQL script
[sqscr.write(gg) for gg in [".import "+" ".join(ff)+"\n" for ff in dds]];
# same, but for survey files
[sqscr.write(gg) for gg in [".import "+" ".join(ff)+"\n" for ff in svs]];
# add site-identifying columns for tables
[sqscr.write("alter table {0} ADD COLUMN svsite TEXT; update {0} set svsite = '{0}';".format(xx[1])+"\n")
 for xx in svs];

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

# TODO: work through the discrepancies, annotate the ones that are okay

# save the in-memory database
sqscr.write(".backup "+pthddsqldb+"\n");

# done
sqscr.close();


# screw whatever the sqlite3 module wants us to do for running scripts
# we'll just run it from the native client via shell and cross the t's
# later

subprocess.call("sqlite3 < "+pthddsqlscr,shell=True);

cn = sq.connect(pthddsqldb);
cn.create_aggregate('typect',1,typect);
cn.create_aggregate('valct',1,valct);

# all the column names in all the tables
svrawcols = [cn.execute('pragma table_info({0})'.format(xx)).fetchall() for xx in [yy[1] for yy in svs]];
# just the unique ones
svunqcols = unq([xx[1] for sublist in svrawcols for xx in sublist]);
# create a table with the union of all site survey columns
cn.execute("CREATE TABLE sv_unified ("+" TEXT,".join(['site']+svunqcols)+" TEXT);");
# create a dictionary object, which can be used to insert into columns defined for each site
svcols = dict(zip([xx[1] for xx in svs],svrawcols));
# one insert statement for each site's survey table
svinsrt = ["insert into sv_unified (site, " +\
  ",".join([jj[1] for jj in svcols[ii]])+") select '{0}' site,* from {0}".format(ii) for ii in svcols.keys()];
# now run the above inserts
[cn.execute(xx) for xx in svinsrt];
[cn.execute("attach database '{0}' as {1}".format(*xx) for xx in dbs];
# TODO: the below two statements execute silently. Find a way to output to screen
# ...these are comparisons between PATIENT_NUM's in the survey and in the .db file
"""
pmatched = cn.execute(" union all ".join(["select '{1}' site, count(*) good from {0}{1} where patient_num in (select patient_num from {1}.patient_dimension)".format(svprfx,xx[1]) for xx in dbs])).fetchall();
punmatched = cn.execute(" union all ".join(["select '{1}' site, count(*) missing from {0}{1} where patient_num not in (select patient_num from {1}.patient_dimension)".format(svprfx,xx[1]) for xx in dbs])).fetchall();
"""
# create a counts table
cn.execute("create table svcolcts as select site,"+",".join(["count(case when {0} is not null and {0} not in ('','0') then 1 else null end) {0}".format(ii) for ii in svunqcols])+" from sv_unified group by site");

cn.commit();

"""
#creates summary tables of data types broken up by site
cn.execute("create table sv_summ01 as select "+",".join(['site']+['typect({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified group by site union all select "+",".join(["'ALL' site"]+['typect({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified")
# ditto distinct counts
cn.execute("create table sv_summ02 as select "+",".join(['site']+['valct({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified group by site union all select "+",".join(["'ALL' site"]+['valct({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified")

"""
tocoal =  ",".join([ii[0] for ii in cn.execute("select distinct `Variable / Field Name` from allsites where `Form Name` != 'tracker_form' and `Field Type` = 'checkbox' and site != 'dd_utsw';").fetchall()]);
pdb.set_trace();


cn.close();