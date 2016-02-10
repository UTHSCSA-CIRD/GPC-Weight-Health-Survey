# for file listing
import sqlite3 as sq,pdb,os,subprocess,re;
# to create rxs object, where order matters
from collections import OrderedDict;


"""
Stuff to move to a config file if we keep using this script
"""

# where data dictionary files are to be found
dddir = '.';
# match repsective file patterns
ddmatch = '_dd.csv'; svmatch = '_survey.csv'; dbmatch = '.db';
# replace data dictionary string to get site name
ddnmrep = ddmatch; svnmrep = svmatch; dbnmrep = dbmatch;
# prefixs for sqlite data dictionary and survey tables
ddprfx = 'dd_'; svprfx = 'sv_';
# name of sqlite script created
ddsqlscr = 'dd_sqlscript.sql';
pthddsqlscr = dddir+"/"+ddsqlscr;
# name of sqlite output database
ddsqldb = 'ddcheck.db';
pthddsqldb = dddir+"/"+ddsqldb;
# REDcap columns that need to be used
rccols = ["`Variable / Field Name`","`Field Type`"
,"`Field Label`","`Choices, Calculations, OR Slider Labels`"
,"`Field Note`","`Text Validation Type OR Show Slider Number`"
,"`Identifier?`","`Branching Logic (Show field only if...)`"];

"""
regexps
"""
rxs = OrderedDict([
  ('Empty' , '^\s{0,}$'),
  ('Digit' , '^[0-9]$'),
  ('Numbr' , '^[0-9,. ]{1,}$'),
  ('YMD'   , '^201[0-9]-[0-9]{2}-[0-9]{2}$'),
  ('MDY'   , '^[0-9]{2}-[0-9]{2}-201[0-9]$'),
  ('YMDTS' , '^201[0-9]-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$'),
  ('MDYTS' , '^[0-9]{2}-[0-9]{2}-201[0-9] [0-9]{2}:[0-9]{2}:[0-9]{2}$'),
  ('Text'  , '^.*$')
  ]);

"""
rxoneint = '^[0-9]$'; rxbignum = '^[0-9,. ]{1,}$';
rxymd = '^201[0-9]-[0-9]{2}-[0-9]{2}$';
rxmdy = '^[0-9]{2}-[0-9]{2}-201[0-9]$';
rxymdts = '^201[0-9]-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$';
rxmdyts = '^[0-9]{2}-[0-9]{2}-201[0-9] [0-9]{2}:[0-9]{2}:[0-9]{2}$';
rxempty = '^\s{0,}$';
"""
"""
SQL UDFs
"""
class typect:
  def __init__(self):
    self.xxvals = {};
  def step(self,xx):
    if xx is None: xx = '';
    mtype=[ii for ii in rxs.keys() if re.compile(rxs[ii]).match(xx) is not None][0];
    if mtype in self.xxvals.keys(): self.xxvals[mtype] += 1
    else: self.xxvals[mtype] = 1;
  def finalize(self):
    return (str(self.xxvals)[1:-1]).replace(' ','').replace("'",'');
  
class valct:
  def __init__(self):
    self.xxvals = {};
  def step(self,xx):
    if xx is None: xx = 'None'
    else: xx = str(xx).lower();
    if xx in self.xxvals.keys(): self.xxvals[xx] += 1
    else: self.xxvals[xx] = 1;
  def finalize(self):
    return (str(sorted(self.xxvals.items(), key=lambda tt: tt[1]))).replace("','",':').replace("(",'').replace(")",'').replace("'",'').replace(' ','')
  

"""
    if 'aa' not in foo.keys(): foo['aa'] = 1
... else: foo['aa'] += 1
"""

# mdy2ymd

"""
From http://stackoverflow.com/a/480227/945039
By Markus Jaderot, January 26, 2009
Returns a set of unique list elements while preserving order
"""
def unq(seq):
    seen = set()
    seen_add = seen.add
    return [xx for xx in seq if not (xx in seen or seen_add(xx))]

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
  ",".join([jj[1] for jj in svcols[ii]])+") select '"+ii+"' site,* from "+\
    ii for ii in svcols.keys()];
# now run the above inserts
[cn.execute(xx) for xx in svinsrt];
[cn.execute("attach database '{0}' as {1}".format(xx[0],xx[1])) for xx in dbs];
# TODO: the below two statements execute silently. Find a way to output to screen
# ...these are comparisons between PATIENT_NUM's in the survey and in the .db file
"""
pmatched = cn.execute(" union all ".join(["select '{1}' site, count(*) good from {0}{1} where patient_num in (select patient_num from {1}.patient_dimension)".format(svprfx,xx[1]) for xx in dbs])).fetchall();
punmatched = cn.execute(" union all ".join(["select '{1}' site, count(*) missing from {0}{1} where patient_num not in (select patient_num from {1}.patient_dimension)".format(svprfx,xx[1]) for xx in dbs])).fetchall();
"""
cn.commit();
cn.create_aggregate('typect',1,typect);
cn.create_aggregate('valct',1,valct);

"""
#creates summary tables of data types broken up by site
cn.execute("create table sv_summ01 as select "+",".join(['site']+['typect({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified group by site union all select "+",".join(["'ALL' site"]+['typect({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified")
# ditto distinct counts
cn.execute("create table sv_summ02 as select "+",".join(['site']+['valct({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified group by site union all select "+",".join(["'ALL' site"]+['valct({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified")

"""
pdb.set_trace();

cn.close();