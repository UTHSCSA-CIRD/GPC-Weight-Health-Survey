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

# fix messed up uiowa multiply nested quotes by just blowing away those few rows for now
subprocess.call("grep -v '\"\{2,\}' uiowa_survey.csv > tempfix.csv",shell=True)
subprocess.call("mv tempfix.csv uiowa_survey.csv",shell=True)


""" 
find files and create tables, the assumption is a consistant naming pattern for tables
where the site name is the prefix and the rest is recognized by ddmatch, and can be
removed using ddnmrep
"""

# create a list of paired lists where the first one is the file name and the second is the table name
dds = [[ff,ddprfx+re.sub(ddnmrep,'',ff)] for ff in os.listdir(dddir) if os.path.isfile(ff) and re.search(ddmatch,ff) > 0];
# same, but for survey files
svs = [[ff,svprfx+re.sub(svnmrep,'',ff)] for ff in os.listdir(dddir) if os.path.isfile(ff) and re.search(svmatch,ff) > 0];
# ...and database files
dbs = [[ff,re.sub(dbnmrep,'',ff)] 
       for ff in os.listdir(dddir) if os.path.isfile(ff) and re.search(dbmatch,ff) > 0 and ff != ddsqldb];
# now write table import commands to a SQL script
[sqscr.write(gg) for gg in [".import "+" ".join(ff)+"\n" for ff in dds]];
# same, but for survey files
[sqscr.write(gg) for gg in [".import "+" ".join(ff)+"\n" for ff in svs]];
# add site-identifying columns for tables
[sqscr.write("alter table {0} ADD COLUMN svsite TEXT; update {0} set svsite = '{0}';".format(xx[1])+"\n")
 for xx in svs];

# create a single column of all unique field names to later join things onto
sqscr.write("create table scaffold as select distinct vfn from ("+\
  "\n union all \n".join(["select `Variable / Field Name` vfn from "+\
    xx[1] for xx in dds])+");\n");

sqscr.write("create table allsites as "+
  "\n union all \n".join([(" select "+",".join(["'{0}' site"]+rccols)+
    " from {0}").format(xx[1]) for xx in dds])+";\n");
  
"""
gigantic ugly statement that creates a table of all possible misalignments
(based on data dictionaries) between the site surveys.
Createed in TWO lines for... uh.. maintainability?
"""
diffqry01 = "select ' ' status,vfn,"+",".join(rccols[1:])+" from scaffold "+" ".join([(" left join (select {0} var, group_concat(distinct '('||{1}||')') {1} \nfrom allsites group by {0}) {1} \n on vfn = {1}.var".format(rccols[0],"{0}")).format(xx) for xx in rccols[1:]]);
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

# create an indicator column for overall responder/nonresponder
cn.execute("alter table sv_unified add column s2resp number"); 
cn.execute("update sv_unified set s2resp = 0");
respondercolumn = """
update sv_unified set s2resp = 1
where coalesce(research,possible_research,research_types_adult,research_depends_why,children_in_home,children_research
	       ,research_types2_child,res_dep_why2_child,res_talk_family,research_feeling,q6_ans6_response,deid_data
	       ,q7_ans6_response,height_req,height_feet,height_in,height_value_cm,weight_req,weight_value_lbs
	       ,weight_value_kg,bp_hypten_self,chole_triigl_hyperlip_self,bloodsugar_diabetes_self,cancer_anytype_self
	       ,hpb_hprtnsn,chole_trig_hyperlip,elev_bs_diabetes,cancer_anytype,sex,other_sex,age,latino_origin,other_race
	       ,income,insurance,other_insurance,education,other_schooling,household,language,other_language
	       ,research_accept_decisions___1,research_accept_decisions___2,research_accept_decisions___3
	       ,research_accept_decisions___4,research_accept_decisions___5,research_accept_decisions___6
	       ,research_accept_decisions___7,research_accept_dec_child___1,research_accept_dec_child___2
	       ,research_accept_dec_child___3,research_accept_dec_child___4,research_accept_dec_child___5
	       ,research_accept_dec_child___6,research_accept_dec_child___7,race___1,race___2,race___3,race___4,race___5
	       ,race___6,'') not in ('0','')
	       """;
cn.execute(respondercolumn);


# attach the i2b2 databuilder files to crossheck patients
[cn.execute("attach database '{0}' as {1}".format(xx[0],xx[1])) for xx in dbs];
dbspatients = [xx[1] for xx in dbs 
	       if any(['patient_num' in yy[1] 
		for yy in cn.execute("pragma table_info({0}{1})".format(svprfx,xx[1])).fetchall()])];
pmatched = cn.execute(" union all ".join(["""
  select '{1}' site, count(*) good from {0}{1} 
  where patient_num in 
  (select patient_num from {1}.patient_dimension)
  """.format(svprfx,xx) for xx in dbspatients])).fetchall();
punmatched = cn.execute(" union all ".join(["""
  select '{1}' site, count(*) missing from {0}{1} 
  where patient_num not in 
  (select patient_num from {1}.patient_dimension)
  """.format(svprfx,xx) for xx in dbspatients])).fetchall();

cn.execute("create table matched as "+" union all "
	   .join(["select '{0}' site, {1} matched, {2} unmatched "
	     .format(xx[0][0],xx[0][1],xx[1][1]) for xx in zip(pmatched,punmatched)]));

# TODO: the below two statements execute silently. Find a way to output to screen
# ...these are comparisons between PATIENT_NUM's in the survey and in the .db file
"""
pmatched = cn.execute(" union all ".join(["select '{1}' site, count(*) good from {0}{1} where patient_num in (select patient_num from {1}.patient_dimension)".format(svprfx,xx[1]) for xx in dbs])).fetchall();
punmatched = cn.execute(" union all ".join(["select '{1}' site, count(*) missing from {0}{1} where patient_num not in (select patient_num from {1}.patient_dimension)".format(svprfx,xx[1]) for xx in dbs])).fetchall();
"""
cn.commit();

"""
#creates summary tables of data types broken up by site
cn.execute("create table sv_summ01 as select "+",".join(['site']+['typect({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified group by site union all select "+",".join(["'ALL' site"]+['typect({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified")
# ditto distinct counts
cn.execute("create table sv_summ02 as select "+",".join(['site']+['valct({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified group by site union all select "+",".join(["'ALL' site"]+['valct({0}) {0}'.format(ii) for ii in svunqcols])+" from sv_unified")

"""
tocoal =  ",".join([ii[0] for ii in cn.execute("select distinct `Variable / Field Name` from allsites where `Form Name` != 'tracker_form' and `Field Type` = 'checkbox' and site != 'dd_utsw';").fetchall()]);
pdb.set_trace();


