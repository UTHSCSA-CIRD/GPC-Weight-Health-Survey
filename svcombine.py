# for file listing
import sqlite3 as sq,pdb,os,subprocess,re;
# to create rxs object, where order matters
from collections import OrderedDict;

# DONE: move the shared definitions to a separate file
# TODO: simple way to distinguish non-respondents, respondents, and long-form respondents
# TODO: MDY -> YMD for MCRF
# DONE: examine the unique text values in fields that have them, insure no Identifiers
# DONE: .csv file with the svconsensus columns and site indicator
# TODO: import into R, examine

# path to database
from rccheck_shared import pthddsqldb;

# manually pre-fill the ones that need to be treated specially
# the rest will get programmatically populated from dd data
acodes = {
'children_research' : ['Yes_Contact', 'Maybe_Contact', 'No_Contact', 'No_Kids', 'Prefer_Not_Answer']
};
  
# convert codes to labels, the existence of an acodes dictionary is hardcoded for now
def cd2str(colname,colval):
  try:
    if colval != None and colval.isdigit() and int(colval) > 0 \
      and len(globals()['acodes'][colname]) >= int(colval) and colname in globals()['acodes'].keys():
      return globals()['acodes'][colname][int(colval)-1];
    else: 
      return colval;
  except Exception as wtf:
    print type(wtf); print wtf; print colname, colval;

# bulk replace func from http://stackoverflow.com/a/6117042/945039
# by http://stackoverflow.com/users/756329/joseph
def repl_all(text, dic):
    for ii, ij in dic.iteritems():
        text = text.replace(ii, ij);
    return text;

"""
These are the fields that are consistent between site surveys in every way 
we could find so far
"""
svconsensus = ['bloodsugar_diabetes_self','bp_hypten_self','cancer_anytype','cancer_anytype_self','children_in_home'
	       ,'children_research','chole_trig_hyperlip','chole_triigl_hyperlip_self','deid_data','education'
	       ,'elev_bs_diabetes','height_req','household','hpb_hprtnsn','income','insurance','invite_response_nature'
	       ,'language','latino_origin','possible_research','preferred_contact_method','race','res_talk_family'
	       ,'research','research_accept_dec_child','research_accept_decisions','research_feeling','sex'
	       ,'survey_contact_method','weight_req','age','city','cmp_address','comment_1','comment_2','comment_3'
	       ,'contact_email','contact_fname','contact_fullname','contact_lname','contact_type','family_id'
	       ,'first_con_atmpt_res','height_feet','height_in','height_value_cm','invite_date1','invite_date2'
	       ,'invite_date3','invite_response_date','kic_note_date','mailaddress','mailaddress2','match_type'
	       ,'other_insurance','other_language','other_race','other_schooling','other_sex','pat_age','pat_bmi_pct'
	       ,'pat_bmi_raw','pat_sex','patient_num','proj_bin','proj_id','proj_id_number','q6_ans6_response'
	       ,'q7_ans6_response','record_id','res_dep_why2_child','research_depends_why','research_types2_child'
	       ,'research_types_adult','state','survey_access_code','survey_complete_date','survey_contact_person'
	       ,'survey_date1','survey_date2','survey_date3','survey_link','usps_address','wave','weight_value_kg'
	       ,'weight_value_lbs','zipcode'];

# manually configured fields that are currently to be allowed into the output data
okayfields = ['site', 'state', 'contact_type', 'match_type', 'pat_sex', 'proj_bin,research_types_adult'
	      , 'children_in_home', 'research_types2_child', 'res_talk_family', 'q6_ans6_response', 'deid_data'
	      , 'q7_ans6_response', 'height_req', 'height_feet', 'height_in', 'weight_value_lbs'
	      , 'bloodsugar_diabetes_self', 'other_sex', 'other_race', 'other_insurance','other_language'
	      , 'health_medical_research_family_survey_completerecord_id','family_id', 'pat_age', 'pat_bmi_raw'
	      , 'pat_bmi_pct', 'proj_id', 'patient_num', 'wave', 'invite_response_nature', 'preferred_contact_method'
	      , 'survey_contact_method', 'tracker_form_complete', 'research', 'possible_research'
	      , 'research_accept_decisions___1', 'research_accept_decisions___2', 'research_accept_decisions___3'
	      , 'research_accept_decisions___4', 'research_accept_decisions___5', 'research_accept_decisions___6'
	      , 'research_accept_decisions___7', 'research_accept_dec_child___1', 'research_accept_dec_child___2'
	      , 'research_accept_dec_child___3', 'research_accept_dec_child___4', 'research_accept_dec_child___5'
	      , 'research_accept_dec_child___6', 'research_accept_dec_child___7', 'height_value_cm', 'weight_req'
	      , 'weight_value_kg', 'bp_hypten_self', 'chole_triigl_hyperlip_self', 'cancer_anytype_self'
	      , 'hpb_hprtnsn', 'chole_trig_hyperlip', 'elev_bs_diabetes', 'cancer_anytype', 'sex', 'age'
	      , 'latino_origin', 'race___1', 'race___2', 'race___3', 'race___4', 'race___5', 'race___6', 'income'
	      , 'insurance', 'household', 'language'];


# stuff to replace
repls = {'<i>':'','</i>':''}


# exclusion criteria
svexclude = ' and '.join([" site != 'sv_utsw' "]);
ddexclude = ' and '.join([" site != 'dd_utsw' "]);

# connect to this badboy...
cn = sq.connect(pthddsqldb);
cn.create_function('cd2str',2,cd2str);

# radio and dropdown elements from data dictionaries 
# (each value is a different integer code in the corresponding column)
codestrings = cn.execute("select distinct `Variable / Field Name`,`Choices, Calculations, OR Slider Labels` from allsites where `Field Type` in ('dropdown','radio')"+" and "+ddexclude).fetchall();

# codestrings is now used to create key-value pairs in acodes where the column name is the key
# and the list of text descriptions is the value e.g. ['Yes', 'No', 'I prefer to not answer']
for xx in codestrings:
  if xx[0] not in acodes.keys():
    acodes[xx[0]] = [yy[3:] for yy in repl_all(xx[1],repls).split(" | ")];

# now we pull the checkbox elements from the data dictionaries
# (each value is a 1 in its own column, and 0 if that box was not checked)
codestrings = cn.execute("select distinct `Variable / Field Name`,`Choices, Calculations, OR Slider Labels` from allsites where `Field Type` = 'checkbox'"+" and "+ddexclude).fetchall();

# add these to acodes
for xx in codestrings:
  if len([yy for yy in acodes.keys() if xx[0]+"___" in yy]) == 0:
    acodes.update(dict([(xx[0]+"___"+yy[0],[yy[3:]]) for yy in repl_all(xx[1],repls).split(" | ")]));

dataout = cn.execute("select "+",".join([" cd2str('{0}',{0}) {0} ".format(xx[1]) 
					 if xx[1] in acodes.keys() else xx[1] 
					 for xx in cn.execute('pragma table_info(sv_unified)').fetchall() 
					 if xx[1] in okayfields])+" from sv_unified where "+svexclude).fetchall();
out = open('testoutput.csv','w');
out.truncate();
out.write('"'+'"\t"'.join([xx[1] for xx in cn.execute('pragma table_info(sv_unified)').fetchall() 
			   if xx[1] in okayfields])+'"\n');
[out.write('"'+'"\t"'.join(yy or "" for yy in xx)+'"\n') for xx in dataout];

pdb.set_trace();
  
