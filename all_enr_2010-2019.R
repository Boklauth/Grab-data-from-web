
# WMU




# making a function2 for old format 2012-2010
# the difference is the classification of "Professional" and "Doctorate/Grad II"

transform_midata2<-function(college_name, term_name, dir_and_filename){
  
  a<-read.csv(dir_and_filename, stringsAsFactors = FALSE)
  # see number row
  dim(a)[1]
  
  # get enrollment number and distance learning for in-state
  # rename column
  b<-a[c(37:43, 94:97),c(1:2)]
  names(b)<-c("Level", "Enrollment")
  Residency<-rep("In State", dim(b)[1])
  bb<-data.frame(Residency, b)
  
  # get enrollment number and distance learning for in-state
  c<-a[c(37:43, 94:97),c(1,3)]
  names(c)<-c("Level", "Enrollment")
  Residency<-rep("Out of State", dim(c)[1])
  cc<-data.frame(Residency, c, stringsAsFactors = FALSE)
  d<-rbind(bb,cc)
  toString(d$Level)
  # change old classificatin to fit new ones for traditional learning
  d$Level[5]="Doctors Degree - Professional Practice"
  d$Level[16]="Doctors Degree - Professional Practice"
  d$Level[7]="Doctor's Degree - Research/Scholarship"
  d$Level[18]="Doctor's Degree - Research/Scholarship"
  
  # change old classificatin to fit new ones for online learning
  d$Level[8]="Dist/Learn Doctors - Professional Practice"
  d$Level[19]="Dist/Learn Doctors - Professional Practice"
  d$Level[10]="Doctor's Degree - Research/Scholarship"
  d$Level[21]="Dist/Learn Doctors - Research/Scholarship"
  
  
  # create vairables Term and School with values
  
  Term<-rep(term_name, dim(d)[1])
  School<-rep(college_name, dim(d)[1])
  dd<-data.frame(Term, School, d)
}


# function for new format 2013-2019
transform_midata<-function(college_name, term_name, dir_and_filename){
  
  a<-read.csv(dir_and_filename)
  # see number row
  dim(a)[1]
  
  # get enrollment number and distance learning for in-state
  # rename column
  b<-a[c(37:44, 99:103),c(1:2)]
  names(b)<-c("Level", "Enrollment")
  Residency<-rep("In State", dim(b)[1])
  bb<-data.frame(Residency, b)
  # get enrollment number and distance learning for in-state
  c<-a[c(37:44, 99:103),c(1,3)]
  names(c)<-c("Level", "Enrollment")
  Residency<-rep("Out of State", dim(c)[1])
  cc<-data.frame(Residency, c)
  d<-rbind(bb,cc)
  
  dim(d[1])
  # create vairables Term and School with values
  
  Term<-rep(term_name, dim(d)[1])
  School<-rep(college_name, dim(d)[1])
  dd<-data.frame(Term, School, d)
}






# APPLYING THE FUNCTION

wmu_2019<-transform_midata("WMU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2019.csv")
dim(a)[1]
wmu_2018<-transform_midata("WMU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2018.csv")
dim(a)[1]
wmu_2017<-transform_midata("WMU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2017.csv")
dim(a)[1]
wmu_2016<-transform_midata("WMU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2016.csv")
dim(a)[1]
bb
cc
dd
wmu_2015<-transform_midata("WMU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2015.csv")
dim(a)[1]
bb
cc
dim(dd)
wmu_2014<-transform_midata("WMU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2014.csv")
dim(a)[1]
bb
cc
dd
wmu_2013<-transform_midata("WMU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2013.csv")

# using a different fucntion
wmu_2012<-transform_midata2("WMU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2012.csv")
dd
# wrong numbers for 2012
wmu_2011<-transform_midata2("WMU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2011.csv")
wmu_2010<-transform_midata2("WMU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2010.csv")

wmu_enr<-rbind(
  wmu_2019,
  wmu_2018,
  wmu_2017,
  wmu_2016,
  wmu_2015,
  wmu_2014,
  wmu_2013,
  wmu_2012,
  wmu_2011,
  wmu_2010
)
dim(wmu_enr)
# write file to hard folder
write.csv(wmu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wmu/wmu_2010-2019.csv", row.names = FALSE)


# end wmu








# FUNCTION TO GRAB DATA FROM THE WEB


# version 1

enrollement_webgrab<-function(college_name, term_name, html_path){
  
  
  ##########################
  
  # Declare argument
  # college name
  #college_name="CMU"
  # Term
  #term_name="Fall 2019"
  # html path
  #html_path<-'C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2019.html'
  ###############
  ##############
  
  # traditional learning enrollment
  
  ###############
  ##############
  
  # read a web page.
  html_page<-readLines(html_path)
  
  # locate location with a title--levels
  grep('Headcount and Student Credit Hour by Academic Level', html_page)
  
  # Grab data with html tag
  parttern_levels='<td headers="MainContent_EN1_1">([^<]*)</td><td class="vReportCell" headers="MainContent_EN1_2_L1 MainContent_EN1_1_L2">'
  datalines_levels=grep(parttern_levels, html_page[234:248], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_levels,datalines_levels)
  matches = mapply(getexpr,datalines_levels,gg)
  result_levels = gsub(parttern_levels,'\\1',matches)
  names(result_levels)=NULL
  col_labels<-as.data.frame(matrix(result_levels, ncol=1, byrow=TRUE))
  names(col_labels)="Level"
  
  # locate location with a title--working instate_Enrollment
  
  
  # Grab data with html tag--instate enrollment
  parttern_is_enr='<td class="vReportCell" headers="MainContent_EN1_2_L1 MainContent_EN1_1_L2">([^<]*)</td>'
  datalines_is_enr=grep(parttern_is_enr, html_page[234:248], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_is_enr,datalines_is_enr)
  matches = mapply(getexpr,datalines_is_enr,gg)
  result_is_enr = gsub(parttern_is_enr,'\\1',matches)
  names(result_is_enr)=NULL
  col_ins_enr<-as.data.frame(matrix(result_is_enr, ncol=1, byrow=TRUE))
  names(col_ins_enr)<-"Enrollment"
  
  
  # Grab data with html tag--out of state enrollment
  # change the pattern
  parttern_os_enr='<td class="vReportCell" headers="MainContent_EN1_2_L1 MainContent_EN1_2_L2">([^<]*)</td>'
  datalines_os_enr=grep(parttern_os_enr, html_page[234:248], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_os_enr,datalines_os_enr)
  matches = mapply(getexpr,datalines_os_enr,gg)
  result_os_enr = gsub(parttern_os_enr,'\\1',matches)
  names(result_os_enr)=NULL
  col_os_enr<-as.data.frame(matrix(result_os_enr, ncol=1, byrow=TRUE))
  names(col_os_enr)="Enrollment"
  
  # Combine data labels and instate enrolment
  b<-col_ins_enr
  Residency<-rep("In State", dim(b)[1])
  bb<-data.frame(Residency, col_labels, b)
  # combine data levels and out of statement enrollment
  c<-col_os_enr
  Residency<-rep("Out of State", dim(c)[1])
  cc<-data.frame(Residency, col_labels, c)
  # rowbind 2 datasets
  dd<-rbind(bb,cc)
  
  
  ###############
  ##############
  
  # get distance learning enrollment
  
  ###############
  ##############
  
  # locate location with a title--levels
  grep('Distance Learning By Academic Level', html_page)
  
  # Grab data with html tag
  parttern_levels='<td headers="MainContent_EN3_1">([^<]*)</td><td class="vReportCell" headers="MainContent_EN3_1_L1 MainContent_EN3_1_L2">'
  datalines_levels=grep(parttern_levels, html_page[364:372], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_levels,datalines_levels)
  matches = mapply(getexpr,datalines_levels,gg)
  result_levels = gsub(parttern_levels,'\\1',matches)
  names(result_levels)=NULL
  col_labels<-as.data.frame(matrix(result_levels, ncol=1, byrow=TRUE))
  names(col_labels)="Level"
  
  # locate location with a title--working instate_Enrollment
  
  
  # Grab data with html tag--instate enrollment
  parttern_is_enr='<td class="vReportCell" headers="MainContent_EN3_1_L1 MainContent_EN3_1_L2">([^<]*)</td>'
  datalines_is_enr=grep(parttern_is_enr, html_page[364:372], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_is_enr,datalines_is_enr)
  matches = mapply(getexpr,datalines_is_enr,gg)
  result_is_enr = gsub(parttern_is_enr,'\\1',matches)
  names(result_is_enr)=NULL
  col_ins_enr<-as.data.frame(matrix(result_is_enr, ncol=1, byrow=TRUE))
  names(col_ins_enr)<-"Enrollment"
  
  
  # Grab data with html tag--out of state enrollment
  # change the pattern
  parttern_os_enr='<td class="vReportCell" headers="MainContent_EN3_1_L1 MainContent_EN3_2_L2">([^<]*)</td>'
  datalines_os_enr=grep(parttern_os_enr, html_page[364:372], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_os_enr,datalines_os_enr)
  matches = mapply(getexpr,datalines_os_enr,gg)
  result_os_enr = gsub(parttern_os_enr,'\\1',matches)
  names(result_os_enr)=NULL
  col_os_enr<-as.data.frame(matrix(result_os_enr, ncol=1, byrow=TRUE))
  names(col_os_enr)="Enrollment"
  
  # Combine data labels and instate enrolment
  e<-col_ins_enr
  Residency<-rep("In State", dim(e)[1])
  ee<-data.frame(Residency, col_labels, e)
  # combine data levels and out of statement enrollment
  f<-col_os_enr
  Residency<-rep("Out of State", dim(f)[1])
  ff<-data.frame(Residency, col_labels, f)
  # rowbind 2 datasets
  gg<-rbind(ee,ff)
  
  # combine traditional and distance enrollment
  
  hh<-rbind(dd,gg)
  
  
  # add variable school to the data set;
  
  School<-rep(college_name, dim(hh)[1])
  Term<-rep(term_name, dim(hh)[1])
  hhh<-cbind(Term, School, hh)
  
}



# version 2

enrollement_webgrab2<-function(college_name, term_name, html_path){
  
  
  ##########################
  
  # Declare argument
  # college name
  #college_name="CMU"
  # Term
  #term_name="Fall 2012"
  # html path
  #html_path<-"C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2012.html"
  ###############
  ##############
  
  # traditional learning enrollment
  
  ###############
  ##############
  
  # read a web page.
  html_page<-readLines(html_path)
  
  # locate location with a title--levels
  grep('Headcount and Student Credit Hour by Academic Level', html_page)
  
  # Grab data with html tag
  parttern_levels='<td headers="MainContent_EN1_1">([^<]*)</td><td class="vReportCell" headers="MainContent_EN1_2_L1 MainContent_EN1_1_L2">'
  datalines_levels=grep(parttern_levels, html_page[234:248], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_levels,datalines_levels)
  matches = mapply(getexpr,datalines_levels,gg)
  result_levels = gsub(parttern_levels,'\\1',matches)
  names(result_levels)=NULL
  col_labels<-as.data.frame(matrix(result_levels, ncol=1, byrow=TRUE))
  names(col_labels)="Level"
  
  # locate location with a title--working instate_Enrollment
  
  
  # Grab data with html tag--instate enrollment
  parttern_is_enr='<td class="vReportCell" headers="MainContent_EN1_2_L1 MainContent_EN1_1_L2">([^<]*)</td>'
  datalines_is_enr=grep(parttern_is_enr, html_page[234:248], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_is_enr,datalines_is_enr)
  matches = mapply(getexpr,datalines_is_enr,gg)
  result_is_enr = gsub(parttern_is_enr,'\\1',matches)
  names(result_is_enr)=NULL
  col_ins_enr<-as.data.frame(matrix(result_is_enr, ncol=1, byrow=TRUE))
  names(col_ins_enr)<-"Enrollment"
  
  
  # Grab data with html tag--out of state enrollment
  # change the pattern
  parttern_os_enr='<td class="vReportCell" headers="MainContent_EN1_2_L1 MainContent_EN1_2_L2">([^<]*)</td>'
  datalines_os_enr=grep(parttern_os_enr, html_page[234:248], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_os_enr,datalines_os_enr)
  matches = mapply(getexpr,datalines_os_enr,gg)
  result_os_enr = gsub(parttern_os_enr,'\\1',matches)
  names(result_os_enr)=NULL
  col_os_enr<-as.data.frame(matrix(result_os_enr, ncol=1, byrow=TRUE))
  names(col_os_enr)="Enrollment"
  
  # Combine data labels and instate enrolment
  b<-col_ins_enr
  Residency<-rep("In State", dim(b)[1])
  bb<-data.frame(Residency, col_labels, b)
  # combine data levels and out of statement enrollment
  c<-col_os_enr
  Residency<-rep("Out of State", dim(c)[1])
  cc<-data.frame(Residency, col_labels, c)
  # rowbind 2 datasets
  dd<-rbind(bb,cc)
  
  
  ###############
  ##############
  
  # get distance learning enrollment
  
  ###############
  ##############
  
  # locate location with a title--levels
  grep('Distance Learning By Academic Level', html_page)
  
  # Grab data with html tag
  parttern_levels='<td headers="MainContent_EN3_1">([^<]*)</td><td class="vReportCell" headers="MainContent_EN3_1_L1 MainContent_EN3_1_L2">'
  datalines_levels=grep(parttern_levels, html_page[352:362], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_levels,datalines_levels)
  matches = mapply(getexpr,datalines_levels,gg)
  result_levels = gsub(parttern_levels,'\\1',matches)
  names(result_levels)=NULL
  col_labels<-as.data.frame(matrix(result_levels, ncol=1, byrow=TRUE))
  names(col_labels)="Level"
  
  # locate location with a title--working instate_Enrollment
  
  
  # Grab data with html tag--instate enrollment
  parttern_is_enr='<td class="vReportCell" headers="MainContent_EN3_1_L1 MainContent_EN3_1_L2">([^<]*)</td>'
  datalines_is_enr=grep(parttern_is_enr, html_page[352:362], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_is_enr,datalines_is_enr)
  matches = mapply(getexpr,datalines_is_enr,gg)
  result_is_enr = gsub(parttern_is_enr,'\\1',matches)
  names(result_is_enr)=NULL
  col_ins_enr<-as.data.frame(matrix(result_is_enr, ncol=1, byrow=TRUE))
  names(col_ins_enr)<-"Enrollment"
  
  
  # Grab data with html tag--out of state enrollment
  # change the pattern
  parttern_os_enr='<td class="vReportCell" headers="MainContent_EN3_1_L1 MainContent_EN3_2_L2">([^<]*)</td>'
  datalines_os_enr=grep(parttern_os_enr, html_page[352:362], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(parttern_os_enr,datalines_os_enr)
  matches = mapply(getexpr,datalines_os_enr,gg)
  result_os_enr = gsub(parttern_os_enr,'\\1',matches)
  names(result_os_enr)=NULL
  col_os_enr<-as.data.frame(matrix(result_os_enr, ncol=1, byrow=TRUE))
  names(col_os_enr)="Enrollment"
  
  # Combine data labels and instate enrolment
  e<-col_ins_enr
  Residency<-rep("In State", dim(e)[1])
  ee<-data.frame(Residency, col_labels, e)
  # combine data levels and out of statement enrollment
  f<-col_os_enr
  Residency<-rep("Out of State", dim(f)[1])
  ff<-data.frame(Residency, col_labels, f)
  # rowbind 2 datasets
  gg<-rbind(ee,ff)
  
  # combine traditional and distance enrollment
  
  hh<-rbind(dd,gg)
  
  
  # add variable school to the data set;
  
  School<-rep(college_name, dim(hh)[1])
  Term<-rep(term_name, dim(hh)[1])
  hhh<-cbind(Term, School, hh)
  
}





# APPLYING FUCNTIONS



# cmu













cmu_2019<-enrollement_webgrab("CMU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2019.html")
cmu_2018<-enrollement_webgrab("CMU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2018.html")
cmu_2017<-enrollement_webgrab("CMU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2017.html")
cmu_2016<-enrollement_webgrab("CMU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2016.html")
cmu_2015<-enrollement_webgrab("CMU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2015.html")
cmu_2014<-enrollement_webgrab("CMU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2014.html")
cmu_2013<-enrollement_webgrab("CMU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2013.html")

# function v2
cmu_2012<-enrollement_webgrab2("CMU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2012.html")
cmu_2011<-enrollement_webgrab2("CMU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2011.html")
cmu_2010<-enrollement_webgrab2("CMU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2010.html")

cmu_enr<-rbind(
  cmu_2019,
  cmu_2018,
  cmu_2017,
  cmu_2016,
  cmu_2015,
  cmu_2014,
  cmu_2013,
  cmu_2012,
  cmu_2011,
  cmu_2010	
)
write.csv(cmu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/cmu/cmu_2010-2019.csv", row.names=FALSE)






# end cmu






# emu







# emu


emu_2019<-enrollement_webgrab("EMU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2019.html")
emu_2018<-enrollement_webgrab("EMU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2018.html")
emu_2017<-enrollement_webgrab("EMU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2017.html")
emu_2016<-enrollement_webgrab("EMU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2016.html")
emu_2015<-enrollement_webgrab("EMU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2015.html")
emu_2014<-enrollement_webgrab("EMU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2014.html")
emu_2013<-enrollement_webgrab("EMU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2013.html")

# function v2
emu_2012<-enrollement_webgrab2("EMU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2012.html")
emu_2011<-enrollement_webgrab2("EMU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2011.html")
emu_2010<-enrollement_webgrab2("EMU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2010.html")

emu_enr<-rbind(
  emu_2019,
  emu_2018,
  emu_2017,
  emu_2016,
  emu_2015,
  emu_2014,
  emu_2013,
  emu_2012,
  emu_2011,
  emu_2010	
)
write.csv(emu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/emu/emu_2010-2019.csv", row.names=FALSE)






# end emu







# fsu







fsu_2019<-enrollement_webgrab("FSU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2019.html")
fsu_2018<-enrollement_webgrab("FSU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2018.html")
fsu_2017<-enrollement_webgrab("FSU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2017.html")
fsu_2016<-enrollement_webgrab("FSU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2016.html")
fsu_2015<-enrollement_webgrab("FSU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2015.html")
fsu_2014<-enrollement_webgrab("FSU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2014.html")
fsu_2013<-enrollement_webgrab("FSU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2013.html")

# function v2
fsu_2012<-enrollement_webgrab2("FSU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2012.html")
fsu_2011<-enrollement_webgrab2("FSU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2011.html")
#fsu_2010<-enrollement_webgrab2("FSU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2010.html")

fsu_enr<-rbind(
  fsu_2019,
  fsu_2018,
  fsu_2017,
  fsu_2016,
  fsu_2015,
  fsu_2014,
  fsu_2013,
  fsu_2012,
  fsu_2011
  # fsu_2010	
)
write.csv(fsu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/fsu/fsu_2010-2019.csv", row.names=FALSE)



# end fsu





# gvsu







gvsu_2019<-enrollement_webgrab("GVSU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2019.html")
gvsu_2018<-enrollement_webgrab("GVSU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2018.html")
gvsu_2017<-enrollement_webgrab("GVSU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2017.html")
gvsu_2016<-enrollement_webgrab("GVSU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2016.html")
gvsu_2015<-enrollement_webgrab("GVSU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2015.html")
gvsu_2014<-enrollement_webgrab("GVSU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2014.html")
gvsu_2013<-enrollement_webgrab("GVSU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2013.html")

# function v2
gvsu_2012<-enrollement_webgrab2("GVSU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2012.html")
gvsu_2011<-enrollement_webgrab2("GVSU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2011.html")
gvsu_2010<-enrollement_webgrab2("gvsu", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2010.html")

gvsu_enr<-rbind(
  gvsu_2019,
  gvsu_2018,
  gvsu_2017,
  gvsu_2016,
  gvsu_2015,
  gvsu_2014,
  gvsu_2013,
  gvsu_2012,
  gvsu_2011,
  gvsu_2010	
)
write.csv(gvsu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/gvsu/gvsu_2010-2019.csv", row.names=FALSE)





# end gvsu




# LSSU








lssu_2019<-enrollement_webgrab("LSSU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2019.html")
lssu_2018<-enrollement_webgrab("LSSU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2018.html")
lssu_2017<-enrollement_webgrab("LSSU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2017.html")
lssu_2016<-enrollement_webgrab("LSSU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2016.html")
lssu_2015<-enrollement_webgrab("LSSU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2015.html")
lssu_2014<-enrollement_webgrab("LSSU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2014.html")
lssu_2013<-enrollement_webgrab("LSSU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2013.html")

# function v2
lssu_2012<-enrollement_webgrab2("LSSU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2012.html")
#lssu_2011<-enrollement_webgrab2("LSSU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2011.html")
#lssu_2010<-enrollement_webgrab2("LSSU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2010.html")

lssu_enr<-rbind(
  lssu_2019,
  lssu_2018,
  lssu_2017,
  lssu_2016,
  lssu_2015,
  lssu_2014,
  lssu_2013,
  lssu_2012
  #lssu_2011,
  #lssu_2010	
)
write.csv(lssu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2010-2019.csv", row.names=FALSE)




# end LSSU




# MSU




# mus is msu

msu_2019<-enrollement_webgrab("MSU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2019.html")
msu_2018<-enrollement_webgrab("MSU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2018.html")
msu_2017<-enrollement_webgrab("MSU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2017.html")
msu_2016<-enrollement_webgrab("MSU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2016.html")
msu_2015<-enrollement_webgrab("MSU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2015.html")
msu_2014<-enrollement_webgrab("MSU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2014.html")
msu_2013<-enrollement_webgrab("MSU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2013.html")

# function v2
msu_2012<-enrollement_webgrab2("MSU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2012.html")
msu_2011<-enrollement_webgrab2("MSU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2011.html")
msu_2010<-enrollement_webgrab2("MSU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/mus_2010.html")

msu_enr<-rbind(
  msu_2019,
  msu_2018,
  msu_2017,
  msu_2016,
  msu_2015,
  msu_2014,
  msu_2013,
  msu_2012,
  msu_2011,
  msu_2010	
)
write.csv(msu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/msu/msu_2010-2019.csv", row.names=FALSE)




# end msu




# MTU







mtu_2019<-enrollement_webgrab("MTU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2019.html")
mtu_2018<-enrollement_webgrab("MTU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2018.html")
mtu_2017<-enrollement_webgrab("MTU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2017.html")
mtu_2016<-enrollement_webgrab("MTU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2016.html")
mtu_2015<-enrollement_webgrab("MTU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2015.html")
mtu_2014<-enrollement_webgrab("MTU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2014.html")
mtu_2013<-enrollement_webgrab("MTU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2013.html")

# function v2
mtu_2012<-enrollement_webgrab2("MTU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2012.html")
mtu_2011<-enrollement_webgrab2("MTU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2011.html")
mtu_2010<-enrollement_webgrab2("MTU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2010.html")

mtu_enr<-rbind(
  mtu_2019,
  mtu_2018,
  mtu_2017,
  mtu_2016,
  mtu_2015,
  mtu_2014,
  mtu_2013,
  mtu_2012,
  mtu_2011,
  mtu_2010	
)
write.csv(mtu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/mtu/mtu_2010-2019.csv", row.names=FALSE)




# end MTU




# NMU






nmu_2019<-enrollement_webgrab("NMU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2019.html")
nmu_2018<-enrollement_webgrab("NMU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2018.html")
nmu_2017<-enrollement_webgrab("NMU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2017.html")
nmu_2016<-enrollement_webgrab("NMU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2016.html")
nmu_2015<-enrollement_webgrab("NMU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2015.html")
nmu_2014<-enrollement_webgrab("NMU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2014.html")
nmu_2013<-enrollement_webgrab("NMU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2013.html")

# function v2
nmu_2012<-enrollement_webgrab2("NMU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2012.html")
nmu_2011<-enrollement_webgrab2("NMU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2011.html")
nmu_2010<-enrollement_webgrab2("NMU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2010.html")

nmu_enr<-rbind(
  nmu_2019,
  nmu_2018,
  nmu_2017,
  nmu_2016,
  nmu_2015,
  nmu_2014,
  nmu_2013,
  nmu_2012,
  nmu_2011,
  nmu_2010	
)
write.csv(nmu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/nmu/nmu_2010-2019.csv", row.names=FALSE)




# end NMU





# OU






lssu_2019<-enrollement_webgrab("LSSU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2019.html")
lssu_2018<-enrollement_webgrab("LSSU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2018.html")
lssu_2017<-enrollement_webgrab("LSSU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2017.html")
lssu_2016<-enrollement_webgrab("LSSU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2016.html")
lssu_2015<-enrollement_webgrab("LSSU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2015.html")
lssu_2014<-enrollement_webgrab("LSSU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2014.html")
lssu_2013<-enrollement_webgrab("LSSU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2013.html")

# function v2
lssu_2012<-enrollement_webgrab2("LSSU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2012.html")
#lssu_2011<-enrollement_webgrab2("LSSU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2011.html")
#lssu_2010<-enrollement_webgrab2("LSSU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2010.html")

lssu_enr<-rbind(
  lssu_2019,
  lssu_2018,
  lssu_2017,
  lssu_2016,
  lssu_2015,
  lssu_2014,
  lssu_2013,
  lssu_2012
  #lssu_2011,
  #lssu_2010	
)
write.csv(lssu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/lssu/lssu_2010-2019.csv", row.names=FALSE)




# end OU





# SVSU




svsu_2019<-enrollement_webgrab("SVSU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2019.html")
svsu_2018<-enrollement_webgrab("SVSU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2018.html")
svsu_2017<-enrollement_webgrab("SVSU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2017.html")
svsu_2016<-enrollement_webgrab("SVSU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2016.html")
svsu_2015<-enrollement_webgrab("SVSU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2015.html")
svsu_2014<-enrollement_webgrab("SVSU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2014.html")
svsu_2013<-enrollement_webgrab("SVSU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2013.html")

# function v2
#svsu_2012<-enrollement_webgrab2("SVSU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2012.html")
#svsu_2011<-enrollement_webgrab2("SVSU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2011.html")
#svsu_2010<-enrollement_webgrab2("SVSU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2010.html")

svsu_enr<-rbind(
  svsu_2019,
  svsu_2018,
  svsu_2017,
  svsu_2016,
  svsu_2015,
  svsu_2014,
  svsu_2013
  #svsu_2012,
  #svsu_2011,
  #svsu_2010	
)
write.csv(svsu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/svsu/svsu_2010-2019.csv", row.names=FALSE)



# end SVSU





# UMAA








umaa_2019<-enrollement_webgrab("UMAA", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2019.html")
umaa_2018<-enrollement_webgrab("UMAA", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2018.html")
umaa_2017<-enrollement_webgrab("UMAA", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2017.html")
umaa_2016<-enrollement_webgrab("UMAA", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2016.html")
umaa_2015<-enrollement_webgrab("UMAA", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2015.html")
umaa_2014<-enrollement_webgrab("UMAA", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2014.html")
umaa_2013<-enrollement_webgrab("UMAA", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2013.html")

# function v2
umaa_2012<-enrollement_webgrab2("UMAA", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2012.html")
umaa_2011<-enrollement_webgrab2("UMAA", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2011.html")
umaa_2010<-enrollement_webgrab2("UMAA", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2010.html")

umaa_enr<-rbind(
  umaa_2019,
  umaa_2018,
  umaa_2017,
  umaa_2016,
  umaa_2015,
  umaa_2014,
  umaa_2013,
  umaa_2012,
  umaa_2011,
  umaa_2010	
)
write.csv(umaa_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umaa/umaa_2010-2019.csv", row.names=FALSE)





# end UMAA






# UMD





umd_2019<-enrollement_webgrab("UMD", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2019.html")
umd_2018<-enrollement_webgrab("UMD", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2018.html")
umd_2017<-enrollement_webgrab("UMD", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2017.html")
umd_2016<-enrollement_webgrab("UMD", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2016.html")
umd_2015<-enrollement_webgrab("UMD", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2015.html")
umd_2014<-enrollement_webgrab("UMD", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2014.html")
umd_2013<-enrollement_webgrab("UMD", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2013.html")

# function v2
umd_2012<-enrollement_webgrab2("UMD", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2012.html")
umd_2011<-enrollement_webgrab2("UMD", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2011.html")
umd_2010<-enrollement_webgrab2("UMD", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2010.html")

umd_enr<-rbind(
  umd_2019,
  umd_2018,
  umd_2017,
  umd_2016,
  umd_2015,
  umd_2014,
  umd_2013,
  umd_2012,
  umd_2011,
  umd_2010	
)
write.csv(umd_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umd/umd_2010-2019.csv", row.names=FALSE)



# end UMD







# UMF





umf_2019<-enrollement_webgrab("UMF", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2019.html")
umf_2018<-enrollement_webgrab("UMF", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2018.html")
umf_2017<-enrollement_webgrab("UMF", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2017.html")
umf_2016<-enrollement_webgrab("UMF", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2016.html")
umf_2015<-enrollement_webgrab("UMF", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2015.html")
umf_2014<-enrollement_webgrab("UMF", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2014.html")
umf_2013<-enrollement_webgrab("UMF", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2013.html")

# function v2
#umf_2012<-enrollement_webgrab2("UMF", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2012.html")
umf_2011<-enrollement_webgrab2("UMF", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2011.html")
umf_2010<-enrollement_webgrab2("UMF", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2010.html")

umf_enr<-rbind(
  umf_2019,
  umf_2018,
  umf_2017,
  umf_2016,
  umf_2015,
  umf_2014,
  umf_2013,
  #umf_2012,
  umf_2011,
  umf_2010	
)
write.csv(umf_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/umf/umf_2010-2019.csv", row.names=FALSE)





# end UMF





# WSU






wsu_2019<-enrollement_webgrab("WSU", "Fall 2019", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2019.html")
wsu_2018<-enrollement_webgrab("WSU", "Fall 2018", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2018.html")
wsu_2017<-enrollement_webgrab("WSU", "Fall 2017", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2017.html")
wsu_2016<-enrollement_webgrab("WSU", "Fall 2016", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2016.html")
wsu_2015<-enrollement_webgrab("WSU", "Fall 2015", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2015.html")
wsu_2014<-enrollement_webgrab("WSU", "Fall 2014", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2014.html")
wsu_2013<-enrollement_webgrab("WSU", "Fall 2013", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2013.html")

# function v2
wsu_2012<-enrollement_webgrab2("WSU", "Fall 2012", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2012.html")
wsu_2011<-enrollement_webgrab2("WSU", "Fall 2011", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2011.html")
wsu_2010<-enrollement_webgrab2("WSU", "Fall 2010", "C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2010.html")

wsu_enr<-rbind(
  wsu_2019,
  wsu_2018,
  wsu_2017,
  wsu_2016,
  wsu_2015,
  wsu_2014,
  wsu_2013,
  wsu_2012,
  wsu_2011,
  wsu_2010	
)
write.csv(wsu_enr, file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/wsu/wsu_2010-2019.csv", row.names=FALSE)



# end WSU













# combind all datasets

all_enr<-rbind(
  cmu_enr,
  emu_enr,
  fsu_enr,
  gvsu_enr, 
  lssu_enr,
  lssu_enr,
  msu_enr,
  mtu_enr,
  nmu_enr,
  ou_enr,
  svsu_enr,
  umaa_enr,
  umd_enr,
  umf_enr,
  wsu_enr,
  wmu_enr
  )

dim(all_enr)

write.csv(all_enr,
          file="C:/Users/shh6304/Documents/01_Tasks/20191023_working_folder_enrollment_MI/all_enr_2010-2019.csv", 
          row.names=FALSE)
