




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

