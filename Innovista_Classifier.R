# Example inputs
var_b<-0.851544922472933 # dataset_b1: row 1, class = bo


var_o<-c(0.8178090360475355, 0.9786911275448799,
         0.00392683539593758,	0.812027417171962,	
         0.00520251761289356,	0.8969672067652326,	
         0.0485203912575636,	0.022907260957708543) # dataset_o1: row 1, class = o6

var_of<-c(NA, "0.9786911275448799",
         0.00392683539593758,	0.812027417171962,	
         0.00520251761289356,	0.8969672067652326,	
         0.0485203912575636,	'a string')


var_c<-read.csv('c0.csv', header = F)
c1<-var_c$V2 # This is the input values vector
c2<-var_c$V1  # This is the true values vector 



#-------------START OF FUNCTION-------------
classify<-function()
{
  j<-numeric()
  n<-numeric()
  wd<-getwd() 
  packs<-c('nnet', 'stringr', 'svDialogs', 'class', 'ggplot2', 'patchwork')
  if (all(packs %in% (.packages()))==F)
  {
    pack_check<-readline(prompt = 'Install and load required packages? 1 for yes, 0 to exit: ')
    if(as.numeric(pack_check) == 1){
      if (!require(nnet)){
        install.packages('nnet')
        library(nnet)
      }
      if (!require(svDialogs)){
        install.packages('svDialogs')
        library(svDialogs)
      }
      if (!require(stringr)){
        install.packages('stringr')
        library(stringr)
      }
      if (!require(class)){
        install.packages('class')
        library(class)
      }
      if (!require(ggplot2)){
        install.packages('ggplot2')
        library(ggplot2)
      }
      if (!require(patchwork)){
        install.packages('patchwork')
        library(patchwork)
      }
    }else if(as.numeric(pack_check) == 0){
      stop("Packages not installed/loaded, execution stoped")
    }else{
      print("Incorrect input")
    }
  }
  file_other<-c("other")
  file_check<-list.files(path = getwd(), pattern = c("(^c.*|^dataset.*)\\.csv$"), full.names = F)
  file_options<-append(file_other, file_check)
  file_menu<-dlg_list(file_options, title = "Select a file", gui = .GUI)
  file_result<-file_menu$res
  if (file_result=='other'){
    menu_other<-dlg_input('Enter names of training datasets, include .csv/txt/etc, do not include quotes, seperate by commas', default = '', gui = .GUI)
    file_name<-menu_other$res
    file_name<-unlist(strsplit(file_name, ","))
    file_name<-trimws(file_name)
  }else{
    file_name<-file_result
  }
  for (name in file_name){
    if (file.exists(name) == FALSE)
    {
      dlg_message('File is not in working directory', gui = .GUI)
      tempdr<-dlg_dir(default = getwd(), title = "Choose file location", gui = .GUI)$res
      setwd(tempdr)
    }
    train_file<-read.csv(name, header = F)
  }
  if (ncol(train_file) == 9){
    use<-'O_Classifier'
  }else if (ncol(train_file)==2){
    use<-'C_Classifier'
  }else{
    stop("Dataset does not contain 2 or 9 columns. Cannot classify")
  }
  if (use == "C_Classifier")
  {
    vec_check<-dlg_list(c("yes                                                          
           
           ", "no                                                          
           
           "), title = "Do you have an existing input vector?       ", gui = .GUI)$res
    vec_check<-trimws(vec_check)
    if (vec_check=='yes')
    {
      c_input<-(dlg_input('Enter your vector name (variable name)', default = '', gui = .GUI)$res)
      c_input<-eval(as.name(paste(c_input)))
      suppressWarnings({for (i in c_input){
        j<-append(j, which(is.na(c_input)))
        if (is.character(c_input)){
          for (i in c_input){
            if (is.na(as.numeric(i))){
              n<-append(n, which(input == i))
            }  
          }
        }
      }})
      l<-sort(unique(append(unique(n), unique(j))))
      if(length(l)>0){
        p<-('Strings or NAs at positions:')
        oops<-cat(p,'\n', l, '\n')
        stop(oops)
      }
      
    }
    if (vec_check=='no')
    {
      c_input<-(dlg_input('Enter a vector of numeric values', default = '', gui = .GUI)$res)
      c_input<-(unlist(strsplit(c_input, ",", fixed=TRUE)))
      suppressWarnings({for (i in c_input){
        j<-append(j, which(is.na(c_input)))
        if (is.character(c_input)){
          for (i in c_input){
            if (is.na(as.numeric(i))){
              n<-append(n, which(c_input == i))
            }  
          }
        }
      }})
      l<-sort(unique(append(unique(n), unique(j))))
      if(length(l)>0){
        p<-('Strings or NAs at positions:')
        oops<-cat(p,'\n', l, '\n')
        stop(oops)
      }
      c_input<-as.numeric(c_input)
    }
    c_input<-as.data.frame(c_input)
    kv<-as.integer(sqrt(nrow(train_file)))
    preds<-knn(train = as.data.frame(train_file$V2), test = as.data.frame(c_input), cl=train_file$V1, k = kv)
    results<-data.frame(Input = c_input,
                        Predictions = preds)
    results$Predictions<-gsub("\\..*", "", results$Predictions)
    if (nrow(results) == 1){
      colnames(results)<-c('Input', "Predictions")
      l<-paste("Predicted Class:", results$Predictions)
      g<-'Results assigned to global environment'
      cat(l,'\n', g, '\n')
    }else{
      vec_check2<-dlg_list(c("yes                                                          
           
           ", "no                                                          
           
           "), title = "Do you have an existing vector of true values?       ", gui = .GUI)$res
      vec_check2<-trimws(vec_check2)
      if (vec_check2=='no'){
        colnames(results)<-c('Input', "Predictions")
        print('Results assigned to global environment')
      }
      if (vec_check2=='yes'){
        z<-(dlg_input('Enter the name of the vector of True Values', default = '', gui = .GUI)$res)
        z<-eval(as.name(paste(z)))
        results<-data.frame(Input = c_input,
                            Predictions = preds,
                            True_Values = z)
        colnames(results)<-c('Input', "Predictions", 'True_Values')
        results$tf<-ifelse(results$Predictions==results$True_Values, T, F)
        acc<-(nrow(results[results$Predictions == results$True_Values,])/nrow(results))*100
        h<-paste(acc, "%", sep = "")
        l<-paste("Accuracy:", h)
        g<-'Results assigned to global environment'
        cat(l,'\n', g, '\n')
      }
      if (ncol(results)==2){
        p<-ggplot(results, aes(x=Predictions))+ 
          geom_bar(color = 'black', fill = "lightblue", width = .25)+ 
          ggtitle('Count of Predicted Classes')
        print(p)
      }
      if (ncol(results)==4){
        lvl<-levels(results$Predictions)
        lvl1<-lvl[1]
        lvl2<-lvl[2]
        grp1<-subset(results, Predictions==lvl1)
        grp2<-subset(results, Predictions==lvl2)
        grp3<-data.frame(
          group = c(T, F),
          value = c(acc, (100-acc)),
          perc = c((paste(acc, '%', sep = '')), (paste((100-acc), '%', sep = ''))
        )
        )
        p1<-ggplot(grp1, aes(x=tf))+ 
          geom_bar(color = 'black', fill = "lightblue", width = .25)+
          ggtitle(paste('Count of Predictions for:', lvl1, sep = ""))
        p2<-ggplot(results, aes(x=tf)) + 
          geom_bar(color = 'black', fill = "steelblue", width = .25)+
          ggtitle(paste('Count of Predictions for:', lvl2, sep=""))
        p3<-ggplot(grp3, aes(x="", y = value, fill = group))+
          geom_bar(stat = 'identity', width = 1, color = 'white') +
          geom_text(aes(label=perc),
                    position = position_stack(vjust = 0.5))+
          coord_polar("y", start = 0)+
          theme_void()
        print(p3 |(p1/p2))
       
      }
    }
  }
  if (use == 'O_Classifier'){
    train_file$f<-factor(train_file$V1)
    train_file$f<-relevel(train_file$f, ref = 'o0')
    model1<-multinom(f ~ V2+V3+V4+V5+V6+V7+V8+V9, data=train_file, trace=F)
    vec_check<-dlg_list(c("yes                                                          
           
           ", "no                                                          
           
           "), title = "Do you have an existing input vector?       ", gui = .GUI)$res
    vec_check<-trimws(vec_check)
    if (vec_check=='yes')
    {
      x_pre<-(dlg_input('Enter your vector name (variable name)', default = '', gui = .GUI)$res)
      x<-eval(as.name(paste(x_pre)))
      suppressWarnings({for (i in x){
        j<-append(j, which(is.na(x)))
        if (is.character(x)){
          for (i in x){
            if (is.na(as.numeric(i))){
              n<-append(n, which(x == i))
            }  
          }
        }
      }})
      l<-sort(unique(append(unique(n), unique(j))))
      if(length(l)>0){
        p<-('Strings or NAs at positions:')
        oops<-cat(p,'\n', l, '\n')
        stop(oops)
      }
    }
    if (vec_check=='no')
    {
      x<-(dlg_input('Enter a vector of numeric values', default = '', gui = .GUI)$res)
      x<-(unlist(strsplit(x, ",", fixed=TRUE)))
      suppressWarnings({for (i in x){
        j<-append(j, which(is.na(x)))
        if (is.character(x)){
          for (i in x){
            if (is.na(as.numeric(i))){
              n<-append(n, which(input == i))
            }  
          }
        }
      }})
      l<-sort(unique(append(unique(n), unique(j))))
      if(length(l)>0){
        p<-('Strings or NAs at positions:')
        oops<-cat(p,'\n', l, '\n')
        stop(oops)
      }
      x<-as.numeric(x)
    }
    if (length(x) != 8){
      stop("Input has more or less than 8 values")
    }else{
      
      y<-data.frame(
        V2=numeric(),
        V3=numeric(),
        V4=numeric(),
        V5=numeric(),
        V6=numeric(),
        V7=numeric(),
        V8=numeric(),
        V9=numeric()
      )
      d<-colnames(y)
      y<-rbind(x)
      colnames(y)<-d
      v<-predict(model1, y)
      td<-paste('Results assigned to global environment')
      pc<-paste("Predicted Class:", as.character(v))
      cat(td,'\n', pc, '\n')
      results<-v
    }
  }
  if (getwd()!=wd)
  {
    setwd(wd)
    w<-getwd()
    m<-"Working directory restored to:"
    paste(m, w)
  }
  vrbl<-head(sort(ls(.GlobalEnv, pattern = "^results..+"), decreasing = T), 1)
  if (length(vrbl)>0){
    vr1<-str_extract(vrbl, '\\d+')
    vr2<-str_extract(vrbl, '[a-z]+')
    vr1<-as.numeric(vr1)+1
    vr4<-paste(vr2,'.',vr1, sep = '' )
    assign(vr4, results, envir = .GlobalEnv) 
  }else{
    assign('results.1', results, envir = .GlobalEnv)
  }
}
#-------------END OF FUNCTION-------------


classify() # AFTER INITIZILING CALL THE FUNCTIION LIKE THIS, NO PARAMETERS OR ASSIGNMENTS, IT WILL DO IT ALL FOR YOU!
