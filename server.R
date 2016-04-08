# coding by Yulong Deng, updated 2016-4-8 
#=====================================================================================

library(shiny)

#library(DT)
#library(RWeka)

library(tm)

# load n-gram freq table files

load("my_tdm_freq_n_1_gram_sorted.Rdata")   
load("my_tdm_freq_n_2_gram_sorted.Rdata") 
load("my_tdm_freq_n_3_gram_sorted.Rdata") 
load("my_tdm_freq_n_4_gram_sorted.Rdata") 



#-------------------------------------------------------------------
# function: get_last_word , get the last word from a given string.

get_last_word <- function(input_str){
    
    output_str <- sub(gsub("[^ ]*$","",input_str),"",input_str)
    
    output_str
    
}




#-----------------------------------------------------------------------------------
# function: input_process
# given a input string, make the process: 
# trans to lower case
# remove all numbers
# remove punctuations (but keep the ', eg. he's  whill keep as he's)
# remove the writespace at the head or tail of the string as " hello "
# remove strange "" (not " ") contained in the string



input_process <- function(input_raw){
    
    my_input <- tolower(input_raw)
    my_input <- removeNumbers(my_input)
    
    # my_input <- removePunctuation(my_input), not used because the function will
    # also remove ' in the string as (he's will be trans to hes, i'll will be trans
    # to ill, using code below instead.
    
    # replace symbols with " " ,except of these symbols: alphanumeric, space, ', -, * 
    
    my_input <- gsub("[^[:alnum:][:space:]'-*]", " ", my_input)
    
    # remove leading and trailing whitespace
    
    my_input <- gsub("^\\s+|\\s+$", "",my_input)
    
    # using tm package to make use of the function: the strip writespace
    
    input <- VCorpus(VectorSource(my_input))
    
    # remove ""
    
    my_input <- tm_map(input, stripWhitespace)
    
    # return the string processed
    
    my_input[[1]]$content
}






#-----------------------------------------------------------------------------------
# function: pred_nextword
# 
#  given a phrase, output 10 predicted word, ordered as possibility.
# 
# stupid back off model 
# 
# parameters of the model:  
# 
# px = c1*p1 + c2*p2 + c3*p3 + c4*p4  when given a three words (or more) phrase,  output word x.
# eg. input: "a case of" , output "beer".
# p4 = p("beer"|"a case of") = c("a case of beer")/c("a case of") 
#    = c("a case of beer")/c("a case of x1", "a case of x2",..., "a case of xn") 
# p3 = p("beer"|"case of") = c("case of beer")/c("case of")
#    = c("case of beer")/c("case of x1", "case of x2",..., "case of xn") 
# p2 = p("beer"|"of") = c("of beer")/c("of")
#    = c("of beer")/c("of x1", "of x2",..., "of xn")  
# p1 = p("beer") = c("beer")/c("x1",..,"xn")
#
#
# c1+c2+c3+c4 = 1, initial: c1=0.1, c2=0.2, c3=0.3, c4=0.4  , using c_4 = c(0.1, 0.2,0.3, 0.4)


# px = c1*p1 + c2*p2 + c3*p3  when given a two words phrase,  output word x.
# eg. input: "case of" , output "beer".
# p3 = p("beer"|"case of") = c("case of beer")/c("case of")
#    = c("case of beer")/c("case of x1", "case of x2",..., "case of xn") 
# p2 = p("beer"|"of") = c("of beer")/c("of")
#    = c("of beer")/c("of x1", "of x2",..., "of xn")  
# p1 = p("beer") = c("beer")/c("x1",..,"xn")
#
#
# c1+c2+c3 =1, initial: c1=0.2, c2=0.3, c3=0.5, using c_3 = c(0.2, 0.3, 0.5)


# px = c1*p1 + c2*p2  when only given a word,  output word x.
# eg. input: "of" , output "beer".
# p2 = p("beer"|"of") = c("of beer")/c("of")
#    = c("of beer")/c("of x1", "of x2",..., "of xn")  
# p1 = p("beer") = c("beer")/c("x1",..,"xn")
#
#
# c1+c2 =1, initial: c1=0.3, c2=0.7, using c_2 = c(0.3, 0.7)


# parameter setting before invoke function pred_nextword

# c_2 = c(0.3, 0.7)
# c_3 = c(0.2, 0.3, 0.5)
# c_4 = c(0.1, 0.2,0.3, 0.4)

 c_2 = c(0.2, 0.8)
 c_3 = c(0.1, 0.45, 0.45)
 c_4 = c(0.05, 0.3,0.3, 0.35)

# my loop counts for seraching work

my_loop_counts <- 3

pred_nextword <- function(input_str){
    
    
    # some variables defined      
    temp_name <- NULL
    words_name <- NULL
    words_prob <- NULL
    words_count <- NULL
    
    
    p4 <- 0
    p3 <- 0
    p2 <- 0
    p1 <- 0
    
    p_sum_v <- NULL
    
    
    # split every words in input_str and get a vector ssv
    ss <- strsplit(input_str, " ")
    ssv <- unlist(ss)
    ssv_length <- length(ssv)
    
    
    
    #<----------------------
    
    #remove the 's from the last word if the last word is not he's, she's and it's
 
    my_tempwords <- c("he's", "she's", "it's")
    if(grepl("'s$",ssv[ssv_length]))
        if(!(ssv[ssv_length] %in% my_tempwords)){
            
            print("hi, i am here")
            
            ssv[ssv_length] <- gsub("'s$","",ssv[ssv_length])
            
        }  
    
    
    #<----------------------
    
   
    
    
    
    
    
    # make the strings search_str3, search_str2, search_str1   
    # ps3--trigram, ps2--bigram, ps1-- unique gram
    # eg: input_str is  "a case of" or "and a case of"  
    # search_str3: "^a case of ."
    # search_str2: "^case of ."
    # search_str1: "^of ."
    
    # do3 = TRUE, do2 =FALSE, do1 = FALSE, the first task in tasks set is started from looking up 
    # the search_str3 from my_tdm_freq_n_4_gram_sorted; if can not found, then do2 =TRUE, do1 = FALSE,
    # the first task in tasks set is started from looking up the search_str2 from my_tdm_freq_n_3_gram_sorted; 
    # if can not found, then do1 = TRUE, the first task in tasks set is started from looking up the 
    # search_str1 from my_tdm_freq_n_2_gram_sorted
    
    # in a tasks set, all of the px will be work out, then the found word will be sorted on px order. 
    # top 10 words will be returned from the function.
    
    if(ssv_length >= 3){
        
        ps3 <- paste(ssv[ssv_length - 2], ssv[ssv_length - 1], ssv[ssv_length],sep=" ")
        ps2 <- paste(ssv[ssv_length - 1], ssv[ssv_length],sep=" ")
        ps1 <- ssv[ssv_length]
        
        do3 <- TRUE
        do2 <- FALSE
        do1 <- FALSE
        
        search_str3 <- paste("^", ps3, " .", sep="")
        search_str2 <- paste("^", ps2, " .", sep="")
        search_str1 <- paste("^", ps1, " .", sep="")
        
    } else if(ssv_length == 2){
        
        ps2 <- paste(ssv[ssv_length - 1], ssv[ssv_length],sep=" ")
        ps1 <- ssv[ssv_length]
        
        do3 <- FALSE
        do2 <- TRUE
        do1 <- FALSE
        
        search_str2 <- paste("^", ps2, " .", sep="")
        search_str1 <- paste("^", ps1, " .", sep="")
        
    } else if(ssv_length == 1){
        
        ps1 <- ssv[ssv_length]
        
        do3 <- FALSE
        do2 <- FALSE
        do1 <- TRUE
        
        search_str1 <- paste("^", ps1, " .", sep="")
        
    } else {
        
        do3 <- FALSE
        do2 <- FALSE
        do1 <- FALSE
        
        
        cat("error,nothing input!\n")
    }
    
  
    

    
    # do3 == TRUE ,the input_str must contain 3 words or more, looking for search_str3, search_str2, 
    # search_str1 and the last word ,step by step, in each step, work out the p4, p3, p2, p1 respectively.
    # then given top my_loop_counts predicted words with their px ordered.
    
    if(do3 == TRUE){
        
        #------------ step 1-------------------
        # eg. search_str3: "a case of"
        temp_index <- grep(search_str3, names(my_tdm_freq_n_4_gram_sorted))
        
        length_temp_index <- length(temp_index)
        
        # found search_str3 in my_tdm_freq_n_4_gram_sorted         
        
        if(length_temp_index != 0){
            
            do2 <- FALSE
            
            do1 <- FALSE
            
            temp_set <- my_tdm_freq_n_4_gram_sorted[temp_index]
            
            temp_set_sorted <-  sort(temp_set, decreasing = TRUE)
            
            temp_set_sorted_value <- as.numeric(temp_set_sorted)
            
            temp_set_sorted_value_sum <- sum(temp_set_sorted_value)
            
            length_temp_set_sorted <- length_temp_index
            
            # work out p4.
            # e.g   p("beer"|"a case of") = c("a case of beer")/c("a case of")
            # =  c("a case of beer")/c("a case of x1", "a case of x2",..., "a case of xn")
            temp_set_sorted_p <- temp_set_sorted_value/temp_set_sorted_value_sum
            
            # only work for my_loop_counts (or less than my_loop_counts) items found
            loop_counts <- my_loop_counts
            
            if(length_temp_set_sorted < my_loop_counts){
                
                loop_counts <- length_temp_set_sorted
                
            }
            
            
            #             cat("found ")
            #             
            #             cat(length_temp_set_sorted)
            #             
            #             cat(" matched items ")
            #             
            #             cat("\n")
            
            
            # in the tasks set, all of the px will be work out ( based on p4,p3,p2,p1)
            
            for(i in 1:loop_counts){
                
                names_temp_set_sorted_i <- names(temp_set_sorted[i])
                
                #                 cat(" matched item ")
                #                 
                #                 cat(i)
                #                 
                #                 cat("\n")
                #                 
                #                 cat("the p of ")
                #                 
                #                 cat(names_temp_set_sorted_i)
                #                 
                #                 cat(":")
                #                 
                #                 cat(temp_set_sorted_p[i])
                #                 
                #                 cat("\n")
                
                p4 <- temp_set_sorted_p[i]
                
                #------------ step 2-------------------
                
                input_str <- names_temp_set_sorted_i
                
                # split every words in input_str
                # eg. input_str here: "a case of beer"
                
                ss <- strsplit(input_str, " ")
                
                ssv <- unlist(ss)
                
                ps2 <- paste(ssv[2], ssv[3], sep=" ")
                
                # eg. search_str2: "^case of ."
                search_str2 <- paste("^", ps2, " .", sep="")
                
                # eg. want_str3 here: "^case of beer$"
                want_str3 <- paste("^", ssv[2], " ", ssv[3], " ", ssv[4], "$", sep ="")
                
                want_str3_p <- 0  # init 0
                
                temp_index_1 <- grep(search_str2, names(my_tdm_freq_n_3_gram_sorted))
                
                length_temp_index_1 <- length(temp_index_1)
                
                if(length_temp_index_1 != 0){
                    
                    temp_set_1 <- my_tdm_freq_n_3_gram_sorted[temp_index_1]
                    
                    temp_set_1_sorted <-  sort(temp_set_1, decreasing = TRUE)
                    
                    temp_set_1_sorted_value <- as.numeric(temp_set_1_sorted)
                    
                    temp_set_1_sorted_value_sum <- sum(temp_set_1_sorted_value)
                    
                    temp_index_2 <- grep(want_str3, names(temp_set_1_sorted))
                    
                    length_temp_index_2 <- length(temp_index_2)
                    
                    if(length_temp_index_2 != 0){
                        
                        want_found <- temp_set_1_sorted[temp_index_2]
                        
                        want_str3_value <- as.numeric(want_found)
                        
                        # work out p3.
                        # e.g   p("beer"|"case of") = c("case of beer")/c("case of")
                        # =  c("case of beer")/c("case of x1", "case of x2",..., "case of xn")
                        want_str3_p <- want_str3_value/temp_set_1_sorted_value_sum
                        
                    }
                    
                }
                
                #                 cat("the p of ")
                #                 
                #                 cat(want_str3)
                #                 
                #                 cat(":")
                #                 
                #                 cat(want_str3_p)
                #                 
                #                 cat("\n")
                
                p3 <- want_str3_p
                
                #------------ step 3-------------------
                
                ps1 <- ssv[3]
                
                # eg. search_str1: "^of ."
                search_str1 <- paste("^", ps1, " .", sep="")
                
                # eg. want_str2: "^of beer$"
                want_str2 <- paste("^", ssv[3], " ", ssv[4], "$", sep ="")
                
                want_str2_p <- 0  # init 0
                
                temp_index_1 <- grep(search_str1, names(my_tdm_freq_n_2_gram_sorted))
                
                length_temp_index_1 <- length(temp_index_1)
                
                if(length_temp_index_1 != 0){
                    
                    temp_set_1 <- my_tdm_freq_n_2_gram_sorted[temp_index_1]
                    
                    temp_set_1_sorted <-  sort(temp_set_1, decreasing = TRUE)
                    
                    temp_set_1_sorted_value <- as.numeric(temp_set_1_sorted)
                    
                    temp_set_1_sorted_value_sum <- sum(temp_set_1_sorted_value)
                    
                    temp_index_2 <- grep(want_str2, names(temp_set_1_sorted))
                    
                    length_temp_index_2 <- length(temp_index_2)
                    
                    if(length_temp_index_2 != 0){
                        
                        want_found <- temp_set_1_sorted[temp_index_2]
                        
                        want_str2_value <- as.numeric(want_found)
                        
                        # work out p2.
                        # e.g   p("beer"|"of") = c("of beer")/c("of")
                        # =  c("of beer")/c("of x1", "of x2",..., "of xn")
                        want_str2_p <- want_str2_value/temp_set_1_sorted_value_sum
                        
                    }
                    
                }
                
                #                 cat("the p of ")
                #                 
                #                 cat(want_str2)
                #                 
                #                 cat(":")
                #                 
                #                 cat(want_str2_p)
                #                 
                #                 
                #                 
                #                 cat("\n")
                
                p2 <- want_str2_p 
                
                #------------ step 4 -------------------
                
                ps0 <- ssv[4]
                
                # eg. search_str0: "^beer$"
                search_str0 <- paste("^", ps0, "$", sep="")
                
                # eg. want_str1: "beer"
                want_str1 <- ssv[4]
                
                want_str1_p <- 0  # init 0
                
                temp_index_1 <- grep(search_str0, names(my_tdm_freq_n_1_gram_sorted))
                
                length_temp_index_1 <- length(temp_index_1)
                
                if(length_temp_index_1 != 0){
                    
                    want_found <- my_tdm_freq_n_1_gram_sorted[temp_index_1]
                    
                    temp_set_0_sorted_value <- as.numeric(my_tdm_freq_n_1_gram_sorted)
                    
                    temp_set_0_sorted_value_sum <- sum(temp_set_0_sorted_value)
                    
                    want_str1_value <- as.numeric(want_found)
                    
                    # work out p1.
                    # e.g   p("beer") = c("beer")/c("x")
                    # =  c("beer")/c("x1", "x2",..., "xn")
                    want_str1_p <- want_str1_value/temp_set_0_sorted_value_sum
                    
                }
                
                #                 cat("the p of ")
                #                 
                #                 cat(want_str1)
                #                 
                #                 cat(":")
                #                 
                #                 cat(want_str1_p)
                #                 
                #                 cat("\n")
                
                p1 <- want_str1_p
                
                #------------ step 5 -------------------
                
                # work out px
                p_sum <- p4*c_4[4] + p3*c_4[3] + p2*c_4[2] + p1*c_4[1]
                
                #                 cat("the sum of p:")
                #                 
                #                 cat(p_sum)
                #                 
                #                 cat("\n")
                
                #the vector of px
                p_sum_v <- c(p_sum_v, p_sum)
                
            }
            
            
            
            #------------ step 6 -------------------
            
            # make the px ordered by possibility
            
            k <- order(p_sum_v, decreasing = TRUE)
            
            p_sum_v <- sort(p_sum_v, decreasing = TRUE)
            
            temp_set_sorted_new <- NULL
            
            for( j in 1:loop_counts){
                
                temp_set_sorted_new <- c(temp_set_sorted_new, temp_set_sorted[k[j]])
                
            }
            
            for( m in 1:loop_counts){
                
                temp_name[m] <- get_last_word(names(temp_set_sorted_new[m]))
                
                #                 cat(names(temp_set_sorted_new[m]))
                #                 
                #                 cat("; ")
                
                
            } 
            
            #------------ step 7 -------------------
            # make the final results (my_loop_counts or less words predicted )in the tasks set  
            
            words_name <- temp_name
            words_prob <- p_sum_v
            words_count <- loop_counts
            
            
            
        }else{
            
            do2 <- TRUE
            
            do1 <- FALSE
        } 
        
    }     
    
    
    
    # do2 == TRUE ,the input_str must contain 2 words or more, looking for search_str2, 
    # search_str1 and the last word ,step by step, in each step, work out the p3, p2, p1 respectively.
    # then given top my_loop_counts predicted words with their px ordered.
    
    #------------ step 1-------------------
    # eg. search_str2: "case of"
    
    if(do2 == TRUE){
        
        do1 <- FALSE
        
        temp_index <- grep(search_str2, names(my_tdm_freq_n_3_gram_sorted))
        
        length_temp_index <- length(temp_index)
        
        if(length_temp_index != 0){
            
            
            temp_set <- my_tdm_freq_n_3_gram_sorted[temp_index]
            
            temp_set_sorted <- sort(temp_set, decreasing = TRUE)
            
            temp_set_sorted_value <- as.numeric(temp_set_sorted)
            
            temp_set_sorted_value_sum <- sum(temp_set_sorted_value)
            
            length_temp_set_sorted <- length_temp_index
            
            # work out p3.
            # eg. p("beer"|"case of") = c("case of beer")/c("case of)
            # = c("case of beer")/c("case of x1","case of x2",..,"case of xn")
            temp_set_sorted_p <- temp_set_sorted_value/temp_set_sorted_value_sum
            
            
            # only work for my_loop_counts(or less than my_loop_counts) items found
            
            loop_counts <- my_loop_counts
            
            if(length_temp_set_sorted < my_loop_counts){
                
                loop_counts <- length_temp_set_sorted
                
            }
            
            #             
            #             cat("found ")
            #             
            #             cat(length_temp_set_sorted)
            #             
            #             cat(" matched items ")
            #             
            #             cat("\n")
            
            # in the tasks set, all of the px will be work out (based on p3, p2, p1)
            
            for(i in 1:loop_counts){
                
                names_temp_set_sorted_i <- names(temp_set_sorted[i])
                
                #                 cat(" matched item ")
                #                 
                #                 cat(i)
                #                 
                #                 cat("\n")
                #                 
                #                 cat("the p of ")
                #                 
                #                 cat(names_temp_set_sorted_i)
                #                 
                #                 cat(":")
                #                 
                #                 cat(temp_set_sorted_p[i])
                #                 
                #                 cat("\n")
                
                p3 <- temp_set_sorted_p[i]
                
                #------------ step 2-------------------
                
                input_str <- names_temp_set_sorted_i
                
                # split every words in input_str
                # eg. input_str here: "case of beer"
                
                ss <- strsplit(input_str, " ")
                
                ssv <- unlist(ss)
                
                ps1 <- ssv[2]
                
                # eg. search_str1: "^of ."
                search_str1 <- paste("^", ps1, " .", sep="")
                
                #eg. want_str2 here: "^of beer$"
                want_str2 <- paste("^",ssv[2]," ", ssv[3], "$", sep ="")
                
                want_str2_p <- 0  # init 0
                
                temp_index_1 <- grep(search_str1, names(my_tdm_freq_n_2_gram_sorted))
                
                length_temp_index_1 <- length(temp_index_1)
                
                if(length_temp_index_1 != 0){
                    
                    temp_set_1 <- my_tdm_freq_n_2_gram_sorted[temp_index_1]
                    
                    temp_set_1_sorted <-  sort(temp_set_1, decreasing = TRUE)
                    
                    temp_set_1_sorted_value <- as.numeric(temp_set_1_sorted)
                    
                    temp_set_1_sorted_value_sum <- sum(temp_set_1_sorted_value)
                    
                    temp_index_2 <- grep(want_str2, names(temp_set_1_sorted))
                    
                    length_temp_index_2 <- length(temp_index_2)
                    
                    if(length_temp_index_2 != 0){
                        
                        want_found <- temp_set_1_sorted[temp_index_2]
                        
                        want_str2_value <- as.numeric(want_found)
                        
                        # work out p2.
                        # e.g. p("beer"|"of") = c("of beer")/c("of")
                        # = c("of beer")/c("of x1","of x2",..,"of xn")
                        want_str2_p <- want_str2_value/temp_set_1_sorted_value_sum
                        
                    }
                    
                }
                
                #                 cat("the p of ")
                #                 
                #                 cat(want_str2)
                #                 
                #                 cat(":")
                #                 
                #                 cat(want_str2_p)
                #                 
                #                 cat("\n")
                
                p2 <- want_str2_p
                
                
                #------------ step 3 -------------------
                
                ps0 <- ssv[3]
                
                # e.g search_str0 : "^beer$"
                search_str0 <- paste("^", ps0, "$", sep="")
                
                # eg. want_str1: "beer"
                want_str1 <- ssv[3]
                
                want_str1_p <- 0  # init 0
                
                temp_index_1 <- grep(search_str0, names(my_tdm_freq_n_1_gram_sorted))
                
                length_temp_index_1 <- length(temp_index_1)
                
                if(length_temp_index_1 != 0){
                    
                    want_found <- my_tdm_freq_n_1_gram_sorted[temp_index_1]
                    
                    temp_set_0_sorted_value <- as.numeric(my_tdm_freq_n_1_gram_sorted)
                    
                    temp_set_0_sorted_value_sum <- sum(temp_set_0_sorted_value)
                    
                    want_str1_value <- as.numeric(want_found)
                    
                    # work out p1.
                    # e.g p("beer") = c("beer")/c("x")
                    # = c("beer")/c("x1","x2",..,"xn")
                    want_str1_p <- want_str1_value/temp_set_0_sorted_value_sum
                    
                }
                
                #                 cat("the p of ")
                #                 
                #                 cat(want_str1)
                #                 
                #                 cat(":")
                #                 
                #                 cat(want_str1_p)
                #                 
                #                 cat("\n")
                
                p1 <- want_str1_p
                
                #------------ step 4 ------------------- 
                
                #work out px
                p_sum <- p3*c_3[3] + p2*c_3[2] + p1*c_3[1]
                
                #                 cat("the sum of p:")
                #                 
                #                 cat(p_sum)
                #                 
                #                 cat("\n")
                
                # the vector of px
                p_sum_v <- c(p_sum_v, p_sum)
                
            }
            
            
            #------------ step 5 ------------------- 
            
            # make the px ordered by possibility
            
            k <- order(p_sum_v, decreasing = TRUE)
            
            p_sum_v <- sort(p_sum_v, decreasing = TRUE)
            
            temp_set_sorted_new <- NULL
            
            for( j in 1:loop_counts){
                
                temp_set_sorted_new <- c(temp_set_sorted_new, temp_set_sorted[k[j]])
                
            }
            
            
            for( m in 1:loop_counts){
                
                temp_name[m] <- get_last_word(names(temp_set_sorted_new[m]))
                
                #                 cat(names(temp_set_sorted_new[m]))
                #                 
                #                 cat("; ")
                
            } 
            
            #------------ step 6 ------------------- 
            # make the final results (my_loop_counts or less words predicted) in the tasks set
            
            words_name <- temp_name
            words_prob <- p_sum_v
            words_count <- loop_counts   
            
        }else{
            
            do1 <- TRUE
        } 
        
    }       
    
    
    
    
    # do1 == TRUE ,the input_str must contain 1 word , looking for search_str1
    # and the last word ,step by step, in each step, work out the p2, p1 respectively.
    # then given top my_loop_counts predicted words with their px ordered.
    
    #------------ step 1-------------------
    # eg. search_str2: "of"
    
    if(do1 == TRUE){
        
        temp_index <- grep(search_str1, names(my_tdm_freq_n_2_gram_sorted))
        
        length_temp_index <- length(temp_index)
        
        if(length_temp_index != 0){
            
            temp_set <- my_tdm_freq_n_2_gram_sorted[temp_index]
            
            temp_set_sorted <- sort(temp_set, decreasing = TRUE)
            
            temp_set_sorted_value <- as.numeric(temp_set_sorted)
            
            temp_set_sorted_value_sum <- sum(temp_set_sorted_value)
            
            length_temp_set_sorted <- length_temp_index
            
            # work out p2.
            # e.g. p("beer"|"of") = c("of beer")/c("of")
            # = c("of beer")/c("of x1","of x2",..,"of xn")
            temp_set_sorted_p <- temp_set_sorted_value/temp_set_sorted_value_sum
            
            # only work for my_loop_counts (or less than my_loop_counts) items found 
            
            loop_counts <- my_loop_counts
            
            if(length_temp_set_sorted < my_loop_counts){
                
                loop_counts <- length_temp_set_sorted
                
            }
            
            #             cat("found ")
            #             
            #             cat(length_temp_set_sorted)
            #             
            #             cat(" matched items ")
            #             
            #             cat("\n")
            
            # in the tasks set, all of the px will be work out (based on p2,p1)
            
            for(i in 1:loop_counts){
                
                names_temp_set_sorted_i <- names(temp_set_sorted[i])
                
                #                 cat(" matched item ")
                #                 
                #                 cat(i)
                #                 
                #                 cat("\n")
                #                 
                #                 cat("the p of ")
                #                 
                #                 cat(names_temp_set_sorted_i)
                #                 
                #                 cat(":")
                #                 
                #                 cat(temp_set_sorted_p[i])
                #                 
                #                 cat("\n")
                
                p2 <- temp_set_sorted_p[i]
                
                #------------ step 2-------------------
                
                input_str <- names_temp_set_sorted_i
                
                # split every words in input_str
                # eg. input_str here" "of beer"
                
                ss <- strsplit(input_str, " ")
                
                ssv <- unlist(ss)
                
                ps0 <- ssv[2]
                
                # eg. search_str0: "^beer$"
                search_str0 <- paste("^", ps0, "$", sep="")
                
                # eg. want_str1: "beer"
                want_str1 <- ssv[2]
                
                want_str1_p <- 0  # init 0
                
                temp_index_1 <- grep(search_str0, names(my_tdm_freq_n_1_gram_sorted))
                
                length_temp_index_1 <- length(temp_index_1)
                
                if(length_temp_index_1 != 0){
                    
                    want_found <- my_tdm_freq_n_1_gram_sorted[temp_index_1]
                    
                    temp_set_0_sorted_value <- as.numeric(my_tdm_freq_n_1_gram_sorted)
                    
                    temp_set_0_sorted_value_sum <- sum(temp_set_0_sorted_value)
                    
                    
                    want_str1_value <- as.numeric(want_found)
                    
                    
                    # work out p1.
                    # e.g p("beer") = c("beer")/c("x")
                    # = c("beer")/c("x1","x2",..,"xn")
                    want_str1_p <- want_str1_value/temp_set_0_sorted_value_sum
                    
                }
                
                #                 cat("the p of ")
                #                 
                #                 cat(want_str1)
                #                 
                #                 cat(":")
                #                 
                #                 cat(want_str1_p)
                #                 
                #                 cat("\n")
                
                p1 <- want_str1_p
                
                #------------ step 3-------------------
                
                # work out px
                
                p_sum <- p2*c_2[2] + p1*c_2[1] 
                
                #                 cat("the sum of p:")
                #                 
                #                 cat(p_sum)
                #                 
                #                 cat("\n")
                
                # the vector of px
                p_sum_v <- c(p_sum_v, p_sum)
                
            }
            
            
            #------------ step 4-------------------
            
            # make the px ordered by possibility
            
            k <- order(p_sum_v, decreasing = TRUE)
            
            p_sum_v <- sort(p_sum_v, decreasing = TRUE)
            
            temp_set_sorted_new <- NULL
            
            for( j in 1:loop_counts){
                
                temp_set_sorted_new <- c(temp_set_sorted_new, temp_set_sorted[k[j]])
                
            }
            
            for( m in 1:loop_counts){
                
                temp_name[m] <- get_last_word(names(temp_set_sorted_new[m]))
                
                #                 cat(names(temp_set_sorted_new[m]))
                #                 
                #                 cat("; ")
                
            } 
            
            #------------ step 5-------------------     
            # make the final results( my_loop_counts or less words predicited) in the tasks set
            
            words_name <- temp_name
            words_prob <- p_sum_v
            words_count <- loop_counts  
            
            
        }else
        {
            
            cat("no items found 8( \n")
            
           
            
            
            # give the top unique word as the predicited word.
            
            words_name <- names(my_tdm_freq_n_1_gram_sorted[1])
            words_prob <- my_tdm_freq_n_1_gram_sorted[1]/sum(my_tdm_freq_n_1_gram_sorted)
            words_count <- 1
        
           
            
            }
        
        
    }        
    
    
    #     cat("=========\n")
    #     
    #     cat(words_name)
    #     
    #     cat("=======\n")
    #     
    #     cat(p_sum_v)
    #     
    #     cat("======\n")
    #     
    #     cat(words_count)
    
    my_words_tab <- data.frame( word = words_name, probability = words_prob)
    
    # return my_words table (only 10 of the words founded) .
    
    
    #if(words_count >10 ) my_words_tab <- my_words_tab[1:10, ] 
    

    
    my_words_tab
}



shinyServer(
    function(input, output) {
        
        input_text <- NULL
    
        
        #output$value <- renderPrint({ input$type })
      
        #output$out2 <- renderPrint(input$text1)
        
        
       # df_null_gram    initial empty gram only for displaying when no phrase inputed
        
        df_null_gram <- data.frame(word = c(" "), freq = c(0))
        
        
        # empty result as the initial time
        data <- df_null_gram
        
     
       # display the phrase inputed by user depended on the input type
        
        output$phrase <- renderText({
            
          input$gobutton
            
           isolate({
          
               
            if(input$gobutton  != 0){   
                
                # if "predict nextword"  button been pressed, render text for displaying 
            
            if(input$type =="1"){
                
               
                input_text <- input$text1
                
                }else{
               
                input_text <- input$text2
                
               }
        
                return(input_text)
                
            }else{
                
                # if "predict nextword"  button not been pressed the first time, display nothing
                return()
            }
          
           })  
         }
   
       )
        
    
    # work out the next word using the phrase inputed
        
       
        output$words_table <- DT::renderDataTable({DT::datatable({
            
           
            
             
            input$gobutton
            
            isolate({
            
            if(input$gobutton != 0){
                
                # if "predict nextword"  button been pressed, workout the prediction word as the result
                
                if(input$type =="1"){
                    
                    input_text <- isolate(input$text1)
                    
                }else{
                    
                    input_text <- isolate(input$text2)
                }
                  
                  
                # if input_text is nothing, display empty result, else go to next steps
                  if(input_text == ""){ 
                      
                      
                      data <- df_null_gram
                  
                  }else{
                    
                   input_text_processed <- input_process(input_text)
                    
                    
                    withProgress(message= 'Working out the next word...',{
                    
                    data <- pred_nextword(input_text_processed)
                    
                    })
                      
               
                  }
             }
            
            })
            
            data
            
        },
        
        options = list(searching = FALSE, paging = FALSE)
  
       
        )})
  
}

)       

