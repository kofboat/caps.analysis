library(tidyverse)
library(ggplot2)

#----------------------------------------------------------
#     IMPORT DATA
#----------------------------------------------------------

msd <- read_csv("MONTHLY DATA/master.data.csv")
msd <- as_tibble(msd)



#-----------------------------------------------------------
#     DATA CLEANING AND PREPARATION
#------------------------------------------------------------

msd <- msd %>%
  select(!c(12:19)) # delete empty columns


msd1 <- msd %>%  # create new , reorganize and delete variables
  mutate(prod_cat = category,
         gross_margin = round((gross_profit/ total_sale_price *100),2),
         sub_cat = sub_category,
         quarter = case_when(month=='NOV_21' ~ 1,
                             month=='DEC_21' ~ 1,
                             month=='JAN_22' ~ 1,
                             month=='FEB_22' ~ 2,
                             month=='MAR_22' ~ 2,
                             month=='APR_22' ~ 2,
                             month=='MAY_22' ~ 3,
                             month=='JUN_22' ~ 3,
                             month=='JUL_22' ~ 3,
                             month=='AUG_22' ~ 4,
                             month=='SEP_22' ~ 4,
                             month=='OCT_22' ~ 4
           
         ),
         category = NULL,
         sub_category = NULL,
         margin = NULL
         )%>% 
relocate(prod_cat, .before = qty)%>% 
  relocate(sub_cat,.after = prod_cat)%>%
    relocate(quarter, .before = product)


#-----------------------------------------------------------
#     GENERATING BUSINESS INSIGHT
#-----------------------------------------------------------
#     Sales Volume Analysis
#
#------------------------------------------------------------
#      1. Monthly Volume Analysis
#-----------------------------------------------------------
#         A. Monthly sales by product category
#             1.filter data by month
#             2.group by prod_cat
#-----------------------------------------------------------
        
month.sales.prod.cat <- function(x){

  a <-  msd1 %>% filter(month == x)  %>% 
    group_by(prod_cat)%>%
    summarise(total_sales=sum(qty))
  a
  }

 month.sales.prod.cat("FEB_22")
  
#--------------------------------------------------------------
#         B. Monthly sales by product sub_category
#             1.filter data by month
#             2.group by sub_cat 
#-------------------------------------------------------------- 
 month.sales.sub.cat <- function(x){
   
   (b <-  msd1 %>% filter(month == x)  %>% 
     group_by(sub_cat)%>%
     summarise(total_sales=sum(qty))
    )
    b <- b[order(-b$total_sales),]
   b
 }             
 
 month.sales.sub.cat("JAN_22")              
#--------------------------------------------------------------
#         C. Monthly sales by product 
#             1.filter data by month
#             2.group by product and sub_cat
#--------------------------------------------------------------                
 month.sales.prod <- function(x){
   
   (c <-  msd1 %>% filter(month == x)  %>% 
      group_by(product,sub_cat)%>%
      summarise(total_sales=(qty))
   )
   c <- c[order(-c$total_sales),]
   c
 }             
 
 month.sales.prod("JAN_22")
 
#---------------------------------------------------------
#      2. Quarterly Volume Analysis
# 
#----------------------------------------------------------
#         A. Quarterly sales volume by product category     
#             1.filter data by quarter
#             2.group by prod_cat 
#-------------------------------------------------------------- 
 
 
 q.sales.prod.cat <- function(x){
   
   d <-  msd1 %>% filter(quarter == x)  %>% 
     group_by(prod_cat)%>%
     summarise(total_sales=sum(qty))
   d
 }
 
 q.sales.prod.cat()
 
#------------------------------------------------------------
#         B. Quarterly sales by product sub_cat
#            1.filter data by quarter
#            2.group by sub_cat 
#----------------------------------------------------------- 
 q.sales.sub.cat <- function(x){
   
   (e <-  msd1 %>% filter(quarter == x)  %>% 
      group_by(sub_cat)%>%
      summarise(total_sales=sum(qty))
   )
   e <- e[order(-e$total_sales),]
   e
 }             
 
 q.sales.sub.cat(1) 
#--------------------------------------------------------------
#        C. Quarterly sales by product 
#           1.filter data by month
#           2.group by product and sub_cat
#--------------------------------------------------------------    
 q.sales.prod <- function(x){
   
   (f <-  msd1 %>% filter(month == x)  %>% 
      group_by(product,sub_cat)%>%
      summarise(total_sales=(qty))
   )
   f <- f[order(-c$total_sales),]
   f
 }             
 
 q.sales.prod()
 
#---------------------------------------------------------
#     3. Year-to-Date Volume Analysis
# 
#----------------------------------------------------------
#        A. YTD sales  by product category
#           
#           1.group by prod_cat 
#--------------------------------------------------------------  
 
 ytd.sales.prod.cat <- function(){(
   g <- msd1 %>% 
     group_by(category) %>%
     summarise(total_sales = sum(qty))
   )
   g <- g[order(-g$total_sales),]
   g
 }
 
 
#--------------------------------------------------------------  
#       B. YTD sales  by product sub_category     
#           
#           1.group by sub_cat 
#--------------------------------------------------------------  
 
 ytd.sales.sub.cat <- function(){(
   h <- msd1 %>% 
     group_by(sub_cat) %>%
     summarise(total_sales = sum(qty))
 )
   h <- h[order(-h$total_sales),]
   h
 }
 
 tail(ytd.sales.sub.cat(),10)
 
#-----------------------------------------------------------------
#       C. YTD sales by product     
#           
#           1.group by product 
#--------------------------------------------------------------  
  ytd.sales.prod <- function(){(
   i <- msd1 %>% 
     group_by(product,sub_cat) %>% 
     summarise(total_sales = sum(qty)) 
 )
   i <- i[order(-i$total_sales),]
   i
 }
 
 head(ytd.sales.prod())
 tail(ytd.sales.prod())
 
 #-----------------------------------------------------------
 #     4. Monthly Revenue Analysis
 #-----------------------------------------------------------
 #     A.  Monthly sales by product category
 #           1.filter data by month
 #           2.group by prod_cat
 #-----------------------------------------------------------
 
 month.sales.rev.prod.cat <- function(x){(
    
    j <-  msd1 %>% filter(month == x)  %>% 
       group_by(prod_cat)%>%
       summarise(total_sales=sum(total_sale_price))
 )
    j <- j[order(-j$total_sales),]
    j
 }
 
 month.sales.rev.prod.cat("MAR_22")
 
 #--------------------------------------------------------------
 #     B. Monthly sales by product sub_category
 #         1.filter data by month
 #         2.group by sub_cat 
 #-------------------------------------------------------------- 
 month.sales.rev.sub.cat <- function(x){(
    
    k <-  msd1 %>% filter(month == x)  %>% 
        group_by(sub_cat)%>%
        summarise(total_sales=sum(total_sale_price))
    )
    k <- k[order(-k$total_sales),]
    k
 }             
 
 head(month.sales.rev.sub.cat("MAR_22"))
 tail(month.sales.rev.sub.cat("MAR_22"))
 
 #--------------------------------------------------------------
 #     C.  Monthly sales by product 
 #         1.filter data by month
 #         2.group by product and sub_cat
 #--------------------------------------------------------------                
 month.sales.rev.prod <- function(x){
    
    (l <-  msd1 %>% filter(month == x)  %>% 
        group_by(product,sub_cat)%>%
        summarise(total_sales=(total_sale_price))
    )
    l <- l[order(-l$total_sales),]
    l
 }             
 
 head(month.sales.rev.prod("DEC_21"))
 tail(month.sales.rev.prod("DEC_21"))
 
 #---------------------------------------------------------
 #   5.  Quarterly Revenue Analysis
 # 
 #----------------------------------------------------------
 #    A. Quarterly sales by product category     
 #       1.filter data by quarter
 #       2.group by prod_cat 
 #-------------------------------------------------------------- 
  q.sales.rev.prod.cat <- function(x){(
    
    m <-  msd1 %>% filter(quarter == x)  %>% 
       group_by(prod_cat)%>%
       summarise(total_sales=sum(total_sale_price))
  )
    m 
 }
 
  head(q.sales.rev.prod.cat(1))
  tail(q.sales.rev.prod.cat(1))
 #------------------------------------------------------------
 #    B. Quarterly sales by product sub_cat
 #       1.filter data by quarter
 #       2.group by sub_cat 
 #----------------------------------------------------------- 
 q.sales.rev.sub.cat <- function(x){
    
    (n <-  msd1 %>% filter(quarter == x)  %>% 
        group_by(sub_cat)%>%
        summarise(total_sales=sum(total_sale_price))
    )
    n <- n[order(-n$total_sales),]
    n
 }             
 
 head(q.sales.rev.sub.cat(1) )
 tail(q.sales.rev.sub.cat(1) )
 #--------------------------------------------------------------
 #    C. Quarterly sales by product 
 #       1.filter data by quarter
 #       2.group by product and sub_cat
 #-------------------------------------------------------------- 
 q.sales.rev.prod <- function(x){(
    
    o <-  msd1 %>% filter(quarter == x)  %>% 
        group_by(product,sub_cat)%>%
        summarise(total_sales=sum(total_sale_price))
    )
    o <- o[order(-o$total_sales),]
    o
 }             
 
 head(q.sales.rev.prod(1))
 tail(q.sales.rev.prod(1))
 #---------------------------------------------------------
 #   6.  Year-To-Date Revenue Analysis
 # 
 #----------------------------------------------------------
 #    A. YTD sales by product category     
 #       1.filter data by quarter
 #       2.group by prod_cat 
 #--------------------------------------------------------------
  ytd.rev.prod.cat <-function(){(
    
   p <-  msd1 %>% 
   group_by(prod_cat) %>%
   summarise(total_sales = sum(total_sale_price))
 )
 ytd.rev.prod.cat
 
 }
 
 #------------------------------------------------------------
 #    B. YTD sales by product sub_cat
 #       1.filter data by quarter
 #       2.group by sub_cat 
 #----------------------------------------------------------- 
 ytd.sales.rev.sub.cat <- function(){(
    
    q <-  msd1 %>% 
        group_by(sub_cat)%>%
        summarise(total_sales=sum(total_sale_price))
    )
    q <- q[order(-q$total_sales),]
    q
 }             
 
 head(ytd.sales.rev.sub.cat() )
 tail(ytd.sales.rev.sub.cat() ) 
 #--------------------------------------------------------------
 #    C. YTD sales by product 
 #      
 #       2.group by product and sub_cat
 #-------------------------------------------------------------- 
 ytd.sales.rev.prod <- function(){(
    p <-  msd1  %>% 
       group_by(product,sub_cat)%>%
       summarise(total_sales=sum(total_sale_price))
 )
    p <- p[order(-p$total_sales),]
    p
 }             
 
 head(ytd.sales.rev.prod(),50)
 tail(ytd.sales.rev.prod())
 #------------------------------------------------------------------
 #    Gross Margin Analysis
 #
 #-----------------------------------------------------------------
 # Average margin per category
 #----------------------------------------------------------------
 gross.margin.prod.cat <- function(){(
 
 r <- msd1 %>% 
   group_by(prod_cat) %>%
   summarise(avg_margin = mean(gross_margin))
 )
 
 r
 }
 gross.margin.prod.cat()
 #-----------------------------------------------------------------
 # Average margin per product sub_category
 #----------------------------------------------------------------
 # average margin per sub_category
 gross.margin.sub.cat <- function(){(
    
    s <- msd1 %>% drop_na()%>%
       group_by(sub_cat) %>%
       summarise(avg_margin = mean(gross_margin))
 )
    s <- s[order(-s$avg_margin),]
    s
    s %>% 
       mutate(sub_cat=fct_reorder(sub_cat,avg_margin)
              )%>%
      ggplot()+
       geom_col(mapping = aes(x=sub_cat,y=avg_margin))+
       coord_flip()
 }
 
 
 
 gross.margin.sub.cat()
 tail(gross.margin.sub.cat())
 #-----------------------------------------------------------------
 # Average margin per product 
 #---------------------------------------------------------------- 
 gross.margin.prod <- function(){(
    
    t <- msd1 %>% 
       group_by(product,prod_cat) %>%
       summarise(avg_margin = round(mean(gross_margin),2))
 )
    t <- t[order(-t$avg_margin),] 
       head(t,30) %>%
    ggplot()+
       geom_col(mapping = aes(product,avg_margin, fill= prod_cat))+
       coord_flip()
    
 }
gross.margin.prod()
tail(gross.margin.prod())
 
 
 # year to date margin
 ytd_margin <- msd %>% 
   
     summarise(avg_margin = mean(margin))
 ytd_margin

