##----------------------------------------------------## 
##                  NewRProjct Script                 ## 
##                                                    ## 
##----------------------------------------------------##

Hello-World <- function(){print("Hello World!")}
## Whaaat?
install.packages("ggplot2")
library(ggplot2)
data(diamonds)

install.packages("dplyr")
library(dplyr)
## classic: 
head(diamonds)

## better: 
tbl_df(diamonds)
# Source: local data frame [53,940 x 10]
# 
# carat       cut  color clarity depth table price     x     y     z
# (dbl)    (fctr) (fctr)  (fctr) (dbl) (dbl) (int) (dbl) (dbl) (dbl)
# 1   0.23     Ideal      E     SI2  61.5    55   326  3.95  3.98  2.43
# 2   0.21   Premium      E     SI1  59.8    61   326  3.89  3.84  2.31
# 3   0.23      Good      E     VS1  56.9    65   327  4.05  4.07  2.31
# 4   0.29   Premium      I     VS2  62.4    58   334  4.20  4.23  2.63
# 5   0.31      Good      J     SI2  63.3    58   335  4.34  4.35  2.75
# 6   0.24 Very Good      J    VVS2  62.8    57   336  3.94  3.96  2.48
# 7   0.24 Very Good      I    VVS1  62.3    57   336  3.95  3.98  2.47
# 8   0.26 Very Good      H     SI1  61.9    55   337  4.07  4.11  2.53
# 9   0.22      Fair      E     VS2  65.1    61   337  3.87  3.78  2.49
# 10  0.23 Very Good      H     VS1  59.4    61   338  4.00  4.05  2.39
# ..   ...       ...    ...     ...   ...   ...   ...   ...   ...   ...

sub.diam <- subset(diamonds, color == "D" | color == "E" | color == "F" & cut == "Ideal" | cut == "Premium" & carat > 3.0)

sub.diam <- subset(diamonds, carat > 3 & 
                      (color == "D" | color == "E" | color == "F") & 
                     (cut == "Ideal" | cut == "Premium"))
sub.diam <- filter(diamonds, carat > 3, 
                  cut %in% c("Premium", "Ideal") & 
                  color %in% c("D", "E", "F"))
# carat     cut color clarity depth table price    x    y    z
# 1  3.01 Premium     F      I1  62.2    56  9925 9.24 9.13 5.73
# 2  3.05 Premium     E      I1  60.9    58 10453 9.26 9.25 5.66

tbl_df(select(diamonds, carat, cut, clarity))
## select all columns from diamonds to clarity: 
tbl_df(select(diamonds, carat:clarity, price)) 

## PIPELINING / CHAINING!
?"%>%" ## magrittr %>% 
?chain ## dplyr %>% 

## base: select columns, order columns 
head(sel.diam <- diamonds[,c("color", "price", "carat")])
head(sel.diam.1 <- sel.diam[order(c(-sel.diam$price, sel.diam$color)),])

## dplyr: 
tbl_df(diamonds %>%
  select(color, price, carat) %>%
  arrange(color, price, desc(carat)))

tbl_df(diamonds %>%
  select(color, carat, price) %>%
  mutate(ppc = price / carat, 
         ppc_rnd = round(ppc, 2)))


tbl_df(diamonds %>% 
         group_by(color) %>% 
         summarise(MIN = min(price), MEAN = mean(price), MAX = max(price)))
#   Source: local data frame [7 x 4]
#   
#   color   MIN     MEAN   MAX
#   (fctr) (int)    (dbl) (int)
#   1      D   357 3169.954 18693
#   2      E   326 3076.752 18731
#   3      F   342 3724.886 18791
#   4      G   354 3999.136 18818
#   5      H   337 4486.669 18803
#   6      I   334 5091.875 18823
#   7      J   335 5323.818 18710
  
summarise(diamonds, MIN = min(price), MEAN = mean(price), MAX = max(price))
#   MIN   MEAN   MAX
# 1 326 3932.8 18823







