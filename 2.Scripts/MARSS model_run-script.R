#######################################
####          MARSS MODELS         ####
#######################################

## Description: this script starts with data obtained from MARSS-input-Total Abundance.csv.
# It is a systematic manner to (1) Turn the input into matrix form, (2) Set the model one by one estimate parameters  
# and (3) Run the hypothesis

#> Authors: Julio Ubeda Quesada and Bjarki Elvarsson
#> Contact: julioubedaquesada@gmail.com
#> Institution: Aristotle University of Thessaloniki
#> Last modified: February 2022
#################################################################################################

rm(list=ls())

#--------------Here I am working in with relative paths, refering to sub-directories in my main directory (home_file)

home_file  = getwd()
data_loc   = paste0(home_file,"/1.Data/")
fig_loc    = paste0(home_file,"/3.Figures_Graphs/")
Output_loc = paste0(home_file, "/4.Outputs/")
                                           
#--------------Make sure to change to your own relative paths

# Libraries. Default libraries included, make sure you install and load if you need more
Packages = c("plyr", "tidyverse", "ggpubr", "MARSS", "reshape2", "parallel")  
lapply(Packages, library, character.only = TRUE)

## MARSS model structure

#  (1) PROCESS EQUATION ------------ Xt = xt-1 + u + Ctct + wt, where wt  ~ MVN (0,Q)
#  (2) OBSERVATION EQUATION -------- Yt = Zxt + a + vt, where vt ~ MVN(0,R)

## 7. Run MARSS models with Biomass information.
setwd(data_loc)

Fish_bio <- read.table('MARSS-input-Total Biomass.csv', header=TRUE, sep=",") ### xt
colnames(Fish_bio)[2:ncol(Fish_bio)] <- 1996:2019
rownames(Fish_bio) <- Fish_bio$Region
logfish_bio = log(Fish_bio[,2:ncol(Fish_bio)])
Y = as.matrix(logfish_bio)
Y = t(apply(X=logfish_bio, MARGIN=1, FUN=scale)) ## Transposing the matrix and normalizing, so can set U to zero.

## 1. CREATING THE MODEL 
model01 = list() 

## 2. SCALING VECTOR
model01$A = 'scaling'

## 3. Variance covariance matrix in the observation equation
R = matrix(list(0),nrow(Y),nrow(Y))
boat = c("GR", "ICE", "GR", "NEA") 
diag(R) <- boat
head(R)
model01$R = R

## 4. The levels in Z corresponds to the abundance time series in fish. We have to manually complete the population structures by giving the same number
# to those time-series that could follow the same population dynamics. Then, we go with the hipotheses. 

zone <- fish$Region # Order = EG - ICE - WGR - NEA

# 4.1. Panmictic
# (1) EG-ICE-WGR-NEA
Z1  = as.matrix(array(1, dim=c(nrow(Y),1)))

# 4.2 Current Stocks 
# (1) EG-NEA (2) WGR (3) NEA
Z2 = factor(c(1, 1, 2, 3))                                              

# 4.3 Correlated population dynamics in north-east atlantic.
# (1) EG (2) ICE-NEA (3) WGR
Z3 = factor(c(1, 2, 3, 2))                                      

# 4.4 Correlated population dynamic in north-west atlantic.
# (1) EG (2) ICE-WGR (3) NEA
Z4 = factor(c(1, 2, 2, 3))                                          

# 4.5 Two separated population with boundaries in south Greenland. 
# (1) EG-ICE-NEA (2) WGR
Z5 = factor(c(1, 1, 2, 1))                                            

# 4.6 Correlated population dynamic in north-east atlantic.
# (1) EG-NEA (2) ICE (3) WGR
Z6 = factor(c(1, 2, 3, 1))                                            

# 4.7 Correlated dynamic population rather in the east and west of the north atlantic.
#(1) EG-WGR (2) ICE-NEA
Z7 = factor(c(1, 2, 1, 2))                                              

# 4.8.	Correlated dynamic population rather in the east and west of the north atlantic.
# (1) EG-NEA (2) ICE-WGR
Z8 = factor(c(1, 2, 2, 1))                                            

# 4.9.	Correlated dynamic population rather in the east and west of the north atlantic.
# (1) EG (2) ICE (3) WGR (4) NEA
Z9 = factor(c(1, 2, 3, 4))                                            

Z <-list(Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9)

## 5. Covariates.
# 5.1. Values: c_t -- is a  p x T matrix.  p = covariates (NROW), T = 24 years (NCOL)
nao_covariates <- 
  list(
       cov1=read.table("Cov_NAO.csv", header=TRUE, sep = ";") %>% 
         as.matrix() %>% 
         t(),
       cov2 = read.table("Cov_NAO_lag1.5.csv", header=TRUE, sep = ";") %>% 
         as.matrix() %>% 
         t())

catch_covariates <- 
  list(
       cov3=read.table("Cov_catch.csv", header=TRUE, sep = ";") %>% 
         as.matrix() %>% 
         t(),
       cov4 = read.table("Cov_catch_lag.csv", header=TRUE, sep = ";") %>% 
         as.matrix() %>% 
         t())

# 5.2. SET UP Covariates MATRIX: C -- is a m x p matrix.  m = states (NROW), p = covariates (NCOL).

C1 = matrix(c("catch_NWAS", "catch_WNS", "catch_NORSL"),        
              ncol = 3, nrow = 1, byrow = TRUE)

C2 = matrix(c(0, "catch_WNS", 0,             
              "catch_NWAS", 0, 0,             
              0, 0, "catch_NORSL"),                     
            ncol = 3, nrow = 3, byrow = TRUE)

C3 = matrix(c(0, "catch_WNS", 0,             
              0, "catch_WNS", "catch_NORSL", 
              "catch_NWAS", 0, 0),           
            ncol = 3, nrow = 3, byrow = TRUE)

C4 = matrix(c(0, "catch_WNS", 0,             
              "catch_NWAS", "catch_WNS", 0, 
              0, 0, "catch_NORSL"),           
            ncol = 3, nrow = 3, byrow = TRUE)

C5 = matrix(c(0, "catch_WNS", "catch_NORSL",             
              "catch_NWAS", 0, 0),           
            ncol = 3, nrow = 2, byrow = TRUE)

C6 = matrix(c(0, "catch_WNS", "catch_NORSL",             
              0, "catch_WNS", 0, 
              "catch_NWAS", 0, 0),           
            ncol = 3, nrow = 3, byrow = TRUE)

C7 = matrix(c("catch_NWAS", "catch_WNS", 0,             
              0, "catch_WNS", "catch_NORSL"),           
            ncol = 3, nrow = 2, byrow = TRUE)

C8 = matrix(c(0, "catch_WNS", "catch_NORSL",             
              "catch_NWAS", "catch_WNS", 0),           
            ncol = 3, nrow = 2, byrow = TRUE)

C9 = matrix(c(0, "catch_WNS", 0,             
              0, "catch_WNS", 0,
              "catch_NWAS", 0, 0,             
              0, 0, "catch_NORSL"),           
            ncol = 3, nrow = 4, byrow = TRUE)

catch_C = list(C1, C2, C3, C4, C5, C6, C7, C8, C9)
  
  
C1 = matrix(c("nao"),            
            ncol = 1, nrow = 1, byrow = TRUE)

C2 = matrix(c("nao",             
              "nao",             
              "nao"),                     
            ncol = 1, nrow = 3, byrow = TRUE)

C3 = matrix(c("nao",             
              "nao", 
              "nao"),           
            ncol = 1, nrow = 3, byrow = TRUE)

C4 = matrix(c("nao",             
              "nao",             
              "nao"),           
            ncol = 1, nrow = 3, byrow = TRUE)

C5 = matrix(c("nao",             
              "nao"),           
            ncol = 1, nrow = 2, byrow = TRUE)

C6 = matrix(c("nao",             
              "nao", 
              "nao"),           
            ncol = 1, nrow = 3, byrow = TRUE)

C7 = matrix(c("nao",          
              "nao"),        
            ncol = 1, nrow = 2, byrow = TRUE)

C8 = matrix(c("nao",             
              "nao"),               
            ncol = 1, nrow = 2, byrow = TRUE)

C9 = matrix(c("nao",            
              "nao",                 
              "nao",            
              "nao"),                    
            ncol = 1, nrow = 4, byrow = TRUE)

nao_C = list(C1, C2, C3, C4, C5, C6, C7, C8, C9)

## 6. MARSS model fitting
z = c("Z1_", "Z2_", "Z3_", "Z4_", "Z5_", "Z6_", "Z7_", "Z8_", "Z9_")
U = c("unequal","equal")
u = c('Uun_','Ueq_')
Q = c("unconstrained", "diagonal and unequal", "diagonal and equal")
q = c('Qun','Qdu','Qde')

run_dat <- 
  expand_grid(dummy=1, U= U, Q = Q) %>% 
  left_join(tibble(ztype = 1:9,Z=Z,dummy=1)) %>% 
  select(-dummy) 

run_dat <- 
  run_dat %>% 
  bind_rows(run_dat %>% 
              left_join(
                tibble(ztype = 1:9,
                       C = catch_C,
                       ctype = 'catch') %>%
                  left_join(expand_grid(ctype = 'catch',
                                        covariates = catch_covariates))),
            run_dat %>% 
              left_join(
                tibble(ztype = 1:9,
                       C = nao_C,
                       ctype = 'nao') %>%
                  left_join(expand_grid(ctype = 'nao',
                                        covariates = nao_covariates)))) %>% 
  mutate(run= 1:n())

tmp_func <- function(x, res='res'){
  dir.create(res)
  model01$Z <- x$Z[[1]]
  model01$U <- x$U
  model01$Q = x$Q
  if(!is.null(x$covariates)){
    model01$c = x$covariates[[1]] 
    model01$C = x$C[[1]]
  } 
  
  m1 = MARSS(Y, model=model01, 
             control=list(maxit = 5000, trace = 1, conv.test.slope.tol= 100),
             silent=2, 
             method="kem")
  
  m2 = MARSSaic(m1, output = "AICbp", Options = list(nboot = 100, silent = TRUE))
  capture.output(m2,file=paste0(res,'/',"run_",x$run,'.txt'))
  saveRDS(m2,file = paste0(res,'/',"run_",x$run,'.rds'))
}

## 7. Run MARSS models with Biomass information.
Fish_bio <- read.table('MARSS-input-Total Biomass.csv', header=TRUE, sep=",") ### xt
colnames(Fish_bio)[2:ncol(Fish_bio)] <- 1996:2019
rownames(Fish_bio) <- Fish_bio$Region
logfish_bio = log(Fish_bio[,2:ncol(Fish_bio)])
Y = as.matrix(logfish_bio)
Y = t(apply(X=logfish_bio, MARGIN=1, FUN=scale)) ## Transposing the matrix and normalizing, so can set U to zero.

parallel::mclapply(run_dat %>% split(.$run),tmp_func,mc.cores = parallel::detectCores(logical = TRUE)) 

setwd(Output_loc)

mod_bio <- 
  fs::dir_ls('res_bioms',regexp = '*.rds') %>% 
  purrr::map(readRDS) %>% 
  purrr::map(~try(broom::glance(.))) %>% 
  purrr::keep(~class(.)=='data.frame') %>% 
  bind_rows(.id = 'run') %>% 
  mutate(run = as.numeric(gsub('res_bioms/run_([0-9]+).rds','\\1',run))) %>% 
  right_join(run_dat)

mod_output <-
    fs::dir_ls('res',regexp = '*.rds') %>%
    purrr::map(readRDS) %>%
    parallel::mclapply(MARSSparamCIs, method = 'parametric', nboot=100,
                       mc.cores = parallel::detectCores(logical = TRUE))

AICB_bio <- rename(AICB_bio, AICbp = AICb)
AICB_bio <- arrange(AICB_bio, AICbp)
mod_bio <- arrange(mod_bio, AICbp)
AICB_bio <- cbind(AICB_bio, Hipotheses = mod_bio$ztype)
AICB_bio$C <- recode_factor(AICB_bio$C, 
                            "5" = "Catch",
                            "2" = "NAO",
                            "1" = "No covariates")

write.table(AICB_bio, "AICB All models_bio.csv",sep=",", col.names=TRUE, row.names=FALSE)
saveRDS(mod_bio, "mod_bio.rds")

####

bmod_bio <- readRDS("run_152.rds")
states1 <- bmod_bio$states
states.se1 <- bmod_bio$states.se
states.df <- melt(states1)
states.se.df <-melt(states.se1)

colnames(states.df) <- c("States","Year","log.bio")
colnames(states.se.df) <- c("states","year","se")

states.df$States <- as.character(states.df$States)
states.df$Year <- as.character(states.df$Year)
df1 <- cbind(states.df, states.se.df)
df2 <-select(df1, -states, -year)

df2 <- 
  df2 %>%
  mutate(States = revalue(States, c("1" = "EG", "2" = "ICE-WGR", "3" = "NEA")))

df2 <- 
  df2 %>%
  mutate(Year = revalue(Year, c("1"="1996", "2"="1997", "3"="1998", "4"="1999", "5"="2000", "6"="2001", "7"="2002", "8"="2003", "9"="2004", "10"="2005", "11"="2006", "12"="2007"
                                , "13"="2008", "14"="2009", "15"="2010", "16"="2011", "17"="2012", "18"="2013", "19"="2014", "20"="2015", "21"="2016","22"="2017", "23"="2018", "24"="2019")))
### Best fit model BIOMASS PLOT

bmod_bio_plot <- ggplot(df2, 
                        aes(Year, log.bio, 
                            group = States, colour = States)) +
  geom_point() +
  geom_line() +
  
  geom_ribbon(aes(ymin=log.bio-se, ymax=log.bio+se), fill = "grey80", alpha = .5, linetype = 0, show.legend = F) +
  
  xlab("Years") + ylab("Log-Biomass") +
  theme(strip.text.y = element_blank (),
        legend.position = c(.99, .999),
        legend.justification = c("right", "top"),
        legend.title = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.box.background = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(colour = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "bold", margin = margin(t =10)),
        axis.text.x = element_text(colour = "black", size = 15, angle = 90, hjust = .5, vjust = 0.5, face = "bold"),
        axis.title.y.left = element_text(colour = "black", size = 20, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 10)),
        axis.text.y.left = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"))
bmod_bio_plot
bmod_abu_plot
setwd(fig_loc)
ggsave("Best-Fit-Model_BIOMASS.png", dpi = 300, units = "cm", height = 20, width = 40)

### Now with Abundance information.
setwd(data_loc)

Fish_abu <- read.table('MARSS-input-Total ABUNDANCE.csv', header=TRUE, sep=",") ### xt
colnames(Fish_abu)[2:ncol(Fish_abu)] <- 1996:2019
rownames(Fish_abu) <- Fish_abu$Region
logfish_abu = log(Fish_abu[,2:ncol(Fish_abu)])
Y = as.matrix(logfish_abu)
Y = t(apply(X=logfish_abu, MARGIN=1, FUN=scale)) ## Transposing the matrix and normalizing, so can set U to zero.

parallel::mclapply(run_dat %>% split(.$run), tmp_func, res='res_abund', mc.cores = parallel::detectCores(logical = TRUE)) 

setwd(Output_loc)
mod_abu <- 
  fs::dir_ls('res_abund',regexp = '*.rds') %>% 
  purrr::map(readRDS) %>% 
  purrr::map(~try(broom::glance(.))) %>% 
  purrr::keep(~class(.)=='data.frame') %>% 
  bind_rows(.id = 'run') %>% 
  mutate(run = as.numeric(gsub('res_abund/run_([0-9]+).rds','\\1',run))) %>% 
  right_join(run_dat)

mod_output_abund <-
    fs::dir_ls('res_abund',regexp = '*.rds') %>%
    purrr::map(readRDS) %>%
    parallel::mclapply(MARSSparamCIs, method = 'parametric', nboot=100,
                       mc.cores = parallel::detectCores(logical = TRUE))

AICB_abu <- rename(AICB_abu, AICbp = AICb)
AICB_abu <- arrange(AICB_abu, AICbp)
mod_abu <- arrange(mod_abu, AICbp)
AICB_abu <- cbind(AICB_abu, Hipotheses = mod_abu$ztype)
AICB_abu$C <- recode_factor(AICB_abu$C, 
                            "5" = "Catch",
                            "2" = "NAO",
                            "1" = "No covariates")

saveRDS(mod_abu, "mod_abu.rds")
write.table(AICB_abu, "AICB All models_abu.csv",sep=",", col.names=TRUE, row.names=FALSE)

# 8. Model selection using the AIC bootstrap variant.

mod_abu$AICbp = as.numeric(as.character(mod_abu$AICbp))
mod_abu$delta = mod_abu$AICbp - min(mod_abu$AICbp, na.rm=TRUE)
mod_abu = mod_abu[order(mod_abu$delta),]

# Wrangling with the dataframe.
mod_abu <- select(mod_abu, -coef.det, -sigma, -df, -logLik, -AIC, -AICc, -convergence, -errors)
mod_abu <- mod_abu[1:10,]

mod_abu$run <- 1:10
mod_abu$Z <- c(3, 3, 3, 2, 3, 3, 3, 3, 2, 3)
vec <- c("Catch_U", "Catch_L", "-", "NAO_L", "NAO_L", "NAO_U", "NAO_U", "NAO_L", "Catch_L", "Catch_L")
mod_abu$ctype <- vec
mod_abu$K <- c(20, 20, 15, 12, 18, 10, 18, 15, 9, 20)
mod_abu <- select(mod_abu, -C, -covariates)

mod_abu <- mod_abu %>% relocate(ztype, .after = run)
mod_abu <- mod_abu %>% relocate(c(U, Q, Z, K), .after = ztype)
mod_abu <- mod_abu %>% relocate(ctype, .after = ztype)
colnames(mod_abu) <- c('Model','Hipothesis','Covariates','U','Q',"Z",'K','AICb','dAICb')

write.table(mod_abu, "AICB 10first models_abu.csv",sep=",", col.names=TRUE, row.names=FALSE)

## 
setwd(Output_loc)

bmod_abu <- readRDS("run_152.rds")
states1 <- bmod_abu$states
states.se1 <- bmod_abu$states.se
states.df <- melt(states1)
states.se.df <-melt(states.se1)

colnames(states.df) <- c("States","Year","log.abu")
colnames(states.se.df) <- c("states","year","se")

states.df$States <- as.character(states.df$States)
states.df$Year <- as.character(states.df$Year)
df1 <- cbind(states.df, states.se.df)
df2 <-select(df1, -states, -year)

df2 <- 
  df2 %>%
  mutate(States = revalue(States, c("1" = "EG", "2" = "ICE-WGR", "3" = "NEA")))

df2 <- 
  df2 %>%
  mutate(Year = revalue(Year, c("1"="1996", "2"="1997", "3"="1998", "4"="1999", "5"="2000", "6"="2001", "7"="2002", "8"="2003", "9"="2004", "10"="2005", "11"="2006", "12"="2007"
                                , "13"="2008", "14"="2009", "15"="2010", "16"="2011", "17"="2012", "18"="2013", "19"="2014", "20"="2015", "21"="2016","22"="2017", "23"="2018", "24"="2019")))

# PLOT Abundance trajectories

bmod_abu_plot <- ggplot(df2, 
       aes(Year, log.abu, 
           group = States, colour = States)) +
  geom_point() +
  geom_line() +
  
  geom_ribbon(aes(ymin=log.abu-se, ymax=log.abu+se), fill = "grey80", alpha = .5, linetype = 0, show.legend = F) +
  
  xlab("Years") + ylab("Log-Abundance") +
  theme(strip.text.y = element_blank (),
        legend.position = c(.99, .999),
        legend.justification = c("right", "top"),
        legend.title = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.box.background = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill ="white"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(colour = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "bold", margin = margin(t =10)),
        axis.text.x = element_text(colour = "black", size = 15, angle = 90, hjust = .5, vjust = 0.5, face = "bold"),
        axis.title.y.left = element_text(colour = "black", size = 20, angle = 90, hjust = .5, vjust = 0, face = "bold", margin = margin(r = 10)),
        axis.text.y.left = element_text(colour = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"))
bmod1_plot

setwd(fig_loc)
ggsave("Best-Fit-Model_ABUNDANCE.png", dpi = 300, units = "cm", height = 20, width = 40)
#########


#############    TAIL    #################
# With this script you have: (1) Set up the parameters for MARSS models, (2) Hypothesized different populations structures,
# (3) Run MARSS model and save outputs
