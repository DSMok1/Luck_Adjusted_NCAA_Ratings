
#### Libraries to import ####
library(dplyr)
library(magrittr)
library(glmnet)
library(biglm)
library(MatrixModels)
library(ggplot2)

#### Source Data ####
# Import Detailed Game Results
Reg_Season_Details <- read.csv("~/ETC/Sports/NCAA Basketball/Luck_Adjusted_NCAA_Ratings/Source_Data/RegularSeasonDetailedResults.csv")


# Add a unique ID for season matchup combos
Reg_Season_Details <- Reg_Season_Details %>% rowwise() %>%
  mutate(Team_Combo = paste0(Season, "_",min(Wteam,Lteam),"_",max(Wteam,Lteam))) %>%
  as.data.frame()

# Count how many times these teams played in the given season
Reg_Season_Details <- Reg_Season_Details %>%
  group_by(.,Team_Combo) %>%
  summarise(.,Count_Team_Combo = length(Team_Combo)) %>%
  merge(Reg_Season_Details, .)

# Add additional details and statistics
Reg_Season_Details <- Reg_Season_Details %>% rowwise() %>%
  mutate(Lloc = if (Wloc=="H") "A" else if (Wloc=="A") "H" else "N",
         Wopp = Lteam,
         Lopp = Wteam,
         Wresult = "W",
         Lresult = "L",
         WOReb_per= Wor/(Ldr+Wor),
         WDReb_per= Wdr/(Lor+Wdr),
         LOReb_per= Lor/(Lor+Wdr),
         LDReb_per= Ldr/(Ldr+Wor),
         Wfgm2 = Wfgm - Wfgm3,
         Wfga2 = Wfga - Wfga3,
         Lfgm2 = Lfgm - Lfgm3,
         Lfga2 = Lfga - Lfga3,
         Poss = mean(Wfga-Wor+Wto+0.475*Wfta,Lfga-Lor+Wto+0.475*Lfta),
         Wblk_per = Wblk/Lfga2,
         Lblk_per = Lblk/Wfga2,
         Wstl_per = Wstl/Poss,
         Lstl_per = Lstl/Poss
         ) %>%
  as.data.frame()


#### Rearrange Data ####

# Split winner and loser data and merge back together in tall format (twice as many rows, about half as wide)

# Columns starting with W and L are about the winner and loser
Winner_Data<-Reg_Season_Details[ , grepl( "^W" , names( Reg_Season_Details ) ) ]

Loser_Data<-Reg_Season_Details[ , grepl( "^L" , names( Reg_Season_Details ) ) ]

# Meta Data about game itself
Game_Data<-Reg_Season_Details[ , !(grepl( "^W" , names( Reg_Season_Details ) ) | grepl( "^L" , names( Reg_Season_Details ) ))]

names(Winner_Data) <- substring(names(Winner_Data),2)  #Strip off leading W and L
names(Loser_Data) <- substring(names(Loser_Data),2)

Tall_Season_Details <- rbind(cbind(Game_Data,Winner_Data),cbind(Game_Data,Loser_Data))
Tall_Season_Details <- arrange(Tall_Season_Details,Season,Daynum,Team_Combo)

Tall_Season_Details <- Tall_Season_Details %>% rowwise() %>%
  mutate( Team_ID = paste0(team, "_" , Season),
          Opp_ID = paste0(opp, "_", Season),
          ft_per = if (fta == 0) 0 else ftm/fta,
          fg3_per = if (fga3 == 0) 0 else fgm3/fga3,
          fg2_per = if (fga2 == 0) 0 else fgm2/fga2) %>%
  as.data.frame()


Factor_Vars <- c("Team_Combo","team","opp","result","Team_ID","Opp_ID")
Tall_Season_Details <- Tall_Season_Details %>% mutate_each_(funs(factor),Factor_Vars)

str(Tall_Season_Details)



#### Get team FT% and FT% Defense ####

# Implement Frisch–Waugh–Lovell theorem for nesting Ridge Regressions

# Generic Source Data
Data2003 <- droplevels(Tall_Season_Details[Tall_Season_Details$Season==2003,])
Data2003Vars <-Data2003[,c("Team_ID","Opp_ID","loc")]
Data2003ft_per <- Data2003$ft_per
Data2003fta <- Data2003$fta
Data2003fg3_per <- Data2003$fg3_per
Data2003fg3a <- Data2003$fga3

# Simple Logistic Model
FTModel <- glm(formula = ft_per ~ Team_ID + loc + 0,
               family = binomial (link = "logit"),
               data = Data2003,
               weights = fta)

summary(FTModel)

Predicted_ft_per_lm <- as.data.frame(predict(FTModel, type = "response"))
names(Predicted_ft_per_lm) <- c("pred_ft_per_lm")
Data2003 <- cbind(Data2003,Predicted_ft_per_lm)




# Construct Initial Model Matrix for Ridge Regression
FTModel_Matrix1 <- model.Matrix(  ~ loc,
                                data=Data2003Vars,
                                contrasts.arg = lapply(Data2003Vars[1:3], contrasts, contrasts=FALSE),
                                sparse = TRUE)


FTModelRidge1 <- glmnet(x = FTModel_Matrix1,
                       y = Data2003ft_per,
                       weights = Data2003fta,
                       alpha = 0,
                       family = "gaussian"
                       )

plot(FTModelRidge1,xvar="lambda")


cv.FTModelRidge1 <- cv.glmnet(x = FTModel_Matrix1,
                          y = Data2003ft_per,
                          weights = Data2003fta,
                          alpha = 0,
                          family = "gaussian"
                          )

plot(cv.FTModelRidge1)

best_lambda_ft1 <- cv.FTModelRidge1$lambda.min



Predicted_ft_per1 <- as.data.frame(predict(cv.FTModelRidge1,FTModel_Matrix1,s="lambda.min"))
names(Predicted_ft_per1) <- c("pred_ft_per1")

Data2003 <- cbind(Data2003,Predicted_ft_per1)
Data2003$Residual_ft1 <- Data2003$ft_per - Data2003$pred_ft_per1


# Construct Second Model Matrix for Ridge Regression
FTModel_Matrix2 <- model.Matrix(  ~ Team_ID,
                                  data=Data2003Vars,
                                  contrasts.arg = lapply(Data2003Vars[1:3], contrasts, contrasts=FALSE),
                                  sparse = TRUE)


FTModelRidge2 <- glmnet(x = FTModel_Matrix2,
                        y = Data2003$Residual_ft1,
                        weights = Data2003fta,
                        alpha = 0,
                        family = "gaussian"
)

plot(FTModelRidge2,xvar="lambda")


cv.FTModelRidge2 <- cv.glmnet(x = FTModel_Matrix2,
                              y = Data2003$Residual_ft1,
                              weights = Data2003fta,
                              alpha = 0,
                              family = "gaussian"
)

plot(cv.FTModelRidge2)

best_lambda_ft2 <- cv.FTModelRidge2$lambda.min



Predicted_ft_per2 <- as.data.frame(predict(cv.FTModelRidge2,FTModel_Matrix2,s="lambda.min"))
names(Predicted_ft_per2) <- c("pred_ft_per2")

Data2003 <- cbind(Data2003,Predicted_ft_per2)
Data2003$Residual_ft2 <- Data2003$ft_per - Data2003$pred_ft_per1 - Data2003$pred_ft_per2



# Construct Third Model Matrix for Ridge Regression
FTModel_Matrix3 <- model.Matrix(  ~ Opp_ID,
                                  data=Data2003Vars,
                                  contrasts.arg = lapply(Data2003Vars[1:3], contrasts, contrasts=FALSE),
                                  sparse = TRUE)


FTModelRidge3 <- glmnet(x = FTModel_Matrix3,
                        y = Data2003$Residual_ft2,
                        weights = Data2003fta,
                        alpha = 0,
                        family = "gaussian"
)

plot(FTModelRidge3,xvar="lambda")


cv.FTModelRidge3 <- cv.glmnet(x = FTModel_Matrix3,
                              y = Data2003$Residual_ft2,
                              weights = Data2003fta,
                              alpha = 0,
                              family = "gaussian"
)

plot(cv.FTModelRidge3)

best_lambda_ft3 <- cv.FTModelRidge3$lambda.min



Predicted_ft_per3 <- as.data.frame(predict(cv.FTModelRidge3,FTModel_Matrix3,s="lambda.min"))
names(Predicted_ft_per3) <- c("pred_ft_per3")

Data2003 <- cbind(Data2003,Predicted_ft_per3)
Data2003$Predicted_ft_per <- Data2003$pred_ft_per1 + Data2003$pred_ft_per2 + Data2003$pred_ft_per3
Data2003$Residual_ft_per <- Data2003$ft_per - Data2003$Predicted_ft_per






# Construct Initial Model Matrix for Ridge Regression
fg3Model_Matrix1 <- model.Matrix(  ~ loc,
                                  data=Data2003Vars,
                                  contrasts.arg = lapply(Data2003Vars[1:3], contrasts, contrasts=FALSE),
                                  sparse = TRUE)


fg3ModelRidge1 <- glmnet(x = fg3Model_Matrix1,
                        y = Data2003fg3_per,
                        weights = Data2003fg3a,
                        alpha = 0,
                        family = "gaussian"
)

plot(fg3ModelRidge1,xvar="lambda")


cv.fg3ModelRidge1 <- cv.glmnet(x = fg3Model_Matrix1,
                              y = Data2003fg3_per,
                              weights = Data2003fg3a,
                              alpha = 0,
                              family = "gaussian"
)

plot(cv.fg3ModelRidge1)

best_lambda_fg31 <- cv.fg3ModelRidge1$lambda.min



Predicted_fg3_per1 <- as.data.frame(predict(cv.fg3ModelRidge1,fg3Model_Matrix1,s="lambda.min"))
names(Predicted_fg3_per1) <- c("pred_fg3_per1")

Data2003 <- cbind(Data2003,Predicted_fg3_per1)
Data2003$Residual_fg31 <- Data2003$fg3_per - Data2003$pred_fg3_per1


# Construct Second Model Matrix for Ridge Regression
fg3Model_Matrix2 <- model.Matrix(  ~ Team_ID,
                                  data=Data2003Vars,
                                  contrasts.arg = lapply(Data2003Vars[1:3], contrasts, contrasts=FALSE),
                                  sparse = TRUE)


fg3ModelRidge2 <- glmnet(x = fg3Model_Matrix2,
                        y = Data2003$Residual_fg31,
                        weights = Data2003fg3a,
                        alpha = 0,
                        family = "gaussian"
)

plot(fg3ModelRidge2,xvar="lambda")


cv.fg3ModelRidge2 <- cv.glmnet(x = fg3Model_Matrix2,
                              y = Data2003$Residual_fg31,
                              weights = Data2003fg3a,
                              alpha = 0,
                              family = "gaussian"
)

plot(cv.fg3ModelRidge2)

best_lambda_fg32 <- cv.fg3ModelRidge2$lambda.min



Predicted_fg3_per2 <- as.data.frame(predict(cv.fg3ModelRidge2,fg3Model_Matrix2,s="lambda.min"))
names(Predicted_fg3_per2) <- c("pred_fg3_per2")

Data2003 <- cbind(Data2003,Predicted_fg3_per2)
Data2003$Residual_fg32 <- Data2003$fg3_per - Data2003$pred_fg3_per1 - Data2003$pred_fg3_per2



# Construct Third Model Matrix for Ridge Regression
fg3Model_Matrix3 <- model.Matrix(  ~ Opp_ID,
                                  data=Data2003Vars,
                                  contrasts.arg = lapply(Data2003Vars[1:3], contrasts, contrasts=FALSE),
                                  sparse = TRUE)


fg3ModelRidge3 <- glmnet(x = fg3Model_Matrix3,
                        y = Data2003$Residual_fg32,
                        weights = Data2003fg3a,
                        alpha = 0,
                        family = "gaussian"
)

plot(fg3ModelRidge3,xvar="lambda")


cv.fg3ModelRidge3 <- cv.glmnet(x = fg3Model_Matrix3,
                              y = Data2003$Residual_fg32,
                              weights = Data2003fg3a,
                              alpha = 0,
                              family = "gaussian"
)

plot(cv.fg3ModelRidge3)

best_lambda_fg33 <- cv.fg3ModelRidge3$lambda.min



Predicted_fg3_per3 <- as.data.frame(predict(cv.fg3ModelRidge3,fg3Model_Matrix3,s="lambda.min"))
names(Predicted_fg3_per3) <- c("pred_fg3_per3")

Data2003 <- cbind(Data2003,Predicted_fg3_per3)
Data2003$Predicted_fg3_per <- Data2003$pred_fg3_per1 + Data2003$pred_fg3_per2 + Data2003$pred_fg3_per3
Data2003$Residual_fg3_per <- Data2003$fg3_per - Data2003$Predicted_fg3_per


qplot(Predicted_fg3_per, fg3_per,data = Data2003,  size = fga3, alpha = I(1/25))








#
#
#
# BigGLM_Logit <- function (Model_Data) {
#
#   chunksize <- 5000                           #Set Chunk size
#   length_Target <-
#     NROW(Model_Data)           #Get length of the dataset
#
#
#   Chunk_1 <-
#     Model_Data[1:chunksize,]       #Get the starting chunk of data
#
#
#   round_down <- function(x,to = 10)
#   {
#     to * (x %/% to - as.logical(x %% to))
#   }
#
#   End_of_Chunks <-
#     (round_down(length_Target,chunksize))       #Identify the end of the chunks
#
#   Chunk_Last <-
#     Model_Data[End_of_Chunks + 1:length_Target,]        #Get the last, odd chunk
#
#   fit_Target <-
#     bigglm(formula = ft_per ~ Team_ID + Opp_ID + loc,
#            family = binomial (link = "logit"),
#            data = Chunk_1,
#            weights = ~ fta)
#   Sys.time()
#
#   for (i in seq(chunksize,End_of_Chunks,chunksize)) {
#     fit_Target = update(fit_Target, moredata = Model_Data[(i + 1):(i + chunksize),])
#
#     cat(i,"of",length_Target," - (",round(i/length_Target*100,2),")%\n")
#   }
#   Sys.time()
#   fit_Target = update(fit_Target, moredata = Chunk_Last)
#   Sys.time()
#
#
#   return (fit_Target)
# }
#
#
# ### Call Regression and Clean Up Results ####
# Sys.time()
# FTModel <- BigGLM_Logit (Tall_Season_Details[Tall_Season_Details$fta>0,])
# Sys.time()
#
# library(broom)
# FT_Results <- tidy(FTModel)





