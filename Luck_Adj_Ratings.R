

#### Libraries to import ####
library(dplyr)
library(magrittr)
library(glmnet)
library(biglm)
library(MatrixModels)
library(ggplot2)
library(reshape2)
library(stringr)
library(tidyr)


#### Source Data ####
# Import Detailed Game Results
Reg_Season_Details <-
  read.csv(
    "~/ETC/Sports/NCAA Basketball/Luck_Adjusted_NCAA_Ratings/Source_Data/RegularSeasonDetailedResults.csv"
  )

Team_IDs <- read.csv("~/ETC/Sports/NCAA Basketball/Luck_Adjusted_NCAA_Ratings/Source_Data/Teams.csv")

# Add a unique ID for season matchup combos
Reg_Season_Details <- Reg_Season_Details %>% rowwise() %>%
  mutate(Team_Combo = paste0(Season, "_",min(Wteam,Lteam),"_",max(Wteam,Lteam)),
         Game_ID = paste0(Team_Combo,"_",Daynum)) %>%
  as.data.frame()

# Count how many times these teams played in the given season
Reg_Season_Details <- Reg_Season_Details %>%
  group_by(.,Team_Combo) %>%
  summarise(.,Count_Team_Combo = length(Team_Combo)) %>%
  merge(Reg_Season_Details, .)

# Merge in team names
Reg_Season_Details %<>%
  merge(.,Team_IDs,by.x="Wteam",by.y="Team_Id",all.x = TRUE) %>%
  rename(Wteam_name = Team_Name) %>%
  merge(.,Team_IDs,by.x="Lteam",by.y="Team_Id",all.x = TRUE) %>%
  rename(Lteam_name = Team_Name)


# Add additional details and statistics
Reg_Season_Details <- Reg_Season_Details %>% rowwise() %>%
  mutate(
    Lloc = if (Wloc == "H")
        "A"
      else if (Wloc == "A")
        "H"
      else
        "N",
    Wopp = Lteam,
    Lopp = Wteam,
    Wopp_name = Lteam_name,
    Lopp_name = Wteam_name,
    Wopp_score = Lscore,
    Lopp_score = Wscore,
    Wresult = "W",
    Lresult = "L",
    WOReb_per = Wor / (Ldr + Wor),
    WDReb_per = Wdr / (Lor + Wdr),
    LOReb_per = Lor / (Lor + Wdr),
    LDReb_per = Ldr / (Ldr + Wor),
    Wfgm2 = Wfgm - Wfgm3,
    Wfga2 = Wfga - Wfga3,
    Lfgm2 = Lfgm - Lfgm3,
    Lfga2 = Lfga - Lfga3,
    Poss = mean(Wfga - Wor + Wto + 0.475 * Wfta,Lfga - Lor + Wto + 0.475 * Lfta),
    Wblk_per = Wblk / Lfga2,
    Lblk_per = Lblk / Wfga2,
    Wstl_per = Wstl / Poss,
    Lstl_per = Lstl / Poss
  ) %>%
  as.data.frame()

Winning_Scores <- Reg_Season_Details %>% group_by(Season) %>%
  summarize(Winning_Score = mean(Wscore),
            Losing_Score = mean(Lscore),
            Poss = mean(Poss))


ggplot(melt(Winning_Scores,id="Season"), aes(x=Season, y=value,color=variable)) +
  geom_line() + expand_limits(y=0)


#### Rearrange Data ####

# Split winner and loser data and merge back together in tall format (twice as many rows, about half as wide)

# Columns starting with W and L are about the winner and loser
Winner_Data <-
  Reg_Season_Details[, grepl("^W" , names(Reg_Season_Details))]

Loser_Data <-
  Reg_Season_Details[, grepl("^L" , names(Reg_Season_Details))]

# Meta Data about game itself
Game_Data <-
  Reg_Season_Details[,!(grepl("^W" , names(Reg_Season_Details)) |
                          grepl("^L" , names(Reg_Season_Details)))]

names(Winner_Data) <-
  substring(names(Winner_Data),2)  #Strip off leading W and L
names(Loser_Data) <- substring(names(Loser_Data),2)

Tall_Season_Details <-
  rbind(cbind(Game_Data,Winner_Data),cbind(Game_Data,Loser_Data))
Tall_Season_Details <-
  arrange(Tall_Season_Details,Season,Daynum,Team_Combo)

Tall_Season_Details <- Tall_Season_Details %>% rowwise() %>%
  mutate(
    Team_ID = paste0(team, "_" , Season),
    Opp_ID = paste0(opp, "_", Season),
    ft_per = if (fta == 0)
        0
      else
        ftm / fta,
    fg3_per = if (fga3 == 0)
        0
      else
        fgm3 / fga3,
    fg2_per = if (fga2 == 0)
        0
      else
        fgm2 / fga2
  ) %>%
  as.data.frame()


Factor_Vars <-
  c("Team_Combo","team","opp","result","Team_ID","Opp_ID")
Tall_Season_Details <-
  Tall_Season_Details %>% mutate_each_(funs(factor),Factor_Vars)

str(Tall_Season_Details)


### Organize Data for Prediction Models ####

# Implement Frisch–Waugh–Lovell theorem for nesting Ridge Regressions

# Generic Source Data
Data <-
  droplevels(Tall_Season_Details)   #droplevels(Tall_Season_Details[Tall_Season_Details$Season==2003,])

DataVars <- Data[,c("Team_ID","Opp_ID","loc")]
Dataft_per <- Data$ft_per
Datafta <- Data$fta
Datafg3_per <- Data$fg3_per
Datafg3a <- Data$fga3

# Simple Logistic Model (Can't run due to size of model)
# FTModel <- glm(formula = ft_per ~ Team_ID + loc + 0,
#                family = binomial (link = "logit"),
#                data = Data,
#                weights = fta)
#
# summary(FTModel)
#
# Predicted_ft_per_lm <- as.data.frame(predict(FTModel, type = "response"))
# names(Predicted_ft_per_lm) <- c("pred_ft_per_lm")
# Data <- cbind(Data,Predicted_ft_per_lm)


###  Nested Ridge Regressions for FT% Prediction ####

# Construct Initial Model Matrix for Ridge Regression: Location
FTModel_Matrix1 <- model.Matrix(
  ~ loc,
  data = DataVars,
  contrasts.arg = lapply(DataVars[1:3], contrasts, contrasts = FALSE),
  sparse = TRUE
)


FTModelRidge1 <- glmnet(
  x = FTModel_Matrix1,
  y = Dataft_per,
  weights = Datafta,
  alpha = 0,
  family = "gaussian"
)

# plot(FTModelRidge1,xvar = "lambda")


cv.FTModelRidge1 <- cv.glmnet(
  x = FTModel_Matrix1,
  y = Dataft_per,
  weights = Datafta,
  alpha = 0,
  family = "gaussian"
)

# plot(cv.FTModelRidge1)

best_lambda_ft1 <- cv.FTModelRidge1$lambda.min



Predicted_ft_per1 <-
  as.data.frame(predict(cv.FTModelRidge1,FTModel_Matrix1,s = "lambda.min"))
names(Predicted_ft_per1) <- c("pred_ft_per1")

Data <- cbind(Data,Predicted_ft_per1)
Data$Residual_ft1 <- Data$ft_per - Data$pred_ft_per1


# Construct Second Model Matrix for Ridge Regression: Team
FTModel_Matrix2 <- model.Matrix(
  ~ Team_ID,
  data = DataVars,
  contrasts.arg = lapply(DataVars[1:3], contrasts, contrasts = FALSE),
  sparse = TRUE
)


FTModelRidge2 <- glmnet(
  x = FTModel_Matrix2,
  y = Data$Residual_ft1,
  weights = Datafta,
  alpha = 0,
  family = "gaussian"
)

# plot(FTModelRidge2,xvar = "lambda")


cv.FTModelRidge2 <- cv.glmnet(
  x = FTModel_Matrix2,
  y = Data$Residual_ft1,
  weights = Datafta,
  alpha = 0,
  family = "gaussian"
)

plot(cv.FTModelRidge2)

best_lambda_ft2 <- cv.FTModelRidge2$lambda.min



Predicted_ft_per2 <-
  as.data.frame(predict(cv.FTModelRidge2,FTModel_Matrix2,s = "lambda.min"))
names(Predicted_ft_per2) <- c("pred_ft_per2")

Data <- cbind(Data,Predicted_ft_per2)
Data$Residual_ft2 <-
  Data$ft_per - Data$pred_ft_per1 - Data$pred_ft_per2



# Construct Third Model Matrix for Ridge Regression: Opponent
FTModel_Matrix3 <- model.Matrix(
  ~ Opp_ID,
  data = DataVars,
  contrasts.arg = lapply(DataVars[1:3], contrasts, contrasts = FALSE),
  sparse = TRUE
)


FTModelRidge3 <- glmnet(
  x = FTModel_Matrix3,
  y = Data$Residual_ft2,
  weights = Datafta,
  alpha = 0,
  family = "gaussian"
)

# plot(FTModelRidge3,xvar = "lambda")


cv.FTModelRidge3 <- cv.glmnet(
  x = FTModel_Matrix3,
  y = Data$Residual_ft2,
  weights = Datafta,
  alpha = 0,
  family = "gaussian"
)

plot(cv.FTModelRidge3)

best_lambda_ft3 <- cv.FTModelRidge3$lambda.min



Predicted_ft_per3 <-
  as.data.frame(predict(cv.FTModelRidge3,FTModel_Matrix3,s = "lambda.min"))
names(Predicted_ft_per3) <- c("pred_ft_per3")

Data <- cbind(Data,Predicted_ft_per3)
Data$Predicted_ft_per <-
  Data$pred_ft_per1 + Data$pred_ft_per2 + Data$pred_ft_per3
Data$Residual_ft_per <- Data$ft_per - Data$Predicted_ft_per


###  Nested Ridge Regressions for FG3% Prediction ####

# Construct Initial Model Matrix for Ridge Regression: Location
fg3Model_Matrix1 <- model.Matrix(
  ~ loc,
  data = DataVars,
  contrasts.arg = lapply(DataVars[1:3], contrasts, contrasts = FALSE),
  sparse = TRUE
)


fg3ModelRidge1 <- glmnet(
  x = fg3Model_Matrix1,
  y = Datafg3_per,
  weights = Datafg3a,
  alpha = 0,
  family = "gaussian"
)

# plot(fg3ModelRidge1,xvar = "lambda")


cv.fg3ModelRidge1 <- cv.glmnet(
  x = fg3Model_Matrix1,
  y = Datafg3_per,
  weights = Datafg3a,
  alpha = 0,
  family = "gaussian"
)

# plot(cv.fg3ModelRidge1)

best_lambda_fg31 <- cv.fg3ModelRidge1$lambda.min



Predicted_fg3_per1 <-
  as.data.frame(predict(cv.fg3ModelRidge1,fg3Model_Matrix1,s = "lambda.min"))
names(Predicted_fg3_per1) <- c("pred_fg3_per1")

Data <- cbind(Data,Predicted_fg3_per1)
Data$Residual_fg31 <- Data$fg3_per - Data$pred_fg3_per1


# Construct Second Model Matrix for Ridge Regression: Team
fg3Model_Matrix2 <- model.Matrix(
  ~ Team_ID,
  data = DataVars,
  contrasts.arg = lapply(DataVars[1:3], contrasts, contrasts = FALSE),
  sparse = TRUE
)


fg3ModelRidge2 <- glmnet(
  x = fg3Model_Matrix2,
  y = Data$Residual_fg31,
  weights = Datafg3a,
  alpha = 0,
  family = "gaussian"
)

# plot(fg3ModelRidge2,xvar = "lambda")


cv.fg3ModelRidge2 <- cv.glmnet(
  x = fg3Model_Matrix2,
  y = Data$Residual_fg31,
  weights = Datafg3a,
  alpha = 0,
  family = "gaussian"
)

plot(cv.fg3ModelRidge2)

best_lambda_fg32 <- cv.fg3ModelRidge2$lambda.min



Predicted_fg3_per2 <-
  as.data.frame(predict(cv.fg3ModelRidge2,fg3Model_Matrix2,s = "lambda.min"))
names(Predicted_fg3_per2) <- c("pred_fg3_per2")

Data <- cbind(Data,Predicted_fg3_per2)
Data$Residual_fg32 <-
  Data$fg3_per - Data$pred_fg3_per1 - Data$pred_fg3_per2



# Construct Third Model Matrix for Ridge Regression: Opponent
fg3Model_Matrix3 <- model.Matrix(
  ~ Opp_ID,
  data = DataVars,
  contrasts.arg = lapply(DataVars[1:3], contrasts, contrasts = FALSE),
  sparse = TRUE
)


fg3ModelRidge3 <- glmnet(
  x = fg3Model_Matrix3,
  y = Data$Residual_fg32,
  weights = Datafg3a,
  alpha = 0,
  family = "gaussian"
)

# plot(fg3ModelRidge3,xvar = "lambda")


cv.fg3ModelRidge3 <- cv.glmnet(
  x = fg3Model_Matrix3,
  y = Data$Residual_fg32,
  weights = Datafg3a,
  alpha = 0,
  family = "gaussian"
)

plot(cv.fg3ModelRidge3)

best_lambda_fg33 <- cv.fg3ModelRidge3$lambda.min



Predicted_fg3_per3 <-
  as.data.frame(predict(cv.fg3ModelRidge3,fg3Model_Matrix3,s = "lambda.min"))
names(Predicted_fg3_per3) <- c("pred_fg3_per3")

Data <- cbind(Data,Predicted_fg3_per3)
Data$Predicted_fg3_per <-
  Data$pred_fg3_per1 + Data$pred_fg3_per2 + Data$pred_fg3_per3
Data$Residual_fg3_per <- Data$fg3_per - Data$Predicted_fg3_per


### Plot models ####

# Check scatter plot of FG3 Predictions
qplot(
  Predicted_fg3_per, fg3_per,data = sample_n(Data,1000),  size = fga3, alpha = I(1 / 10)
)


# Run a facet plot to see what misses look like
Data_Plot_fg3 <-
  Data[(Data$fga3 > 10 &
          Data$fga3 < 24),c("fga3","Predicted_fg3_per","fg3_per","Residual_fg3_per")]

Plot_fg3_hist <-
  ggplot(Data_Plot_fg3, aes(Residual_fg3_per)) + geom_histogram(binwidth = 0.005)
Plot_fg3_hist
Plot_fg3_hist + facet_grid (fga3 ~ .)


### Summarize and Model Remaining Variance ####

fg3_summary <- group_by(Data,fga3) %>%
  summarize(
    Var_Resid_fg3 = var(Residual_fg3_per),
    Var_fg3 = var(fg3_per),
    Var_Pred_fg3 = var(Predicted_fg3_per),
    Mean_Resid_fg3 = mean(Residual_fg3_per),
    Mean_fg3 = mean(fg3_per),
    Mean_Pred_fg3 = mean(Predicted_fg3_per),
    Count = length(fga3),
    inv_fga3 = 1 / mean(fga3)
  ) %>% filter(Count > 10,fga3 > 0) %>%
  mutate(
    Var_Binomial = Mean_fg3 * (1 - Mean_fg3) / fga3,
    Var_Unexplained = Var_Resid_fg3 - Var_Binomial
  )

fg3_var_model <- lm(data = fg3_summary,
                    Var_Unexplained ~ inv_fga3,
                    weights = Count)

fg3_var_model_int <- coef(fg3_var_model)[[1]]
fg3_var_model_inv_fg3 <- coef(fg3_var_model)[[2]]

fg3_summary$Modeled_Var <- predict.lm(fg3_var_model)

fg3_summary$fitted

ft_summary <- group_by(Data,fta) %>%
  summarize(
    Var_Resid_ft = var(Residual_ft_per),
    Var_ft = var(ft_per),
    Var_Pred_ft = var(Predicted_ft_per),
    Mean_Resid_ft = mean(Residual_ft_per),
    Mean_ft = mean(ft_per),
    Mean_Pred_ft = mean(Predicted_ft_per),
    Count = length(fta),
    inv_fta = 1 / mean(fta)
  ) %>% filter(Count > 10,fta > 0) %>%
  mutate(
    Var_Binomial = Mean_ft * (1 - Mean_ft) / fta,
    Var_Unexplained = Var_Resid_ft - Var_Binomial
  )

ft_var_model <- lm(data = ft_summary,
                   Var_Unexplained ~ inv_fta,
                   weights = Count)

ft_var_model_int <- coef(ft_var_model)[[1]]
ft_var_model_inv_fta <- coef(ft_var_model)[[2]]

ft_summary$Modeled_Var <- predict.lm(ft_var_model)

### Estimate variance of FTs for actual games ####

Data %<>% rowwise() %>%
  mutate( ft_var_binomial <- if(fta == 0)
              0
            else
              Predicted_ft_per * (1 - Predicted_ft_per) / fta,
          ft_var_remaining <- if(fta == 0)
              0
            else
              ft_var_model_int + ft_var_model_inv_fta/fta) %>%
  as.data.frame()

names(Data)[length(Data)] <- "ft_var_remaining"
names(Data)[length(Data)-1] <- "ft_var_binomial"


### Regress FTs based on inverse variance weighting ####

Data %<>% rowwise() %>%
  mutate( Regressed_ft_per = if(fta == 0)
                              Predicted_ft_per
                            else
                              ((Predicted_ft_per/ft_var_remaining +
                                ft_per / ft_var_binomial) /
                              (1/ft_var_remaining + 1/ft_var_binomial))
          ) %>%
  as.data.frame()



### Adjust final scores based on luck adjusted FTs and FG3s ####
Pts_miss <- 0.35   # Offensive points expected per 3Pt miss

Data <- Data %>%
  mutate( ft_m_change = fta*(Regressed_ft_per - ft_per),
          fg3_m_change = fga3*(Predicted_fg3_per - fg3_per),
          score_change = ft_m_change + 3*fg3_m_change - Pts_miss*fg3_m_change,
          score_adj = score + score_change
  )



### Plot Variance curves ####

ggplot(fg3_summary, aes(x = fga3)) +
  geom_line(aes(y = Var_Resid_fg3, color = "Variance, adj. for teams & locs"),size = 1) +
  geom_line(aes(y = Var_fg3, color = "Raw Variance"),size = 1) +
  geom_line(aes(y = Var_Binomial, color = "Binomial Variance"),size = 1) +
  # geom_line(aes(y=Var_Pred_fg3, color="Var_Pred_fg3"))
  labs(title = "Variance in FG3%", x = "3Pt FGA", y = "Variance") +
  theme(legend.position = "top",legend.title = element_blank())

ggplot(ft_summary, aes(x = fta)) +
  geom_line(aes(y = Var_Resid_ft, color = "Variance, adj. for teams & locs"),size = 1) +
  geom_line(aes(y = Var_ft, color = "Raw Variance"),size = 1) +
  geom_line(aes(y = Var_Binomial, color = "Binomial Variance"),size = 1) +
  # geom_line(aes(y=Var_Pred_ft, color="Var_Pred_ft"))
  labs(title = "Variance in FT%", x = "FTA", y = "Variance") +
  theme(legend.position = "top",legend.title = element_blank())


ggplot(fg3_summary, aes(x = fga3)) +
  geom_line(aes(y = Var_Pred_fg3, color = "Variance explained by teams & loc"),size = 1) +
  geom_line(aes(y = Var_Unexplained, color = "Unexplained Variance"),size = 1) +
  geom_line(aes(y = Modeled_Var, color = "Unexplained Variance, Fit"),size = 1) +
  geom_hline(yintercept = 0) +
  labs(title = "Variance in FG3%", x = "3Pt FGA", y = "Variance") +
  theme(legend.position = "top",legend.title = element_blank())

ggplot(ft_summary, aes(x = fta)) +
  geom_line(aes(y = Var_Pred_ft, color = "Variance explained by teams & loc"),size = 1) +
  geom_line(aes(y = Var_Unexplained, color = "Unexplained Variance"),size = 1) +
  geom_line(aes(y = Modeled_Var, color = "Unexplained Variance, Fit"),size = 1) +
  geom_hline(yintercept = 0) +
  labs(title = "Variance in FT%", x = "FTA", y = "Variance") +
  theme(legend.position = "top",legend.title = element_blank())


# qplot(Data$Predicted_ft_per, geom="histogram")

### Look at individual games ####

# View(Data[Data$Team_Combo=="2009_1163_1393",])
View(Data[Data$Team_Combo=="2016_1242_1328",])

hottest_games <- Data %>% filter(Season == 2016) %>% group_by(Game_ID) %>%
  summarize(heat = sum(score_change),
            less_hot = max(score_change),
            margin = max(score)-min(score),
            Poss = mean(Poss),
            OT = mean(Numot),
            exp_margin = max(score_adj)-min(score_adj),
            game_quality = -less_hot - margin/Poss*35 + 5*OT) %>%
  arrange(-game_quality)

View(Data[Data$Game_ID %in% hottest_games$Game_ID[1:10],])


### Construct Ratings Matrix ####

Game_Results <- Data %>% group_by(Game_ID) %>%
  summarize(Team1 = nth(team,1),
            Team2 = nth(team,2),
            Team_Name1 = nth(team_name,1),
            Team_Name2 = nth(team_name,2),
            Score1 = nth(score,1),
            Score2 = nth(score,2),
            Score_Adj1 = nth(score_adj,1),
            Score_Adj2 = nth(score_adj,2),
            Loc_Team1 = nth(loc,1),
            Margin = Score1 - Score2,
            Margin_Adj = Score_Adj1 - Score_Adj2,
            Margin_75 = Margin*(75/max(score)),
            Margin_Adj_75 = Margin_Adj*(75/max(score_adj))
  ) %>% merge(.,unique(Data[c("Game_ID","Poss","Season","Daynum",
                      "Numot","Team_Combo","Count_Team_Combo")])) %>%
  mutate(Team_ID_1 = paste0(Team1,"_",Season),
         Team_ID_2 = paste0(Team2,"_",Season))

Team_Factors <- as.data.frame(c(Game_Results$Team_ID_1,Game_Results$Team_ID_2))
names(Team_Factors) <- c("Team_ID")

Game_Results$Team_ID_1 <- factor(Game_Results$Team_ID_1,levels=levels(Team_Factors$Team_ID))
Game_Results$Team_ID_2 <- factor(Game_Results$Team_ID_2,levels=levels(Team_Factors$Team_ID))

Team1_DF <- Game_Results[,c("Loc_Team1","Team_ID_1")]
names(Team1_DF)[2]<-"Team_"
Team2_DF <- Game_Results[,c("Loc_Team1","Team_ID_2")]
names(Team2_DF)[2]<-"Team_"

Margin_75 <- Game_Results$Margin_75
Margin_Adj_75 <- Game_Results$Margin_Adj_75

Results_Matrix <- model.Matrix(
  ~ Loc_Team1 -1,
  data = Team1_DF,
  contrasts.arg = lapply(Team1_DF[1:2], contrasts, contrasts = FALSE),
  sparse = TRUE
)

Results_Matrix2 <- model.Matrix(
  ~ Team_ -1,
  data = Team1_DF,
  contrasts.arg = lapply(Team1_DF[1:2], contrasts, contrasts = FALSE),
  sparse = TRUE
)

Results_Matrix3 <- model.Matrix(
  ~ Team_ -1,
  data = Team2_DF,
  contrasts.arg = lapply(Team2_DF[1:2], contrasts, contrasts = FALSE),
  sparse = TRUE
)

Results_Matrix4 <- model.Matrix(
  ~ Loc_Team1 + Team_ -1,
  data = Team1_DF,
  contrasts.arg = lapply(Team1_DF[1:2], contrasts, contrasts = FALSE),
  sparse = TRUE
)

Loc_Sparse <- as(Results_Matrix,"dgCMatrix")
Teams_Sparse <- Results_Matrix2-Results_Matrix3

Combined_Sparse <- cbind2(Loc_Sparse,Teams_Sparse)

Matrix_Trial <- new("dsparseModelMatrix", Combined_Sparse,
                    assign = Results_Matrix4@assign,
                    contrasts = Results_Matrix4@contrasts)


str(Matrix_Trial)
dim(Matrix_Trial)

object.size(Matrix_Trial)
object.size(Results_Matrix2)

head(Matrix_Trial)


Scores_1 <- cv.glmnet(
  x = Matrix_Trial,
  y = Margin_75,
  alpha = 0,
  family = "gaussian",
  lambda.min = .000000001,
)

plot(Scores_1)

best_lambda_Scores_1 <- Scores_1$lambda.min

Output1 <- as.data.frame(as.matrix(coef.cv.glmnet(Scores_1)))
names(Output1) <- c("Margin_75_Ridge")
Output1 <- add_rownames (Output1, "Rows")


Scores_2 <- cv.glmnet(
  x = Matrix_Trial,
  y = Margin_Adj_75,
  alpha = 0,
  family = "gaussian",
  lambda.min = .000000001,
)

plot(Scores_2)
Output2 <- as.data.frame(as.matrix(coef.cv.glmnet(Scores_2)))
names(Output2) <- c("Margin_Adj_75_Ridge")
Output2 <- add_rownames (Output2, "Rows")

Scores_3 <- glmnet(
  x = Matrix_Trial,
  y = Margin_75,
  alpha = 0,
  family = "gaussian",
  lambda = 0,
)

Output3 <- as.data.frame(as.matrix(coef(Scores_3)))
names(Output3) <- c("Margin_75")
Output3 <- add_rownames (Output3, "Rows")

Scores_4 <- glmnet(
  x = Matrix_Trial,
  y = Margin_Adj_75,
  alpha = 0,
  family = "gaussian",
  lambda = 0,
)

Output4 <- as.data.frame(as.matrix(coef(Scores_4)))
names(Output4) <- c("Margin_Adj_75")
Output4 <- add_rownames (Output4, "Rows")

Results_Matrix <- Reduce(function(...) merge(...,all.x=TRUE),list(Output1,Output2,Output3,Output4))

# Results_Matrix$Delta_Adj <- Results_Matrix$Margin_Adj_75 - Results_Matrix$Margin_75
Results_Matrix <- separate(data=Results_Matrix,col = Rows, into = c("Junk","Team_Id","season"),sep="_")
Results_Matrix <- merge(Results_Matrix,Team_IDs,all.x = TRUE)

plot(Results_Matrix$Margin_75,Results_Matrix$Margin_Adj_75)
View(Results_Matrix)
View(Results_Matrix[Results_Matrix$season==2016,])
