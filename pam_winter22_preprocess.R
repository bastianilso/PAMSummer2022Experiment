library(gsheet)
library(tidyverse)
source("utils/loadrawdata.R")

options("digits.secs"=6)

# Load data from directories
D <- LoadFromDirectory("data/PAM", event="Game", sample="BlinkLog")
#D <- LoadFromDirectory("testdata2/PAM/", event="Game", sample="BlinkLog")
save(D, file = 'data_pam_raw.rda', compress=TRUE)

load('data_pam_raw.rda')

#############
# Format D
#############
D <- D %>% rename(ConditionLabel = Condition, Condition = i3, Participant = i2)

D <-D %>% mutate(Participant = as.numeric(Participant),
                 TrialResult = ifelse(TrialResult == "NA", NA, TrialResult))
                 #`Success BCI` = as.logical(`Success BCI`))

# veryify all data is present
#D %>% group_by(Participant, Condition) %>% 
#  summarize(
#    success_count = n()
#  ) %>% view()


# FishEvents happen outside Input Windows
# include it to make analysis easier.
# This needs to happen before we fill and filter data outside input windows.
# Technically, these events can be considered part of the windows, afterall.
D <-D %>% mutate(InputWindowOrderFish = InputWindowOrder,
                 InputWindowOrderFish = ifelse(InputWindowOrderFish == "Stopped", NA, InputWindowOrderFish),
                 InputWindowOrderFish = as.numeric(InputWindowOrderFish),
                 InputWindowOrderFish = ifelse(Event == "FishEvent", InputWindowOrderFish-1, InputWindowOrderFish))

# It would be nice to mark input windows with a number indicating whether the input window
# gave fish feedback or not.
D <- D %>% group_by(Participant, Condition, InputWindowOrderFish) %>%
  summarise(fishFeedback = sum(FishEvent == "FishCaught", na.rm=T),
            fishLost = sum(FishEvent == "FishLost", na.rm=T)) %>%
  right_join(D)

posTrial = c("AccInput", "AugSuccess", "AssistSuccess", "ExplicitSham", "OverrideInput")
negTrial = c("RejInput")
neuTrial = c("AssistFail", "MitigateFail")

# Define TrialFeedback, a column denoting what feedback was given during the trial.
D <- D %>% mutate(TrialFeedback = NA,
                  TrialFeedback = ifelse(Event == "GameDecision", "UNDEFINED", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & TrialResult %in% posTrial, "Reel", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & fishFeedback == 1, "FishCaught", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & TrialResult %in% negTrial, "Unreel", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & fishLost == 1, "FishLost", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & TrialResult %in% neuTrial, "Stay", TrialFeedback)
)

#cv <- D %>% group_by(Participant, Condition, TrialFeedback) %>% filter(Event == "GameDecision") %>%
#   summarize(n())
# 


D = D %>% mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  arrange(Timestamp) %>%
  mutate(time_delta = Timestamp - lag(Timestamp),
         time_delta = as.numeric(time_delta),
         time_delta = ifelse(is.na(time_delta), 0, time_delta))

# Create variable for locating GameDecisions within Rest periods and associating them to open periods.
D = D %>% group_by(Participant, Condition) %>%
  mutate(InputWindowOrderWithRest = InputWindowOrderFish,
         InputWindowOrderWithRest = ifelse(Event == "GameStopped", -1, InputWindowOrderWithRest)) %>%
  tidyr::fill(InputWindowOrderWithRest, .direction="down") %>%
  mutate(InputWindowOrderWithRest = as.numeric(InputWindowOrderWithRest))

#D %>% filter(Participant == 13) %>% select(Event, InputWindowOrder, InputWindowOrderWithRest, Condition) %>% view()    

D = D %>% group_by(Participant, Condition, InputWindowOrderWithRest) %>% 
  mutate(TrialResultWindow = TrialResult,
         TrialFeedbackWindow = TrialFeedback) %>%
  tidyr::fill(TrialResultWindow, .direction="up") %>%
  tidyr::fill(TrialFeedbackWindow, .direction="up")

#D %>% filter(Participant == 8) %>% select(Event, InputWindowOrder, InputWindowOrderWithRest,TrialResultWindow,TrialFeedbackWindow, Condition) %>% view()    

# Filter out data happening before GameRunning event.
# Filter out extra "GameStopped" events.
# Test: D %>% filter(Participant == 6) %>% select(Event, Participant, Condition, isGameOver, isGame) %>% view()
# D %>% filter(Participant == 19, Condition == "AS", Event != "Sample") %>% plot_ly(x=~Timestamp, y=~Framecount)
D = D %>% ungroup() %>% group_by(Participant, Condition) %>% arrange(Timestamp) %>% #filter(Participant == 19, Condition == "AS", Event != "Sample") %>%
  mutate(isGame = ifelse(Event == "GameRunning", 1, 0),
         isGame = cumsum(isGame)) %>%
  filter(isGame == 1) %>%
  mutate(isGameOver = ifelse(Event == "GameStopped", 1,0),
         isGameOver = cumsum(isGameOver),
         isGameOver = ifelse(Event == "GameStopped", isGameOver-1,isGameOver)) %>% #select(isGame, isGameOver, Event, Timestamp) %>%
  filter(isGameOver < 1) # %>% #View()



# Create InputWindowClosedFill - creates an identifier for resting periods.
# Test: D %>% filter(Participant == 6) %>% select(Event, Participant, Condition, InputWindowClosedID, InputWindow, InputWindowClosedFill) %>% view()
D = D %>% mutate(InputWindowClosedID = NA,
                 InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", 1, 0),
                 InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", cumsum(InputWindowClosedID), InputWindowClosedID),
                 InputWindowClosedID = ifelse(Event == "GameStopped", -1, InputWindowClosedID),
                 InputWindowClosedID = ifelse(Event == "GameRunning", -1, InputWindowClosedID),
                 InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Open", -1, InputWindowClosedID),
                 InputWindowClosedFill = ifelse(InputWindowClosedID == 0, NA, InputWindowClosedID)) %>%
  tidyr::fill(InputWindowClosedFill, .direction="down")

# Create InputWindowOrderFilled column - an identifier for open periods.
# Test: D %>% filter(Participant == 6) %>% select(Event, Participant, Condition, InputWindowOrderWithDecision,InputWindowOrderFilled, InputWindow, InputWindowClosedFill) %>% view()
D = D %>% group_by(Participant, Condition) %>% 
  mutate(InputWindowOrder = ifelse(Event == "GameStopped", "-1", InputWindowOrder),
         InputWindowOrder = ifelse(Event == "GameRunning", "-1", InputWindowOrder),
         InputWindowOrderWithDecision = InputWindowOrder,
         InputWindowOrder = ifelse(InputWindow == "Closed", "-1", InputWindowOrder),
         Period = NA,
         Period = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", "RestPeriod", Period),
         Period = ifelse(Event == "InputWindowChange" & InputWindowClosedID == max(InputWindowClosedID, na.rm=T), "PostGame", Period),
         Period = ifelse(Event == "InputWindowChange" & InputWindow == "Open", "OpenPeriod", Period),
         Period = ifelse(Event == "GameRunning", "PreGame", Period),
         InputWindowOrderFilled = InputWindowOrder) %>%
  tidyr::fill(InputWindowOrderFilled, .direction="down") %>%
  tidyr::fill(Period, .direction="down")


# InputWindowOrder should be numeric but can contain the value "Stopped"
# if the game was interrupted. Change "Stopped" to NA.
D <-D %>% mutate(InputWindowOrder = as.numeric(InputWindowOrder),
                 InputWindowOrderWithDecision = as.numeric(InputWindowOrderWithDecision),
                 InputWindowOrderFilled = as.numeric(InputWindowOrderFilled))

D = D %>% group_by(Participant, Condition) %>%
  mutate(time_thres = lead(time_delta < 1.0),
         not_same = InputWindowOrderFilled != lead(InputWindowOrderFilled),
         not_na = InputWindowOrderFilled == -1,
         InputWindowOrderFilledSoft = ifelse( InputWindowOrderFilled != lead(InputWindowOrderFilled) &
                                                InputWindowOrderFilled == -1 & 
                                                lead(time_delta) < 1.0, lead(InputWindowOrderFilled), InputWindowOrderFilled),
         InputWindowOrderFilledSoft = ifelse(InputWindowOrderFilled != lag(InputWindowOrderFilled) &
                                               InputWindowOrderFilled == -1 & 
                                               time_delta > 1.0, lag(InputWindowOrderFilled), InputWindowOrderFilledSoft),
         InputWindowOrderFilledSoft = ifelse(InputWindowOrderFilled != lag(InputWindowOrderFilled,2) &
                                               InputWindowOrderFilled == -1 & 
                                               time_delta+lag(time_delta) > 1.0, lag(InputWindowOrderFilled,2), InputWindowOrderFilledSoft),
         InputWindowOrderFilledSoft = ifelse( InputWindowOrderFilled != lead(InputWindowOrderFilled,2) &
                                                InputWindowOrderFilled == -1 & 
                                                lead(time_delta)+lead(time_delta,2) < 1.0, lead(InputWindowOrderFilled,2), InputWindowOrderFilledSoft))

# Add PeriodWithDecision
D = D %>% group_by(Participant, Condition) %>% mutate(
  PeriodWithDecision = ifelse(Event == "GameDecision", "OpenPeriod", Period),
  InputWindowOrderFilledSoft = ifelse(Event == "GameDecision", InputWindowOrderWithDecision, InputWindowOrderFilledSoft)
)

#D %>% filter(Participant == 1, Condition == "AS", Event != "Sample") %>%
#  select(Participant, Condition, Timestamp, Event,
#         InputWindowOrder, InputWindowOrderWithRest, InputWindowOrderWithDecision, InputWindowOrderFilledSoft, Period, PeriodWithDecision, TrialResult, TrialFeedback,
#         fishFeedback, fishLost) %>% view()

#############
# Filter out invalid conditions
#############

valid_conditions = c("AS", "NO", "IO", "MF")

D = D %>% filter(Condition %in% valid_conditions)

############
# Load Likert Data
#############

# Fill empty data for missing participants

# Load data from Google Sheets
L <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Mzi9jYgHLKltmW6SyHuHj9nJUi_tBmGrDjCqR1Dr024/edit?usp=sharing')

# Mutate Easiest/Hardest
L = L %>% mutate(Easiest = ifelse(Easiest == "Yes",1,0),
                 Easiest = ifelse(is.na(Easiest),0,Easiest),
                 Hardest = ifelse(Hardest == "Yes",1,0),
                 Hardest = ifelse(is.na(Hardest),0,Hardest))

# Create Normalized versions of pacing, how much help, liked help

L = L %>% mutate(HowMuchHelpNormalized = `“How much did you feel the game helped you?”`,
                 HowMuchHelpNormalized = ((HowMuchHelpNormalized-1)/6),
                 LikedHelpNormalized = `“I liked how the game helped me.”`,
                 LikedHelpNormalized = ((LikedHelpNormalized-1)/6),
                 PacingNormalized = `"I felt the pacing of the game was"`,
                 PacingNormalized = ((PacingNormalized-1)/6),
                 IrritationNormalized = `"How irritated did you feel in this condition?"`,
                 IrritationNormalized = ((IrritationNormalized-1)/6))

# TODO: Specific questions to each PAM.
L = L %>% mutate(`"I liked it when she took the fish up a notch at times, when I couldn’t"` = ((IOPositiveQuote-1)/6),
                 `"It irritated me that she interefered with the game."` = ((IONegativeQuote-1)/6),
                 `"I think it was useful that he got strong and helped me reel in the fish."` = ((ASPositiveQuote-1)/6),
                 `"He got stronger, but I didn’t think it helped me much."` = ((ASNegativeQuote-1)/6),
                 `"When the fish stood still, it was like saying “Let’s just try that again!"` = ((MFPositiveQuote-1)/6),
                 `"When the fish stood still, it felt like the game went slower."` = ((MFNegativeQuote-1)/6))


# Make sure questions not available in NO condition are reported as "0".
L = L %>% mutate(HowMuchHelpNormalized = ifelse(Condition == "NO",0,HowMuchHelpNormalized),
                 LikedHelpNormalized = ifelse(Condition == "NO",0,LikedHelpNormalized))


## Get Experiment-wide questions

L = L %>% mutate(EnjoyedPlaying = `"I enjoyed playing this game very much."`,
                 EnjoyedPlaying = ((EnjoyedPlaying-1)/6),
                 GoodAtPlaying = `"I felt I was good at playing this game."`,
                 GoodAtPlaying = ((HowMuchHelpNormalized-1)/6),
                 EnjoyedGameStyle = `"I enjoyed the way the game was styled."`,
                 EnjoyedGameStyle = ((EnjoyedGameStyle-1)/6),
                 ThinkHowToDoBetter = `"I was thinking about how I could be better at catching fish."`,
                 ThinkHowToDoBetter = ((ThinkHowToDoBetter-1)/6),
                 LosingFishNotIrritate = `"Losing the fish is not something that irritates me."`,
                 LosingFishNotIrritate = ((LosingFishNotIrritate-1)/6),
                 BadAtBlinkingIrritate = `"It irritated me how bad I was at blinking correctly."`,
                 BadAtBlinkingIrritate = ((BadAtBlinkingIrritate-1)/6),
                 GameNotRegisterIrritate = `"It irritated me when the game did not register my blinks."`,
                 GameNotRegisterIrritate = ((GameNotRegisterIrritate-1)/6),
                 PerceivedPerformance = gsub("%", "", L$PerceivedPerformance),
                 PerceivedPerformance = (as.numeric(PerceivedPerformance)/100))


D <- D %>% left_join(L, by=c('Condition' = 'Condition', 'Participant' = 'Participant'))


############
# Factor
#############

# Replace NA values with a '0' rating for help variables.
D = D %>% mutate(LikedHelp.f = `“I liked how the game helped me.”`,
                 LikedHelp.f = ifelse(is.na(LikedHelp.f),1,LikedHelp.f),
                 HowMuchHelp.f = `“How much did you feel the game helped you?”`,
                 HowMuchHelp.f = ifelse(is.na(HowMuchHelp.f),1,HowMuchHelp.f))

D = D %>% mutate(Condition.f = factor(Condition, levels=c("NO", "AS","IO","MF")),
             Perc.f = factor(PerceivedControlEpisode, levels=c("1","2","3","4","5","6","7")),
             Frust.f = factor(FrustrationEpisode, levels=c("1","2","3","4","5","6","7")),
             Pacing.f = factor(`"I felt the pacing of the game was"`, levels=c("1","2","3","4","5","6","7")),
             LikedHelp.f = factor(LikedHelp.f, levels=c("1","2","3","4","5","6","7")),
             HowMuchHelp.f = factor(HowMuchHelp.f, levels=c("1","2","3","4","5","6","7")),
             Irritation.f = factor(`"How irritated did you feel in this condition?"`, levels=c("1","2","3","4","5","6","7")),
             Gender.f = factor(Gender, levels=c("F","M")),
             Fatigue.f = factor(Fatigue, levels=c("No", "Yes")),
             BCIExp.f = factor(BCIExp, levels=c("Yes","No")),
             Order.f = factor(Order, levels=c(1,2,3,4)),
             Hardest.f = factor(Hardest, levels=c(1,0)),
             Easiest.f = factor(Easiest, levels=c(1,0)),
             Participant.f = as.factor(Participant))

#############
# Save to RDA
#############
save(D, file = 'data_pam.rda', compress=TRUE)
