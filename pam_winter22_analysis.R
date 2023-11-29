library(plotly)
library(tidyverse)
library(lme4)
library(MuMIn)
library(psych)
options("digits.secs"=6)
options(max.print=1000)

source("utils/visutils.R")
source("utils/calcutils.R")
source("utils/clmcalcutils.R")

fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_pam.rda')

# What are we doing:
# - a more elaborate version of the study as previously investigated. which means:
# need to make graphs of the 4 conditions (NO, IO, AS, ..) in terms of "pacing", "how much help", "how was helped"


#slice(1)


#############
# Summaries
#############
posTrialLabels = c("OverrideInput","AccInput","AugSuccess","ExplicitSham","AssistSuccess")

# participants excluded due to blink recognition issues.
excluded_participants = c(17)

D = D %>% filter(!Participant %in% excluded_participants)

# Experiment-wide variables
Se <- D %>% group_by(Participant) %>%
  summarize(`"It irritated me how bad I was at blinking correctly."` = unique(`"It irritated me how bad I was at blinking correctly."`),
            `"Losing the fish is not something that irritates me."` = unique(`"Losing the fish is not something that irritates me."`),
            `"I was thinking about how I could be better at catching fish."` = unique(`"I was thinking about how I could be better at catching fish."`),
            `"I enjoyed the way the game was styled."` = unique(`"I enjoyed the way the game was styled."`),
            `"I felt I was good at playing this game."` = unique(`"I felt I was good at playing this game."`),
            `"I enjoyed playing this game very much."` = unique(`"I enjoyed playing this game very much."`),
            `"It irritated me when the game did not register my blinks."` = unique(`"It irritated me when the game did not register my blinks."`)
            )

St <- D %>% group_by(Participant, Condition) %>%
  summarise(rejInput = sum(TrialResult == "RejInput", na.rm=T),
            accInput = sum(TrialResult == "AccInput", na.rm=T),
            posTrial = sum(TrialResult %in% posTrialLabels, na.rm=T),
            triggerTrial = sum(TrialResult == "AccInput" | TrialResult == "AugSuccess" | TrialResult == "AssistSuccess", na.rm=T),
            assistInput = sum(TrialResult %in% c("AssistSuccess", "AugSuccess"), na.rm=T),
            explicitSham = sum(TrialResult %in% c("ExplicitSham", "OverrideInput"), na.rm=T),
            mitigateFail = sum(TrialResult %in% c("AssistFail","MitigateFail"), na.rm=T),
            totalTrials2 = sum(!is.na(TrialResult), na.rm=T),
            totalTrials = rejInput+accInput+assistInput+explicitSham+mitigateFail,
            #fishCaught = sum(FishEvent == "FishCaught", na.rm=T),
            #fishCaught2 = sum(Event == "GameDecision" & fishFeedback == 1, na.rm=T),
            fishCaught = sum(TrialFeedback == "FishCaught", na.rm=T),
            fishCaught_n = scales::rescale(fishCaught, from=c(0, 25)),
            fishReel = sum(TrialFeedback == "Reel", na.rm=T),
            fishReel_a = sum(TrialFeedback %in% c("Reel","FishCaught"), na.rm=T),
            fishReel_b = sum(TrialFeedback %in% c("Reel","FishCaught") & TrialResult == "AccInput", na.rm=T),
            fishReel_n = scales::rescale(fishReel_a, from=c(0, 25)),
            fishStay = sum(TrialFeedback == "Stay", na.rm=T),
            fishStay_n = scales::rescale(fishStay, from=c(0, 25)),
            fishUnreel = sum(TrialFeedback == "Unreel", na.rm=T),
            fishUnreel_n = scales::rescale(fishUnreel, from=c(0, 25)),
            fishLost = sum(TrialFeedback == "FishLost", na.rm=T),
            fishLost_n = scales::rescale(fishLost, from=c(0, 25)),
            notFishCaught = sum(Event == "GameDecision" & lead(Event) != "FishEvent"),
            reel = sum(Event == "GameDecision" & lead(Event) != "FishEvent" & !(TrialResult %in% c("RejInput", "AssistFail", "MitigateFail"))),
            escape = sum(Event == "GameDecision" & lead(Event) != "FishEvent" & (TrialResult %in% c("RejInput"))),
            trial_rate_accept = (accInput + assistInput) / totalTrials,
            trial_rate_reject = rejInput / totalTrials,
            trial_rate_assist = assistInput / totalTrials,
            trial_rate_sham = explicitSham / totalTrials,
            trial_rate_mitigate = mitigateFail / totalTrials,
            trial_rate_positive = (accInput+assistInput+explicitSham) / (totalTrials-mitigateFail),
            pam_rate = 0,
            pam_rate = unique(ifelse(Condition == "AS", trial_rate_assist, pam_rate)),
            pam_rate = unique(ifelse(Condition == "IO", trial_rate_sham, pam_rate)),
            pam_rate = unique(ifelse(Condition == "MF", trial_rate_mitigate, pam_rate)),
            accRecogRate = posTrial / sum(TrialGoal == "OverrideInput" | TrialGoal == "AccInput" | TrialGoal == "AugSuccess" | TrialGoal == "ExplicitSham" | TrialGoal == "AssistSuccess", na.rm=T),
            time_total = sum(time_delta),
            time_total_n = scales::rescale(time_total, from=c(0, 200)),
            Gender = unique(Gender),
            Gender.f = unique(Gender.f),
            Age = unique(Age),
            Fatigue = unique(Fatigue),
            Fatigue.f = unique(Fatigue.f),
            bci_experience = unique(BCIExp),
            bci_experience.f = unique(BCIExp.f),
            Order = unique(Order),
            Order.f = unique(Order.f),
            PercNormalized = unique(PercNormalized),
            Perc.f = unique(Perc.f),
            FrustNormalized = unique(FrustNormalized),
            Frust.f = unique(Frust.f),
            Hardest = unique(Hardest),
            Hardest.f = unique(Hardest.f),
            Easiest = unique(Easiest),
            Easiest.f = unique(Easiest.f),
            HowMuchHelpNormalized = unique(HowMuchHelpNormalized),
            HowMuchHelp.f = unique(HowMuchHelp.f),
            LikedHelpNormalized = unique(LikedHelpNormalized),
            LikedHelp.f = unique(LikedHelp.f),
            PacingNormalized = unique(PacingNormalized),
            Pacing.f = unique(Pacing.f),
            IrritationNormalized = unique(IrritationNormalized),
            Irritation.f = unique(Irritation.f),
            IOPositiveQuote = unique(IOPositiveQuote),
            IONegativeQuote = unique(IONegativeQuote),
            ASPositiveQuote = unique(ASPositiveQuote),
            ASNegativeQuote = unique(ASNegativeQuote),
            MFPositiveQuote = unique(MFPositiveQuote),
            MFNegativeQuote = unique(MFNegativeQuote),
            Condition.f = unique(Condition.f),
            ConditionLabel = unique(ConditionLabel),
            Participant.f = unique(Participant.f)
            )

St = St %>% mutate(ConditionLabel = ifelse(ConditionLabel == "AugmentedSucces","Aug.\n Success",ConditionLabel),
                   ConditionLabel = ifelse(ConditionLabel == "MitigatedFailure","Mit.\n Failure",ConditionLabel),
                   ConditionLabel = ifelse(ConditionLabel == "OverrideInput","Overr.\n Input",ConditionLabel),
                   ConditionLabel = ifelse(ConditionLabel == "Control","Ref.",ConditionLabel))

# Blink counts
St <- D %>% ungroup() %>% group_by(Participant, Condition) %>%
  summarize(blinks_total = sum(Event == "EyeOpening" & Period == "OpenPeriod")
  ) %>% right_join(St)

# Group by input window. Count the number of attempts in each window.
St <- D %>% ungroup() %>% filter(PeriodWithDecision %in% c("OpenPeriod")) %>% group_by(Participant, Condition, InputWindowOrderFilledSoft) %>%
  summarize(blink_recog_window = ifelse(sum(Event %in% c("EyeOpening","EyeClosing") > 0), 1,0), #Whether Blinks happened in the window
            blink_recog_window_count = sum(Event %in% c("EyeOpening","EyeClosing")), #How much Blink happened in the window
            time_window = sum(time_delta),
            TrialResult = paste(na.omit(unique(TrialResultWindow)), collapse=" ")) %>% 
  filter(InputWindowOrderFilledSoft > -1) %>% ungroup() %>% group_by(Participant, Condition) %>%
  summarize(blink_recog_trial = sum(blink_recog_window > 0),
            blink_recog_window = sum(blink_recog_window),
            blink_conv_trial = sum(blink_recog_window > 0 & TrialResult %in% posTrialLabels),
            blink_recog_window_count = sum(blink_recog_window_count),
            time_window = sum(time_window),
            time_window_min = time_window / 60) %>%
  right_join(St)

# Si = Summary of Input Windows
# Feedback Delay
# Fabricated delay
Si <- D %>% ungroup() %>% group_by(Participant, Condition, InputWindowOrderFilled) %>%
  filter(Event == "EyeOpening") %>%
  summarize(last_attempt_stamp = max(Timestamp)) %>% ungroup()

Si <- D %>% ungroup() %>% group_by(Participant, Condition, InputWindowOrderWithDecision) %>%
  filter(Event == "GameDecision") %>%
  summarize(feedback_stamp = max(Timestamp)) %>%
  mutate(InputWindowOrderFilled = InputWindowOrderWithDecision) %>% ungroup() %>% right_join(Si)

Si <- Si %>% filter(!InputWindowOrderFilled == -1) %>%
  mutate(feedback_delay = feedback_stamp - last_attempt_stamp,
         feedback_delay = as.numeric(feedback_delay))

#fig %>% add_trace(x=~InputWindowOrderFilled, y=~feedback_delay, data=Si, type='scatter')

St <- Si %>% group_by(Participant, Condition) %>%
  summarize(mean_delay = mean(feedback_delay)) %>% right_join(St) %>%
  mutate(mean_delay = ifelse(is.na(mean_delay), 0, mean_delay))


St <- St %>% group_by(Participant, Condition) %>%
  summarize(rate_blink = triggerTrial / blinks_total) %>%
  mutate(rate_blink = ifelse(rate_blink == Inf, 0, rate_blink)) %>% right_join(St)

St <- St %>% ungroup() %>%
  mutate(rate_feedback = posTrial/ totalTrials,
         session = 1,
         session = cumsum(session))

# Calculate blink recognition and blink conversion rate
St <- St %>% mutate(
  # blink recognition: in how many trials were blinks recognized out of 20
  blink_recog = blink_recog_trial / totalTrials,
  # blink conv. rate: how many blinks triggered positive feedback out of total blink attempts.
  blink_conv_rate = blink_conv_trial / blink_recog_window_count
)

# Calculate a "help rate" in %
St <- St %>% mutate(
  rate_help = 0,
  rate_help = ifelse(Condition == "AS", assistInput, rate_help),
  rate_help = ifelse(Condition == "IO", explicitSham, rate_help),
  rate_help = ifelse(Condition == "MF", mitigateFail, rate_help),
  rate_help = rate_help / totalTrials
)

Sp <- St %>% ungroup() %>% group_by(Participant) %>%
  summarize(Gender = unique(Gender),
            Age = unique(Age),
            mean_perc = mean(PercNormalized),
            mean_frust = mean(FrustNormalized),
            mean_feedback = mean(rate_feedback),
            mean_rate_blink = mean(rate_blink),
            mean_blink_recog = mean(accRecogRate)
            )
#save(St, file = 'pamSurrogate.rda', compress=TRUE)

Sc <- St %>% group_by(Condition) %>% 
  summarize(
    PercNormalized_SD = sd(PercNormalized),
    Perc.f_SD = sd(as.numeric(Perc.f)),
    FrustNormalized_SD = sd(FrustNormalized),
    Frust.f_SD = sd(as.numeric(Frust.f)),
    rate_feedback_SD = sd(rate_feedback),
    rate_blink_SD = sd(blink_conv_rate),
    rate_help_SD = sd(rate_help),
    blink_recog_SD = sd(accRecogRate),
    fishCaught_SD = sd(fishCaught),
    fishLost_SD = sd(fishLost),
    fishReel_SD = sd(fishReel),
    fishUnreel_SD = sd(fishUnreel),
    time_total_SD = sd(time_total),
    HowMuchHelpNormalized_SD = sd(HowMuchHelpNormalized),
    HowMuchHelp.f_SD = sd(as.numeric(HowMuchHelp.f)),
    LikedHelpNormalized_SD = sd(LikedHelpNormalized),
    LikedHelp.f_SD = sd(as.numeric(LikedHelp.f)),
    PacingNormalized_SD = sd(PacingNormalized),
    Pacing.f_SD = sd(as.numeric(Pacing.f)),
    IrritationNormalized_SD = sd(IrritationNormalized),
    Irritation.f_SD = sd(as.numeric(Irritation.f)),
    PercNormalized = mean(PercNormalized),
    Perc.f = mean(as.numeric(Perc.f)),
    FrustNormalized = mean(FrustNormalized),
    Frust.f = mean(as.numeric(Frust.f)),
    rate_feedback = mean(rate_feedback),
    rate_blink = mean(blink_conv_rate),
    rate_help = mean(rate_help),
    blink_recog = mean(accRecogRate),
    fishCaught = mean(fishCaught),
    fishLost = mean(fishLost),
    fishReel = mean(fishReel),
    fishUnreel = mean(fishUnreel),
    HowMuchHelpNormalized = mean(HowMuchHelpNormalized),
    HowMuchHelp.f = mean(as.numeric(HowMuchHelp.f)),
    LikedHelpNormalized = mean(LikedHelpNormalized),
    LikedHelp.f = mean(as.numeric(LikedHelp.f)),
    PacingNormalized = mean(PacingNormalized),
    Pacing.f = mean(as.numeric(Pacing.f)),
    IrritationNormalized = mean(IrritationNormalized),
    Irritation.f = mean(as.numeric(Irritation.f)),
    Hardest = sum(Hardest),
    Easiest = sum(Easiest),
    time_total = mean(time_total)
  )

# ICC Scores

#PercNormalized = St %>% ungroup() %>% select(Participant, PercNormalized, Condition) %>%
#  pivot_wider(names_from = Participant, values_from = PercNormalized) %>%
#  ungroup() %>% select(-Condition) %>% 
#  psych::ICC(.) %>% view()



Sicc <- tibble(
  PercNormalized = St %>% ungroup() %>% select(Participant, PercNormalized, Condition) %>%
    pivot_wider(names_from = Participant, values_from = PercNormalized) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  FrustNormalized = St %>% ungroup() %>% select(Participant, FrustNormalized, Condition) %>%
    pivot_wider(names_from = Participant, values_from = FrustNormalized) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  HowMuchHelpNormalized = St %>% ungroup() %>% select(Participant, HowMuchHelpNormalized, Condition) %>%
    filter(Condition != "NO") %>%
    pivot_wider(names_from = Participant, values_from = HowMuchHelpNormalized) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  LikedHelpNormalized = St %>% ungroup() %>% select(Participant, LikedHelpNormalized, Condition) %>%
    filter(Condition != "NO") %>%
    pivot_wider(names_from = Participant, values_from = LikedHelpNormalized) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  PacingNormalized = St %>% ungroup() %>% select(Participant, PacingNormalized, Condition) %>%
    pivot_wider(names_from = Participant, values_from = PacingNormalized) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  IrritationNormalized = St %>% ungroup() %>% select(Participant, IrritationNormalized, Condition) %>%
    pivot_wider(names_from = Participant, values_from = IrritationNormalized) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]], 
  Hardest = St %>% ungroup() %>% select(Participant, Hardest, Condition) %>%
    pivot_wider(names_from = Participant, values_from = Hardest) %>% 
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  Easiest = St %>% ungroup() %>% select(Participant, Easiest, Condition) %>%
    pivot_wider(names_from = Participant, values_from = Easiest) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
) %>% 
  select(`Perc. Control` = PercNormalized, `Frustration` = FrustNormalized, `Help Quantity` = HowMuchHelpNormalized,
         `Help Appeal` = LikedHelpNormalized, `Pacing` = PacingNormalized, `Irritation` = IrritationNormalized, Hardest, Easiest) %>%
  pivot_longer(cols=everything(), names_to = "Variables", values_to="ICC3")


#test = St %>% ungroup() %>% select(Participant, PercNormalized, Condition) %>%
#  pivot_wider(names_from = Participant, values_from = PercNormalized) %>%
#  ungroup() %>% select(-Condition)

#############
# Correlations
#############

corr_colvars = c("LooseNotIrritate",
                 "EnjoyGameStyle",
                 "EnjoyGame",
                 "IrriBadAtBlink",
                 "ThinkBetterFish",
                 "FeltGoodPlaying",
                 "IrriRegisterBlink",
                 "HowMuchHelpNormalized",
                 "LikedHelpNormalized",
                 "ASPositiveQuote",
                 "MFPositiveQuote",
                 "MFNegativeQuote",
                 "ASNegativeQuote",
                 "IONegativeQuote",
                 "IOPositiveQuote")

corr_rowvars = c(corr_colvars, "PercNormalized", 
                 "FrustNormalized",
                 "rate_blink",
                 "accRecogRate",
                 "rate_feedback",
                 "fishCaught",
                 "fishLost",
                 "fishReel",
                 "fishUnreel",
                 "HowMuchHelpNormalized",
                 "LikedHelpNormalized",
                 "PacingNormalized",
                 "IrritationNormalized",
                 "time_total",
                 "Order",
                 "Hardest",
                 "Easiest",
                 "rate_help")

# There is an expected order effect on LikedHelp and HowMuchHelp because 
# the default "reference condition" is included in it.

Ste = St %>% left_join(Se) %>% 
        rename(LooseNotIrritate = `"Losing the fish is not something that irritates me."`,
              EnjoyGameStyle = `"I enjoyed the way the game was styled."`,
              EnjoyGame = `"I enjoyed playing this game very much."`,
              IrriBadAtBlink = `"It irritated me how bad I was at blinking correctly."`,
              ThinkBetterFish = `"I was thinking about how I could be better at catching fish."`,
              FeltGoodPlaying = `"I felt I was good at playing this game."`,
              IrriRegisterBlink = `"It irritated me when the game did not register my blinks."`) %>%
        group_by(Participant) %>%
        mutate(IOPositiveQuote = sum(IOPositiveQuote, na.rm=T),
               IONegativeQuote = sum(IONegativeQuote, na.rm=T),
               MFPositiveQuote = sum(MFPositiveQuote, na.rm=T),
               MFNegativeQuote = sum(MFNegativeQuote, na.rm=T),
               ASPositiveQuote = sum(ASPositiveQuote, na.rm=T),
               ASNegativeQuote = sum(ASNegativeQuote, na.rm=T))

Stef = Ste %>% filter(Condition != "NO")
help_only = c("HowMuchHelpNormalized","LikedHelpNormalized")

corr_table = expand.grid(corr_colvars, corr_rowvars) %>% 
  rename(x = Var1, y=Var2) %>% rowwise() %>%
  mutate(the_coef = cor(Ste[[x]],Ste[[y]], method="spearman"),
         help_coef = cor(Stef[[x]],Stef[[y]], method="spearman"),
         the_coef = ifelse(x %in% help_only, help_coef, the_coef),
         z = ifelse(y %in% help_only, help_coef, the_coef),
         help_coef = NULL,
         the_coef = NULL
        )
         
         

corr_table_f = corr_table %>% 
  mutate(across(everything(), 
         ~ str_replace_all(.x, c("Order" = "Condition Order",
                                 "FrustNormalized" = "Frustration",
                                 "PercNormalized" = "Perc. Control",
                                 "Participant.f" = "Participant",
                                 "LikedHelp.f" = "Help Appeal",
                                 "rate_blink" = "Blink Conv. Rate",
                                 "rate_feedback" = "Pos. Feedback",
                                 "fishCaught" = "Fish Caught",
                                 "fishLost" = "Fish Lost",
                                 "accRecogRate" = "Blink Recognition",
                                 "time_total" = "Duration",
                                 "fishReel" = "Fish Reel",
                                 "rate_help" = "Help Rate",
                                 "fishUnreel" = "Fish Unreel",
                                 "HowMuchHelpNormalized" = "Help Quantity",
                                 "LikedHelpNormalized" = "Help Appeal",
                                 "PacingNormalized" = "Pacing",
                                 "IrritationNormalized" = "Irritation",
                                 "LooseNotIrritate" = "Losing the fish is not \n something that irritates me.",
                                 "EnjoyGameStyle" = "I enjoyed the way \n the game was styled.",
                                 "EnjoyGame" = "I enjoyed playing this \n game very much.",
                                 "IrriBadAtBlink" = "It irritated me how bad \n I was at blinking correctly.",
                                 "ThinkBetterFish" = "I was thinking about how \n I could be better at catching fish.",
                                 "FeltGoodPlaying" = "I felt I was good at \n playing this game.",
                                 "IrriRegisterBlink" = "It irritated me when the game \n did not register my blinks.",
                                 "IOPositiveQuote" =  "I liked it when she took the \n fish up a notch at times, when I couldn’t",
                                 "IONegativeQuote" = "It irritated me that she \n interefered with the game.",
                                 "ASPositiveQuote" ="I think it was useful that he got \n strong and helped me reel in the fish.",
                                 "ASNegativeQuote" = "He got stronger, but I didn’t \n think it helped me much.",
                                 "MFPositiveQuote" = "When the fish stood still, it was like \n saying “Let’s just try that again!",
                                 "MFNegativeQuote" = "When the fish stood still, it felt \n like the game went slower."
                                 ))))
              

corr_colvars_f = rev(c(
  "When the fish stood still, it was like \n saying “Let’s just try that again!",
  "When the fish stood still, it felt \n like the game went slower.",
  "I liked it when she took the \n fish up a notch at times, when I couldn’t",
  "It irritated me that she \n interefered with the game.",
  "I think it was useful that he got \n strong and helped me reel in the fish.",
  "He got stronger, but I didn’t \n think it helped me much.",
  "Losing the fish is not \n something that irritates me.",
  "I enjoyed the way \n the game was styled.",
  "I enjoyed playing this \n game very much.",
  "It irritated me how bad \n I was at blinking correctly.",
  "I was thinking about how \n I could be better at catching fish.",
  "I felt I was good at \n playing this game.",
  "It irritated me when the game \n did not register my blinks."
))
corr_rowvars_f = rev(c(corr_colvars_f,
  "Perc. Control",
  "Frustration",
  "Help Quantity",
  "Help Appeal",
  "Pacing",
  "Irritation",
  "Hardest",
  "Easiest",
  "Blink Recognition",
  "Blink Conv. Rate",
  "Pos. Feedback",
  "Help Rate",
  "Fish Caught",
  "Fish Lost",
  "Fish Reel",
  "Fish Unreel",
  "Duration",
  "Condition Order"
  ))
               
fig_p = plot_ly(data = corr_table_f, x=~x, xgap=3, ygap=3, y=~y, z=~z, 
        texttemplate="%{z:.1f}", type = "heatmap", colors=colorRampPalette(colors = c("white", "#000000"))(20),
        showscale=F, showlegend=F, textfont=list(size=18)) %>%
  layout(xaxis=list(title="", side="top", tickfont = list(size = 14),
                    categoryorder = "array",
                    categoryarray = corr_colvars_f),
         yaxis=list(title="", tickfont = list(size = 16),
                    categoryorder = "array",
                    categoryarray = corr_rowvars_f))
orca(fig_p, "fig/corrmap_vars.pdf", width=1800, height=1800)

# cor(x = St[[x]], y = St[[y]])
#StCorr = GGally::ggcorr(St %>% ungroup() %>% left_join(Se) 
#               %>% select(any_of(corr_rowvars)),
#               label=T, label_size=2)

# pacing (x), enjoying game (y)
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(rate_blink,amount=.02), 
            y=~jitter(PercNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1)),
         yaxis=list(range=c(-0.1,1.1)))
fig_p
orca(fig_p, "fig/corrmap_vars.pdf", width=1800, height=1800)

# perceived control (x), enjoyment (y)
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(PercNormalized,amount=.02), 
            y=~jitter(`"I enjoyed playing this game very much."`,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1)),
         yaxis=list(range=c(-0.1,8)))
fig_p

# IOPositive (x), enjoyment (y)
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(IONegativeQuote,amount=.02), 
            y=~jitter(`"I enjoyed playing this game very much."`,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,8)),
         yaxis=list(range=c(-0.1,8)))
fig_p

# IO Positive (x), self-blame (y)
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(IOPositiveQuote,amount=.02), 
            y=~jitter(`"It irritated me how bad I was at blinking correctly."`,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,8)),
         yaxis=list(range=c(-0.1,8)))
fig_p


# Enjoy styling (x) enjoy game (y)
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(`"I enjoyed the way the game was styled."`,amount=.2), 
            y=~jitter(`"I enjoyed playing this game very much."`,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,8)),
         yaxis=list(range=c(-0.1,8)))
fig_p

# perceived control (x), liked help (y)
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(PercNormalized,amount=.02), 
            y=~jitter(LikedHelpNormalized,amount=.02), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1)),
         yaxis=list(range=c(-0.1,1.1)))
fig_p


#############
# Exploratory Factor Analysis
#############

Stf <- St %>% select(Participant,
                     Condition.f,
                     PercNormalized,
                     FrustNormalized,
                     LikedHelpNormalized,
                     HowMuchHelpNormalized,
                     PacingNormalized,
                     IrritationNormalized,
                     ASNegativeQuote,
                     ASPositiveQuote,
                     IONegativeQuote,
                     IOPositiveQuote,
                     MFNegativeQuote,
                     MFPositiveQuote)

# Repeat Quote ratings to remove missing data problem.
Stf <- Stf %>% ungroup() %>% group_by(Participant) %>%
  tidyr::fill(ASNegativeQuote,
              ASPositiveQuote,
              IONegativeQuote,
              IOPositiveQuote,
              MFNegativeQuote,
              MFPositiveQuote,
              .direction="downup")

# KMO test: how suitable is the data for Factor analysis.
KMO(Stf %>% select(-Condition.f))
KMO(Stf %>% select(-Condition.f))$MSAi>0.50


cortest.bartlett(Stf %>% select(-Condition.f))


#############
# Latex Table: Condition Quote Ratings
#############
Sq <- St %>% group_by(Participant) %>% summarize(
                    `"I liked it when she took the fish up \n a notch at times, when I couldn’t"` = sum(IOPositiveQuote, na.rm=T),
                    `"It irritated me that she interefered \n with the game."` = sum(IONegativeQuote, na.rm=T),
                    `"I think it was useful that he got \n strong & helped me reel in the fish."` = sum(ASPositiveQuote, na.rm=T),
                    `"He got stronger, but I didn’t think \n it helped me much."` = sum(ASNegativeQuote,na.rm=T),
                    `"When the fish stood still, it was \n like saying “Let’s just try that again!"` = sum(MFPositiveQuote,na.rm=T),
                    `"When the fish stood still, it felt \n like the game went slower."` = sum(MFNegativeQuote,na.rm=T))

Sq <- St %>% select(Participant,
                    IOPositiveQuote,
                    IONegativeQuote,
                    ASPositiveQuote,
                    ASNegativeQuote,
                    MFPositiveQuote,
                    MFNegativeQuote)

Sq <- Sq %>% pivot_longer(cols=-c("Participant"), names_to = "Variable") %>% drop_na() %>%
  mutate(y = 1)

# to ensure all histogram bins have same sizes in all cases, add one dummy value to each.
Sqv = Sq %>%
  add_row(tibble_row(
    Participant = 0,
    `"I liked it when she took the fish up \n a notch at times, when I couldn’t"` = 1,
    `"It irritated me that she interefered \n with the game."` = 1,
    `"I think it was useful that he got \n strong & helped me reel in the fish."` = 1,
    `"He got stronger, but I didn’t think \n it helped me much."` = 1,
    `"When the fish stood still, it was \n like saying “Let’s just try that again!"` = 1,
    `"When the fish stood still, it felt \n like the game went slower."` = 1
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"I liked it when she took the fish up \n a notch at times, when I couldn’t"` = 2,
    `"It irritated me that she interefered \n with the game."` = 2,
    `"I think it was useful that he got \n strong & helped me reel in the fish."` = 2,
    `"He got stronger, but I didn’t think \n it helped me much."` = 2,
    `"When the fish stood still, it was \n like saying “Let’s just try that again!"` = 2,
    `"When the fish stood still, it felt \n like the game went slower."` = 2,
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"I liked it when she took the fish up \n a notch at times, when I couldn’t"` = 3,
    `"It irritated me that she interefered \n with the game."` = 3,
    `"I think it was useful that he got \n strong & helped me reel in the fish."` = 3,
    `"He got stronger, but I didn’t think \n it helped me much."` = 3,
    `"When the fish stood still, it was \n like saying “Let’s just try that again!"` = 3,
    `"When the fish stood still, it felt \n like the game went slower."` = 3
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"I liked it when she took the fish up \n a notch at times, when I couldn’t"` = 4,
    `"It irritated me that she interefered \n with the game."` = 4,
    `"I think it was useful that he got \n strong & helped me reel in the fish."` = 4,
    `"He got stronger, but I didn’t think \n it helped me much."` = 4,
    `"When the fish stood still, it was \n like saying “Let’s just try that again!"` = 4,
    `"When the fish stood still, it felt \n like the game went slower."` = 4
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"I liked it when she took the fish up \n a notch at times, when I couldn’t"` = 5,
    `"It irritated me that she interefered \n with the game."` = 5,
    `"I think it was useful that he got \n strong & helped me reel in the fish."` = 5,
    `"He got stronger, but I didn’t think \n it helped me much."` = 5,
    `"When the fish stood still, it was \n like saying “Let’s just try that again!"` = 5,
    `"When the fish stood still, it felt \n like the game went slower."` = 5
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"I liked it when she took the fish up \n a notch at times, when I couldn’t"` = 6,
    `"It irritated me that she interefered \n with the game."` = 6,
    `"I think it was useful that he got \n strong & helped me reel in the fish."` = 6,
    `"He got stronger, but I didn’t think \n it helped me much."` = 6,
    `"When the fish stood still, it was \n like saying “Let’s just try that again!"` = 6,
    `"When the fish stood still, it felt \n like the game went slower."` = 6
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"I liked it when she took the fish up \n a notch at times, when I couldn’t"` = 7,
    `"It irritated me that she interefered \n with the game."` = 7,
    `"I think it was useful that he got \n strong & helped me reel in the fish."` = 7,
    `"He got stronger, but I didn’t think \n it helped me much."` = 7,
    `"When the fish stood still, it was \n like saying “Let’s just try that again!"` = 7,
    `"When the fish stood still, it felt \n like the game went slower."` = 7
  ))


Sq_table_sd = Sq %>% select(-Participant) %>% 
  summarize(across(everything(), list(sd = sd))) %>%
  pivot_longer(cols=everything(), names_to = "Variable") %>%
  mutate(value = format(round(value,2), nsmall = 2),
         Variable = NULL) %>%
  rename(SD = value)


Sq_table = Sq %>% select(-Participant) %>% 
  summarize(across(everything(), list(mean = mean))) %>%
  pivot_longer(cols=everything(), names_to = "Variable") %>%
  mutate(value = format(round(value,2), nsmall = 2)) %>%
  rename(Mean = value, Question = Variable)

Sq_table = Sq_table %>% cbind(Sq_table_sd) %>%
  mutate(Histogram = " ",
         Question = gsub("_mean", "", Question))


fig_p <- fig %>%
  add_histogram(data=Sq, nbinsx = 7, x=~`"I liked it when she took the fish up \n a notch at times, when I couldn’t"`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=1, t=1, b=1),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_LikeSheFish.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sq, nbinsx = 7, x=~`"It irritated me that she interefered \n with the game."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=1, t=1, b=1),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_IrriSheInterfere.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sq, nbinsx = 7, x=~`"I think it was useful that he got \n strong & helped me reel in the fish."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=1, t=1, b=1),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_UsefulHeStrong.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sq, nbinsx = 7, x=~`"He got stronger, but I didn’t think \n it helped me much."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=1, t=1, b=1),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_StrongerNoHelp.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sq, nbinsx = 7, x=~`"When the fish stood still, it was \n like saying “Let’s just try that again!"`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=1, t=1, b=1),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_StillFishTryAgain.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sq, nbinsx = 7, x=~`"When the fish stood still, it felt \n like the game went slower."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=1, t=1, b=1),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_StillFishSlowerGame.pdf", width=450, height=250)




fig_p <- lapply(unique(Sq$Variable), function(var) {
  fig %>%
    add_trace(data=Sq %>% filter(Variable == var), text =~paste0("<b>",Participant,"</b>"), textfont=list(size=14), textangle=0,textposition="inside",
              x=~value, y=~y, type='bar', name=~Participant, marker=list(color=I("lightgrey"), line=list(width=2, color=I('white'))),
              marker = list(line = list(color = "black", width = 1.5))) %>%
    layout(showlegend=F, margin=list(l=1, r=1, t=1, b=0), barmode='stack',
           annotations=list(showarrow=F,x=4, y=8.93,text=paste0(var), font=list(size=7)),
           xaxis=list(range=c(0.50,7.50), ticks='none', visible=T, dtick=1, title=" ", showline=T, tickvals=NULL, showgrid=F, mirror=F),
           yaxis=list(range=c(-0.05,10.05), title="", tickvals=NULL, ticks='none', showline=F, shownumbers=F, visible=F, showgrid=F, mirror=T))
}) %>% subplot(., nrows=1,margin = 0.005) %>% layout(showlegend=F, yaxis=list(title=" "), xaxis=list(title=" "))
fig_p

orca(fig_p, "fig/hist_quotes.pdf", width=750, height=150)


fig_asn <- fig %>%
  add_trace(data=Sq %>% filter(Variable == "ASPositiveQuote"), text =~paste0("<b>",Participant,"</b>"), textfont=list(size=14), textangle=0,textposition="inside",
            x=~value, y=~y, type='bar', name=~Participant, marker=list(color=I("lightgrey"), line=list(width=2, color=I('white'))),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=1, r=1, t=1, b=1), barmode='stack',
         xaxis=list(range=c(0.50,7.50), ticks='none', visible=F, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(-0.05,8.05), title="", tickvals=NULL, ticks='none', showline=F, shownumbers=F, visible=F, showgrid=F, mirror=T))
fig_p
orca(fig_p, "fig/hist_ASPositive.pdf", width=160, height=150)

fig_asp <- fig %>%
  add_trace(data=Sq %>% filter(Variable == "ASNegativeQuote"), text =~paste0("<b>",Participant,"</b>"), textfont=list(size=14), textangle=0,textposition="inside",
            x=~value, y=~y, type='bar', name=~Participant, marker=list(color=I("lightgrey"), line=list(width=2, color=I('white'))),
            marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=1, r=1, t=1, b=1), barmode='stack',
         xaxis=list(range=c(0.50,7.50), ticks='none', visible=F, dtick=1, title="", showline=F, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(-0.05,8.05), title="", tickvals=NULL, ticks='none', showline=F, shownumbers=F, visible=F, showgrid=F, mirror=T))

orca(fig_p, "fig/hist_ASNegative.pdf", width=160, height=150)

fig_ion <- fig %>%
  add_trace(data=Sq %>% filter(Variable == "IONegativeQuote"), text =~paste0("<b>",Participant,"</b>"), textfont=list(size=14), textangle=0,textposition="inside",
            x=~value, y=~y, type='bar', name=~Participant, marker=list(color=I("lightgrey"), line=list(width=2, color=I('white'))),
            marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=1, r=1, t=1, b=1), barmode='stack',
         xaxis=list(range=c(0.50,7.50), ticks='none', visible=F, dtick=1, title="", showline=F, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(-0.05,8.05), title="", tickvals=NULL, ticks='none', showline=F, shownumbers=F, visible=F, showgrid=F, mirror=T))

orca(fig_p, "fig/hist_IONegative.pdf", width=160, height=150)

fig_iop <- fig %>%
  add_trace(data=Sq %>% filter(Variable == "IOPositiveQuote"), text =~paste0("<b>",Participant,"</b>"), textfont=list(size=14), textangle=0,textposition="inside",
            x=~value, y=~y, type='bar', name=~Participant, marker=list(color=I("lightgrey"), line=list(width=2, color=I('white'))),
            marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=1, r=1, t=1, b=1), barmode='stack',
         xaxis=list(range=c(0.50,7.50), ticks='none', visible=F, dtick=1, title="", showline=F, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(-0.05,8.05), title="", tickvals=NULL, ticks='none', showline=F, shownumbers=F, visible=F, showgrid=F, mirror=T))

orca(fig_p, "fig/hist_IOPositive.pdf", width=160, height=150)
                       


#############
# Latex Table: Experiment-wide
#############

# to ensure all histogram bins have same sizes in all cases, add one dummy value to each.
Sev = Se %>%
  add_row(tibble_row(
    Participant = 0,
    `"It irritated me how bad I was at blinking correctly."` = 1,
    `"Losing the fish is not something that irritates me."` = 1,
    `"I was thinking about how I could be better at catching fish."` = 1,
    `"I enjoyed the way the game was styled."` = 1,
    `"I felt I was good at playing this game."` = 1,
    `"I enjoyed playing this game very much."` = 1,
    `"It irritated me when the game did not register my blinks."` = 1
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"It irritated me how bad I was at blinking correctly."` = 2,
    `"Losing the fish is not something that irritates me."` = 2,
    `"I was thinking about how I could be better at catching fish."` = 2,
    `"I enjoyed the way the game was styled."` = 2,
    `"I felt I was good at playing this game."` = 2,
    `"I enjoyed playing this game very much."` = 2,
    `"It irritated me when the game did not register my blinks."` = 2
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"It irritated me how bad I was at blinking correctly."` = 3,
    `"Losing the fish is not something that irritates me."` = 3,
    `"I was thinking about how I could be better at catching fish."` = 3,
    `"I enjoyed the way the game was styled."` = 3,
    `"I felt I was good at playing this game."` = 3,
    `"I enjoyed playing this game very much."` = 3,
    `"It irritated me when the game did not register my blinks."` = 3
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"It irritated me how bad I was at blinking correctly."` = 4,
    `"Losing the fish is not something that irritates me."` = 4,
    `"I was thinking about how I could be better at catching fish."` = 4,
    `"I enjoyed the way the game was styled."` = 4,
    `"I felt I was good at playing this game."` = 4,
    `"I enjoyed playing this game very much."` = 4,
    `"It irritated me when the game did not register my blinks."` = 4
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"It irritated me how bad I was at blinking correctly."` = 5,
    `"Losing the fish is not something that irritates me."` = 5,
    `"I was thinking about how I could be better at catching fish."` = 5,
    `"I enjoyed the way the game was styled."` = 5,
    `"I felt I was good at playing this game."` = 5,
    `"I enjoyed playing this game very much."` = 5,
    `"It irritated me when the game did not register my blinks."` = 5
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"It irritated me how bad I was at blinking correctly."` = 6,
    `"Losing the fish is not something that irritates me."` = 6,
    `"I was thinking about how I could be better at catching fish."` = 6,
    `"I enjoyed the way the game was styled."` = 6,
    `"I felt I was good at playing this game."` = 6,
    `"I enjoyed playing this game very much."` = 6,
    `"It irritated me when the game did not register my blinks."` = 6
  )) %>%
  add_row(tibble_row(
    Participant = 0,
    `"It irritated me how bad I was at blinking correctly."` = 7,
    `"Losing the fish is not something that irritates me."` = 7,
    `"I was thinking about how I could be better at catching fish."` = 7,
    `"I enjoyed the way the game was styled."` = 7,
    `"I felt I was good at playing this game."` = 7,
    `"I enjoyed playing this game very much."` = 7,
    `"It irritated me when the game did not register my blinks."` = 7
  ))
  
  
Se_table_sd = Se %>% select(-Participant) %>% 
  summarize(across(everything(), list(sd = sd))) %>%
  pivot_longer(cols=everything(), names_to = "Variable") %>%
  mutate(value = format(round(value,2), nsmall = 2),
         Variable = NULL) %>%
  rename(SD = value)
  

Se_table = Se %>% select(-Participant) %>% 
  summarize(across(everything(), list(mean = mean))) %>%
  pivot_longer(cols=everything(), names_to = "Variable") %>%
  mutate(value = format(round(value,2), nsmall = 2)) %>%
  rename(Mean = value, Question = Variable)

Se_table = Se_table %>% cbind(Se_table_sd) %>%
  mutate(Histogram = " ",
         Question = gsub("_mean", "", Question))

paste(colnames(Se_table), collapse=" & ")
writeLines(paste(Se_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")

Se %>% ungroup() %>% select(Participant, `"It irritated me how bad I was at blinking correctly."`) %>%
  pivot_wider(names_from = Participant, values_from = `"It irritated me how bad I was at blinking correctly."`) %>%
  ungroup() %>% mutate(across(everything(), ~ factor(.x, levels=c(1,2,3,4,5,6,7)))) %>%
  psych::ICC(.) %>% view()

Seicc <- tibble(
  IrriMe = Se %>% ungroup() %>% select(Participant, `"It irritated me how bad I was at blinking correctly."`) %>%
    pivot_wider(names_from = Participant, values_from = `"It irritated me how bad I was at blinking correctly."`) %>%
    ungroup() %>% mutate(across(everything(), ~ factor(.x, levels=c(1,2,3,4,5,6,7)))) %>%
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  FishIrri = Se %>% ungroup() %>% select(Participant, `"Losing the fish is not something that irritates me."`) %>%
    pivot_wider(names_from = Participant, values_from = `"Losing the fish is not something that irritates me."`) %>%
    ungroup() %>% mutate(across(everything(), ~ factor(.x, levels=c(1,2,3,4,5,6,7)))) %>%
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  ThinkFish = Se %>% ungroup() %>% select(Participant, `"I was thinking about how I could be better at catching fish."`) %>%
    pivot_wider(names_from = Participant, values_from = `"I was thinking about how I could be better at catching fish."`) %>%
    ungroup() %>% mutate(across(everything(), ~ factor(.x, levels=c(1,2,3,4,5,6,7)))) %>%
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  StyleEnjoy = St %>% ungroup() %>% select(Participant, `"I enjoyed the way the game was styled."`) %>%
    pivot_wider(names_from = Participant, values_from = `"I enjoyed the way the game was styled."`) %>%
    ungroup() %>% mutate(across(everything(), ~ factor(.x, levels=c(1,2,3,4,5,6,7)))) %>%
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  FeltGood = St %>% ungroup() %>% select(Participant, `"I felt I was good at playing this game."`) %>%
    pivot_wider(names_from = Participant, values_from = `"I felt I was good at playing this game."`) %>%
    ungroup() %>% mutate(across(everything(), ~ factor(.x, levels=c(1,2,3,4,5,6,7)))) %>%
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  EnjoyedGame = St %>% ungroup() %>% select(Participant, `"I enjoyed playing this game very much."`) %>%
    pivot_wider(names_from = Participant, values_from = `"I enjoyed playing this game very much."`) %>%
    ungroup()%>% mutate(across(everything(), ~ factor(.x, levels=c(1,2,3,4,5,6,7)))) %>%
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]], 
  IrriSystem = St %>% ungroup() %>% select(Participant, `"It irritated me when the game did not register my blinks."`) %>%
    pivot_wider(names_from = Participant, values_from = `"It irritated me when the game did not register my blinks."`) %>%
    ungroup() %>% mutate(across(everything(), ~ factor(.x, levels=c(1,2,3,4,5,6,7)))) %>%
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
) %>%
  pivot_longer(cols=everything(), names_to = "Variables", values_to="ICC3")





#pivot_wider(names_from = Participant, values_from = value)
#skimr::skim(Se)

fig_p <- fig %>%
  add_histogram(data=Sev, nbinsx = 7, x=~`"It irritated me when the game did not register my blinks."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=1, t=1, b=1),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_GameNotRegisterIrritate.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sev, nbinsx = 7, x=~`"It irritated me how bad I was at blinking correctly."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=1, t=1, b=1),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_BadAtBlinkingIrritate.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sev, nbinsx = 7, x=~`"Losing the fish is not something that irritates me."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=0, t=0, b=0),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_LosingFishNotIrritate.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sev, nbinsx = 7, x=~`"I was thinking about how I could be better at catching fish."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=0, t=0, b=0),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_ThinkHowToDoBetter.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sev, nbinsx = 7,  x=~`"I enjoyed the way the game was styled."`, color=I('white'), 
                marker = list(line = list(color = "black", width = 1.5)), textsrc='auto', textposition = 'auto') %>%
  layout(showlegend=F, margin=list(l=7, r=0, t=0, b=0),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(0.95,11.5),  title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_EnjoyedGameStyle.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sev, nbinsx = 7, x=~`"I felt I was good at playing this game."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=0, t=0, b=0),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(-0.05,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_GoodAtPlaying.pdf", width=450, height=250)

fig_p <- fig %>%
  add_histogram(data=Sev, nbinsx = 7, x=~`"I enjoyed playing this game very much."`, color=I('white'),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=7, r=0, t=0, b=0),
         xaxis=list(range=c(0.45,7.55), ticks='none', visible=T, dtick=1, title="", showline=T, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(-0.05,11.5), title="", tickvals=NULL, ticks='none', showline=T, shownumbers=F, visible=T, showgrid=F, mirror=T))
orca(fig_p, "fig/hist_EnjoyedPlaying.pdf", width=450, height=250)

#############
# Latex Table: Participants
#############
cri = tibble(lv_perc = c(0.1, 0.35,0.68,0.82,1.1,1.5,2.2),
             lv_percf = c(0.1, 2.3,4.5,5.8,7.8,8.8,9.8),
             lv_frust = rev(lv_perc),
             lv_frustf = rev(lv_percf),
             lv_rate = c(-0.1, 0.35, 0.55, 0.85, 1.1, 1.5, 2.2),
             colors = c("g0","g1", "g2", "g3", "g4","g4","g4"),
             lv_reel = c(0,3,6,9,12,15,18),
             lv_unreel = rev(c(0,3,6,9,12,15,18)),
             lv_fish = c(0,2,4,6,10,12,14),
             lv_diff = rev(c(0,2,4,6,10,12,14)),
             lv_lost = c(8,4,2,1,0,-1,-2),
             lv_time = c(115,130,145,160,175,190,205),
             lv_icc = c(-0.1,0.5,0.73,0.75,0.77,0.90,1.00),
             lv_help = c(-0.1,0.15,0.25,0.35,0.45,0.90,1.00))

St_table <- St %>% group_by(Condition) %>% select(Participant, PercNormalized, FrustNormalized, rate_blink, accRecogRate, rate_feedback,
                                                  fishCaught, fishLost, fishReel, fishUnreel, HowMuchHelpNormalized, LikedHelpNormalized,
                                                  PacingNormalized, IrritationNormalized, time_total, rate_help) %>%
  mutate(
    Condition = ifelse (Condition == "MF", "Mit. Failure", Condition),
    Condition = ifelse (Condition == "NO", "Ref. Condition", Condition),
    Condition = ifelse (Condition == "AS", "Aug. Success", Condition),
    Condition = ifelse (Condition == "IO", "Input Override", Condition),
    muchhelp_c = t_color(HowMuchHelpNormalized, cri$lv_perc, cri$colors),
    likedhelp_c = t_color(LikedHelpNormalized, cri$lv_perc, cri$colors),
    pacing_c = t_color(PacingNormalized, cri$lv_perc, cri$colors),
    irritation_c = t_color(IrritationNormalized, cri$lv_perc, cri$colors),
    perc_c = t_color(PercNormalized, cri$lv_perc, cri$colors),
    frust_c = t_color(FrustNormalized, cri$lv_frust, cri$colors),
    rate_c = t_color(rate_blink, cri$lv_rate, cri$colors),
    rate_acc_c = t_color(accRecogRate, cri$lv_rate, cri$colors),
    feedback_c = t_color(rate_feedback, cri$lv_rate, cri$colors),
    fish_c = t_color(fishCaught, cri$lv_fish, cri$colors),
    lost_c = t_color(fishLost, cri$lv_lost, cri$colors),
    reel_c = t_color(fishReel, cri$lv_reel, cri$colors),
    unreel_c = t_color(fishUnreel, cri$lv_unreel, cri$colors),
    time_c = t_color(time_total, cri$lv_time, cri$colors),
    help_c = t_color(rate_help, cri$lv_help, cri$colors),
    PercNormalized = format(round(PercNormalized,2), nsmall = 2),
    FrustNormalized = format(round(FrustNormalized,2), nsmall = 2),
    HowMuchHelpNormalized = format(round(HowMuchHelpNormalized,2), nsmall = 2),
    LikedHelpNormalized = format(round(LikedHelpNormalized,2), nsmall = 2),
    PacingNormalized = format(round(PacingNormalized,2), nsmall = 2),
    IrritationNormalized = format(round(IrritationNormalized,2), nsmall = 2),
    time_total = format(round(time_total,2), nsmall = 2),
    rate_blink = paste0(format(round(rate_blink * 100,0), nsmall = 0),"\\%"),
    rate_feedback = paste0(format(round(rate_feedback * 100, 0), nsmall = 0), "\\%"),
    accRecogRate =  paste0(format(round(accRecogRate * 100,0), nsmall = 0),"\\%"),
    HowMuchHelpNormalized = paste0("\\cellcolor{", muchhelp_c, "}", HowMuchHelpNormalized),
    LikedHelpNormalized = paste0("\\cellcolor{", likedhelp_c, "}", LikedHelpNormalized),
    PacingNormalized = paste0("\\cellcolor{", pacing_c, "}", PacingNormalized),
    IrritationNormalized = paste0("\\cellcolor{", irritation_c, "}", IrritationNormalized),
    PercNormalized = paste0("\\cellcolor{", perc_c, "}", PercNormalized),
    FrustNormalized = paste0("\\cellcolor{", frust_c, "}", FrustNormalized),
    rate_blink = paste0("\\cellcolor{", rate_c, "}", rate_blink),
    accRecogRate = paste0("\\cellcolor{", rate_acc_c, "}", accRecogRate),
    rate_feedback = paste0("\\cellcolor{", feedback_c, "}", rate_feedback),
    fishCaught = paste0("\\cellcolor{", fish_c, "}", fishCaught),
    fishLost = paste0("\\cellcolor{", lost_c, "}", fishLost),
    fishReel = paste0("\\cellcolor{", reel_c, "}", fishReel),
    fishUnreel = paste0("\\cellcolor{", unreel_c, "}", fishUnreel),
    time_total = paste0("\\cellcolor{", time_c, "}", time_total),
    perc_c = NULL, frust_c = NULL, rate_c = NULL, rate_acc_c = NULL, feedback_c = NULL, time_c = NULL,
    lost_c = NULL, fish_c = NULL, irritation_c = NULL, reel_c = NULL, unreel_c = NULL, pacing_c = NULL, likedhelp_c = NULL, muchhelp_c = NULL,
    across(everything(), as.character)) %>% arrange(Condition) %>%
  rename(`Perc. Control` = PercNormalized, `Frustration` = FrustNormalized, `Blink Conv. Rate` = rate_blink,
         `Pos. Feedback` = rate_feedback, `Fish Caught` = fishCaught, `Fish Lost` = fishLost,
         `Blink Recognition` = accRecogRate, Duration = time_total) %>%
  pivot_longer(cols=-c(Participant, Condition), names_to = "Variable") %>%
  pivot_wider(names_from = Participant, values_from = value)

St_table <- St_table %>% group_by(Condition) %>%
  group_modify(~ add_row(Variable=paste("\\underline{",.y,"}"),.before=0, .x)) %>%
  ungroup() %>% replace(is.na(.)," ") %>%
  select(-Condition)

paste(colnames(St_table), collapse=" & ")
writeLines(paste(St_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")

#############
# Latex Table: Group Level Features
#############

Sc_table <- Sc %>% ungroup() %>% group_by(Condition) %>%
  mutate(
    Condition = ifelse (Condition == "MF", "Mit. Failure", Condition),
    Condition = ifelse (Condition == "NO", "Ref. Condition", Condition),
    Condition = ifelse (Condition == "AS", "Aug. Success", Condition),
    Condition = ifelse (Condition == "IO", "Input Override", Condition),
    perc_c = t_color(PercNormalized, cri$lv_perc, cri$colors),
    percf_c = t_color(Perc.f, cri$lv_percf, cri$colors),
    frust_c = t_color(FrustNormalized, cri$lv_frust, cri$colors),
    frustf_c = t_color(Frust.f, cri$lv_frustf, cri$colors),
    rate_c = t_color(rate_blink, cri$lv_rate, cri$colors),
    feedback_c = t_color(rate_feedback, cri$lv_rate, cri$colors),
    rate_acc_c = t_color(blink_recog, cri$lv_rate, cri$colors),
    fish_c = t_color(fishCaught, cri$lv_fish, cri$colors),
    lost_c = t_color(fishLost, cri$lv_lost, cri$colors),
    reel_c = t_color(fishReel, cri$lv_reel, cri$colors),
    unreel_c = t_color(fishUnreel, cri$lv_unreel, cri$colors),
    muchhelpf_c = t_color(HowMuchHelp.f, cri$lv_percf, cri$colors),
    likedhelpf_c = t_color(LikedHelp.f, cri$lv_percf, cri$colors),
    pacingf_c = t_color(Pacing.f, cri$lv_percf, cri$colors),
    irritationf_c = t_color(Irritation.f, cri$lv_frustf, cri$colors),
    muchhelp_c = t_color(HowMuchHelpNormalized, cri$lv_perc, cri$colors),
    likedhelp_c = t_color(LikedHelpNormalized, cri$lv_perc, cri$colors),
    pacing_c = t_color(PacingNormalized, cri$lv_perc, cri$colors),
    irritation_c = t_color(IrritationNormalized, cri$lv_frust, cri$colors),
    hardest_c = t_color(Hardest, cri$lv_diff, cri$colors),
    easiest_c = t_color(Easiest, cri$lv_diff, cri$colors),
    time_c = t_color(time_total, cri$lv_time, cri$colors),
    help_c = t_color(rate_help, cri$lv_help, cri$colors),
    HowMuchHelpNormalized = format(round(HowMuchHelpNormalized,2), nsmall = 2),
    HowMuchHelp.f = format(round(HowMuchHelp.f,2), nsmall = 2),
    LikedHelpNormalized = format(round(LikedHelpNormalized,2), nsmall = 2),
    LikedHelp.f = format(round(LikedHelp.f,2), nsmall = 2),
    PacingNormalized = format(round(PacingNormalized,2), nsmall = 2),
    Pacing.f = format(round(Pacing.f,2), nsmall = 2),
    IrritationNormalized = format(round(IrritationNormalized,2), nsmall = 2),
    Irritation.f = format(round(Irritation.f,2), nsmall = 2),
    PercNormalized = format(round(PercNormalized,2), nsmall = 2),
    Perc.f = format(round(Perc.f,2), nsmall = 2),
    FrustNormalized = format(round(FrustNormalized,2), nsmall = 2),
    Frust.f = format(round(Frust.f,2), nsmall = 2),
    fishCaught = format(round(fishCaught,2), nsmall = 2),
    fishLost = format(round(fishLost,2), nsmall = 2),
    fishReel = format(round(fishReel,2), nsmall = 2),
    fishUnreel = format(round(fishUnreel,2), nsmall = 2),
    #blink_recog = format(round(blink_recog,2), nsmall = 2),
    time_total = format(round(time_total,0), nsmall = 0),
    HowMuchHelpNormalized_SD = format(round(HowMuchHelpNormalized_SD,2), nsmall = 2),
    HowMuchHelp.f_SD = format(round(HowMuchHelp.f_SD,2), nsmall = 2),
    LikedHelpNormalized_SD = format(round(LikedHelpNormalized_SD,2), nsmall = 2),
    LikedHelp.f_SD = format(round(LikedHelp.f_SD,2), nsmall = 2),
    PacingNormalized_SD = format(round(PacingNormalized_SD,2), nsmall = 2),
    Pacing.f_SD = format(round(Pacing.f_SD,2), nsmall = 2),
    IrritationNormalized_SD = format(round(IrritationNormalized_SD,2), nsmall = 2),
    Irritation.f_SD = format(round(Irritation.f_SD,2), nsmall = 2),
    PercNormalized_SD = format(round(PercNormalized_SD,2), nsmall = 2),
    Perc.f_SD = format(round(Perc.f_SD,2), nsmall = 2),
    Frust.f_SD = format(round(Frust.f_SD,2), nsmall = 2),
    FrustNormalized_SD = format(round(FrustNormalized_SD,2), nsmall = 2),
    rate_feedback_SD = format(round(rate_feedback_SD,2), nsmall = 2),
    rate_blink_SD = format(round(rate_blink_SD,2), nsmall = 2),
    rate_help_SD = format(round(rate_help_SD,2), nsmall = 2),
    blink_recog_SD = format(round(blink_recog_SD,2), nsmall = 2),
    fishCaught_SD = format(round(fishCaught_SD,2), nsmall = 2),
    fishLost_SD = format(round(fishLost_SD,2), nsmall = 2),
    fishReel_SD = format(round(fishReel_SD,2), nsmall = 2),
    fishUnreel_SD = format(round(fishUnreel_SD,2), nsmall = 2),
    time_total_SD = format(round(time_total_SD,0), nsmall = 0),
    rate_blink = paste0(format(round(rate_blink * 100,0), nsmall = 0),"\\%"),
    blink_recog = paste0(format(round(blink_recog * 100,0), nsmall = 0),"\\%"),
    rate_help = paste0(format(round(rate_help * 100,0), nsmall = 0),"\\%"),
    rate_help = paste0("\\cellcolor{", help_c, "}", rate_help, " (", rate_help_SD,")"),
    rate_feedback = paste0(format(round(rate_feedback * 100, 0), nsmall = 0), "\\%"),
    HowMuchHelpNormalized = paste0("\\cellcolor{", muchhelp_c, "}", HowMuchHelpNormalized, " (", HowMuchHelpNormalized_SD, ")"),
    HowMuchHelp.f = paste0("\\cellcolor{", muchhelpf_c, "}", HowMuchHelp.f, " (", HowMuchHelp.f_SD, ")"),
    LikedHelpNormalized = paste0("\\cellcolor{", likedhelp_c, "}", LikedHelpNormalized, " (", LikedHelpNormalized_SD, ")"),
    LikedHelp.f = paste0("\\cellcolor{", likedhelpf_c, "}", LikedHelp.f, " (", LikedHelp.f_SD, ")"),
    PacingNormalized = paste0("\\cellcolor{", pacing_c, "}", PacingNormalized, " (", PacingNormalized_SD, ")"),
    Pacing.f = paste0("\\cellcolor{", pacingf_c, "}", Pacing.f, " (", Pacing.f_SD, ")"),
    IrritationNormalized = paste0("\\cellcolor{", irritation_c, "}", IrritationNormalized, " (", IrritationNormalized_SD, ")"),
    Irritation.f = paste0("\\cellcolor{", irritationf_c, "}", Irritation.f, " (", Irritation.f_SD, ")"),
    PercNormalized = paste0("\\cellcolor{", perc_c, "}", PercNormalized, " (", PercNormalized_SD, ")"),
    FrustNormalized = paste0("\\cellcolor{", frust_c, "}", FrustNormalized, " (", FrustNormalized_SD, ")"),
    Perc.f = paste0("\\cellcolor{", percf_c, "}", Perc.f, " (", Perc.f_SD, ")"),
    Frust.f = paste0("\\cellcolor{", frustf_c, "}", Frust.f, " (", Frust.f_SD, ")"),
    rate_blink = paste0("\\cellcolor{", rate_c, "}", rate_blink, " (", rate_blink_SD,")"),
    rate_feedback = paste0("\\cellcolor{", feedback_c, "}", rate_feedback,  " (", rate_feedback_SD, ")"),
    fishCaught = paste0("\\cellcolor{", fish_c, "}", fishCaught, " (", fishCaught_SD, ")"),
    fishLost = paste0("\\cellcolor{", lost_c, "}", fishLost, " (", fishLost_SD, ")"),
    fishReel = paste0("\\cellcolor{", reel_c, "}", fishReel, " (", fishReel_SD, ")"),
    fishUnreel = paste0("\\cellcolor{", unreel_c, "}", fishUnreel, " (", fishUnreel_SD, ")"),
    blink_recog = paste0("\\cellcolor{", rate_acc_c, "}", blink_recog, " (", blink_recog_SD, ")"),
    Hardest = paste0("\\cellcolor{", hardest_c, "}", Hardest),
    Easiest = paste0("\\cellcolor{", easiest_c, "}", Easiest),
    time_total = paste0("\\cellcolor{", time_c, "}", time_total, "s (", time_total_SD, "s)"),
    perc_c = NULL, frust_c = NULL, rate_c = NULL, feedback_c = NULL, easiest_c = NULL, hardest_c = NULL, time_c = NULL,
    lost_c = NULL, fish_c = NULL, rate_acc_c = NULL, irritation_c = NULL, pacing_c = NULL, likedhelp_c = NULL, muchhelp_c = NULL,
    across(everything(), as.character)) %>% arrange(Condition) %>%
  select(`Perc. Control` = Perc.f, `Frustration` = Frust.f, `Help Quantity` = HowMuchHelp.f,
         `Help Appeal` = LikedHelp.f, `Pacing` = Pacing.f, `Irritation` = Irritation.f,
          Hardest, Easiest, `Blink Recognition` = blink_recog, `Blink Conv. Rate` = rate_blink,
         `Pos. Feedback` = rate_feedback, `Help Rate` = rate_help, `Fish Caught` = fishCaught, `Fish Lost` = fishLost,
         `Fish Reel` = fishReel, `Fish Unreel` = fishUnreel, Duration = time_total) %>%
  select(-c(ends_with("_SD"))) %>%
  pivot_longer(cols=-c(Condition), names_to = "Variables") %>%
  pivot_wider(names_from = Condition, values_from = value)

# Add ICC Scores
Sc_table <- Sc_table %>% left_join(Sicc) %>% 
  mutate(ICC3 = ifelse(is.na(ICC3),"-",format(round(ICC3,2), nsmall = 2)))


paste(colnames(Sc_table), collapse=" & ")
writeLines(paste(Sc_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")
  


St_icc <- St %>% filter(Condition %in% c("AS","IO","MF")) %>% ungroup %>%
  select(Participant, HowMuchHelpNormalized, Condition) %>%
  pivot_wider(names_from = Participant, values_from = HowMuchHelpNormalized) %>%
  ungroup() %>% select(-Condition)

test <- psych::ICC(St_icc)

#St %>% filter(Condition %in% c("AS","IO","MF"))


D_icc <- St %>% ungroup %>%
  select(Participant, FrustNormalized, GameTitle, Condition) %>%
  pivot_wider(names_from = Participant, values_from = FrustNormalized) %>%
  ungroup() %>% select(-Condition, -GameTitle)

test <- psych::ICC(D_icc)


#############
# Cumulative Link Mixed Models: All
#############

### H1/2: No variables significantly predicted/affected participants' perceived control and frustration.
clmms = list(predictors = c("Perc.f","Frust.f"),
             random = c("bci_experience.f","Fatigue.f","Gender.f"),
             fixed = c("rate_feedback","blink_conv_rate", "Condition.f", "Gender.f", "Order.f",
                       "pam_rate", "fishCaught","fishReel","fishUnreel", "fishLost","time_total"),
             null = c("Participant.f"),
             threshold = 0.10, # increased to 0.1 to show models which were close to significant.
             df = St)

table = g_clmm_table(clmms)

glme_table <- table %>% 
  mutate(p = `$\\chi^2$`,
         `Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         `$\\chi^2$` = ifelse(p < 0.05, paste0(`$\\chi^2$`,"*")),
         across(everything(), ~ str_replace_all(.x, c("Order.f" = "Condition Order",
                                                      "Frust.f" = "Frustration",
                                                      "Perc.f" = "Perc. Control",
                                                      "Participant.f" = "Participant",
                                                      "LikedHelp.f" = "Help Appeal")))
  ) %>%
  select(Predicted, `Fixed Effect`, AIC, ML, LR, `$\\chi^2$`)
message(paste(colnames(glme_table), collapse=" & "))
message(paste(glme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

### H3/4: Condition significantly predicted HowMuchHelp and LikedHelp

#St = St %>% filter(Condition %in% c("AS","IO","MF"))

clmms = list(predictors = c("HowMuchHelp.f", "LikedHelp.f"),
             random = c("bci_experience.f","Fatigue.f","Gender.f"),
             fixed = c("rate_feedback","blink_conv_rate", "Condition.f", "Order.f", "Pacing.f",
                       "pam_rate", "fishCaught","fishReel","fishUnreel", "fishLost","time_total"),
             null = c("Participant.f"),
             threshold = 0.05,
             df = St %>% filter(Condition %in% c("AS","IO","MF")))
table = g_clmm_table(clmms)

glme_table <- table %>% 
  mutate(p = `$\\chi^2$`,
         `Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         `$\\chi^2$` = ifelse(p < 0.05, paste0(`$\\chi^2$`,"*")),
         across(everything(), ~ str_replace_all(.x, c("Order.f" = "Condition Order",
                                                      "HowMuchHelp.f" = "Help Quantity",
                                                      "LikedHelp.f" = "Help Appeal",
                                                      "Pacing.f" = "Pacing",
                                                      "Condition.f" = "Condition",
                                                      "pam_rate" = "PAM Rate",
                                                      "blink_conv_rate" = "Blink Conv. Rate",
                                                      "fishCaught" = "Fish Caught",
                                                      "fishReel" = "Fish Reel",
                                                      "fishUnreel" = "Fish Unreel",
                                                      "fishLost" = "Fish Lost",
                                                      "Irritation.f" = "Irritation",
                                                      "Frust.f" = "Frustration",
                                                      "Perc.f" = "Perc. Control",
                                                      "Participant.f" = "Participant",
                                                      "LikedHelp.f" = "Help Appeal",
                                                      "rate_feedback" = "Pos. Feedback")))
  ) %>%
  select(Predicted, `Fixed Effect`, AIC, ML, LR, `$\\chi^2$`)
message(paste(colnames(glme_table), collapse=" & "))
message(paste(glme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

St$Condition.f = factor(St$Condition.f, levels=c("MF","IO","AS","NO"))
model.null = clm(HowMuchHelp.f ~ 1 + (1|Participant), data=St %>% filter(Condition %in% c("AS","IO","MF")))

model.condition = clmm(HowMuchHelp.f ~ 1 + (1|Participant) + Condition.f , data=St %>% filter(Condition %in% c("AS","IO","MF")))

anova(model.condition, model.null)

modelcond.summary = summary(model.condition)
modelcond.summary

St$Condition.f = factor(St$Condition.f, levels=c("IO","MF","AS","NO"))

model.null = clm(LikedHelp.f ~ 1 + (1|Participant), data=St %>% filter(Condition %in% c("AS","IO","MF")))

model.condition = clmm(LikedHelp.f ~ 1 + (1|Participant) + Condition.f, data=St %>% filter(Condition %in% c("AS","IO","MF")))

anova(model.condition, model.null)

modelcond.summary = summary(model.condition)
modelcond.summary

fixedtable = as.data.frame(modelfrust.summary$coefficients) %>% 
  rownames_to_column("Fixed Effect") %>%
  filter(`Fixed Effect` %in% c("pam_rate", "fishLost")) %>%
  mutate(Predicted = "Frustration") 

model.condition = clmm(Perc.f ~ 1 + (1|Participant) + rate_feedback + Condition, data=St %>% filter(Condition %in% c("AS","IO","MF")))
car::vif(model.condition)



#H5/6: Pacing/irritation

clmms = list(predictors = c("Pacing.f", "Irritation.f"),
             #random = c("bci_experience.f","Fatigue.f"),
             fixed = c("rate_feedback", "Condition.f", "Gender.f", "Order.f",
                       "pam_rate", "fishCaught","fishReel","fishUnreel", "fishLost"),
             null = c("Participant.f"),
             threshold = 0.3,
             df = St)
table = g_clmm_table(clmms)

glme_table <- table %>% 
  mutate(p = `$\\chi^2$`,
         `Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         `$\\chi^2$` = ifelse(p < 0.05, paste0(`$\\chi^2$`,"*"),`$\\chi^2$`),
         across(everything(), ~ str_replace_all(.x, c("Order.f" = "Condition Order",
                                                      "HowMuchHelp.f" = "Help Quantity",
                                                      "LikedHelp.f" = "Help Appeal",
                                                      "Pacing.f" = "Pacing",
                                                      "Irritation.f" = "Irritation",
                                                      "Frust.f" = "Frustration",
                                                      "Perc.f" = "Perc. Control",
                                                      "Participant.f" = "Participant")))
  ) %>%
  select(Predicted, `Fixed Effect`, AIC, ML, LR, `$\\chi^2$`)
message(paste(colnames(glme_table), collapse=" & "))
message(paste(glme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))






#############
# Linear Mixed Models
#############
# Test:
# Some patients report fatigue. Check whether Order has influence.
# Most patients report Number of Fish. Check FishLost / FishCaught
# 

# Testing variables:
# Perc.Control, Frustration, Preference, Dispreference

lmes = list(predictors = c("PercNormalized", "FrustNormalized", "Hardest","Easiest"),
            random = c("Fatigue", "bci_experience"),
            fixed = c("rate_blink","rate_feedback","Gender", "Condition", "HowMuchHelpNormalized",
                      "pam_rate", "fishCaught","fishLost","fishReel","fishUnreel"),
            null = c("Participant"),
            threshold = 0.05,
            df = St)

table = g_lme_table(lmes)

lme_table <- table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         across(everything(), ~ str_replace_all(.x, c("fishCaught" = "Fish Caught",
                                                      "fishLost" = "Fish Lost",
                                                      "fishUnreel" = "Fish Unreel",
                                                      "rate_feedback" = "Pos. Feedback",
                                                      "pam_rate" = "PAM Rate",
                                                      "fishReel" = "Fish Reel",
                                                      "rate_blink" = "Blink Conv. Rate",
                                                      "bci_experience" = "BCI Experience",
                                                      "PercNormalized" = "Perceived Control",
                                                      "FrustNormalized" = "Frustration")))
  ) %>%
  select(Predicted, `Random Intercept`, `Fixed Effect`, AIC, BIC, ML, `$\\chi^2$`, `$R^2_m$`,`$R^2_c$`)
message(paste(colnames(lme_table), collapse=" & "))
message(paste(lme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

#############
# Violin Plots of Main Measurements
#############
#fig_c <- fig %>%
#  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")), 
#            y=~n_clip(jitter(PercNormalized,amount=.02)),
#            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
#            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
#            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
#  add_trace(data=St %>% arrange(Participant,Condition),x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")),
#            y=~n_clip(jitter(PercNormalized,amount=.02)), type='scatter',mode='lines') %>%
#  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
#                                                   text="“I felt I was in control of the \n fisherman reeling in the fish.”"), showlegend=F,
#         xaxis=list(range=c(-0.45,3.55), title=" ",tickfont=list(size=15)),
#         yaxis=list(range=c(-0.02,1.02), title=" ", dtick=0.167, tickformat = ".2", tickfont=list(size=15), zeroline=F))
#fig_c
#orca(fig_c, "fig/condition_percNormalized_violin.pdf", width=275, height=325)

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")), 
            y=~n_clip(jitter(PercNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0),title=list(font=list(size=15),xanchor="center",xref="paper",
                                                  text="“I felt I was in control of the \n fisherman reeling in the fish.”"), showlegend=F,
         xaxis=list(range=c(-0.45,3.55), title=" ", zeroline=F, tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", zeroline=F, dtick=0.167, tickformat = ".2", tickfont=list(size=15), showticklabels=T))
fig_c
orca(fig_c, "fig/condition_percNormalized_violin.pdf", width=285, height=325)



fig %>% add_trace(data=St %>% arrange(Participant,Condition),x=~factor(ConditionLabel, levels=c("Aug.\n Success","Overr.\n Input","Mit.\n Failure","Ref.")),
                    y=~n_clip(jitter(PercNormalized,amount=.02)), name=~Participant, hovertext=~Participant, type='scatter',mode='markers+lines')

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")), 
            y=~n_clip(jitter(FrustNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0),title=list(font=list(size=15),xanchor="center",xref="paper",
                                                  text="“How much frustration did you \n feel in this condition?”"), showlegend=F,
         xaxis=list(range=c(-0.45,3.55), title=" ", zeroline=F, tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", zeroline=F, dtick=0.167, tickformat = ".2", tickfont=list(size=15), showticklabels=T))
fig_c
orca(fig_c, "fig/condition_frustNormalized_violin.pdf", width=285, height=325)

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input")),
            y=~n_clip(jitter(HowMuchHelpNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4,color=I('black')),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="“How much did you feel \n the game helped you?”"),showlegend=F,
         xaxis=list(range=c(-0.45,2.55), title=" ", tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", dtick=0.167, tickformat = ".2", tickfont=list(size=15), zeroline=F, showticklabels=F))
fig_c
orca(fig_c, "fig/condition_howMuchHelp_violin.pdf", width=275, height=325)

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input")),
            y=~n_clip(jitter(LikedHelpNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="\n“I liked how the game helped me.”"), showlegend=F,
         xaxis=list(range=c(-0.45,2.55), title=" ", tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", dtick=0.167, tickformat = ".2", tickfont=list(size=15), zeroline=F, showticklabels=FALSE))
fig_c
orca(fig_c, "fig/condition_LikedHelp_violin.pdf", width=275, height=325)

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")),
            y=~n_clip(jitter(PacingNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="“I felt the pacing of the game was”\n (slow/fast)"), showlegend=F,
         xaxis=list(range=c(-0.45,3.55), title=" ",tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", dtick=0.167, tickformat = ".2", tickfont=list(size=15), zeroline=F,showticklabels=F))
fig_c
orca(fig_c, "fig/condition_pacingNormalized_violin.pdf", width=275, height=325)

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")), y=~n_clip(jitter(IrritationNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.45, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="“How irritated did you feel\n in this condition?”"), showlegend=F,
         xaxis=list(range=c(-0.45,3.55), title=" ",tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", dtick=0.167, tickformat = ".2", tickfont=list(size=15), zeroline=F,showticklabels=FALSE))
fig_c
orca(fig_c, "fig/condition_irritationNormalized_violin.pdf", width=275, height=325)

#############
# Means and ratings by participant
#############

# Fish Reel
fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition == cond), x=~jitter(fishReel,0.45), y=~jitter(LikedHelpNormalized,0.15),
              type='scatter', mode='markers',symbol=I('o'), marker=list(size=8)) %>%
    layout(annotations=list(showarrow=F,x=1.45,y=1.03,text=paste0(cond)),
           xaxis=list(range=c(-0.1,30.1), title="Control"),
           yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
}) %>% subplot(., nrows=1) %>% layout(title="How much frustration did you feel in this condition?", showlegend=F, yaxis=list(title="Frustration Rating"), xaxis=list(title="Participant"))
fig_c
orca(fig_c, "fig/participant_frust.pdf", width=1150, height=350)


# Condition Order
fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition == cond), x=~jitter(Order,0.45), y=~jitter(LikedHelpNormalized,0.15),
              type='scatter', mode='markers',symbol=I('o'), marker=list(size=8)) %>%
    layout(annotations=list(showarrow=F,x=1.45,y=1.03,text=paste0(cond)),
           xaxis=list(range=c(-0.1,5.1), title="Control"),
           yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
}) %>% subplot(., nrows=1) %>% layout(title="How much frustration did you feel in this condition?", showlegend=F, yaxis=list(title="Frustration Rating"), xaxis=list(title="Participant"))
fig_c
orca(fig_c, "fig/participant_frust.pdf", width=1150, height=350)

#Frustration
fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition == cond), x=~jitter(FrustNormalized,0.05), y=~jitter(PercNormalized,0.05),
              type='scatter', mode='marker', marker=list(size=8)) %>%
    layout(annotations=list(showarrow=F,x=1.45,y=1.03,text=paste0(cond)),
           xaxis=list(range=c(-0.1,1.1), title="Control"),
           yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
}) %>% subplot(., nrows=1) %>% layout(title="How much frustration did you feel in this condition?", showlegend=F, yaxis=list(title="Frustration Rating"), xaxis=list(title="Participant"))
fig_c
orca(fig_c, "fig/participant_frust.pdf", width=1150, height=350)

#Irritated
fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~IrritationNormalized,
              type='scattergl', mode='marker', marker=list(size=12)) %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~IrritationNormalized,
              type='scattergl', mode='text') %>%
    add_trace(data=tibble(y=c(mean(St$IrritationNormalized[St$Condition==cond]),mean(St$IrritationNormalized[St$Condition==cond])), x=c(0,10)), x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=1.45,y=1.03,text=paste0(cond)),
           xaxis=list(range=c(-0.1,11), title="Control"),
           yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
}) %>% subplot(., nrows=1) %>% layout(title="How irritated did you feel in this condition?", showlegend=F, yaxis=list(title="Irritation Rating"), xaxis=list(title="Participant"))
fig_c
orca(fig_c, "fig/participant_irritation.pdf", width=1150, height=350)

#Perceived Control
fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~PercNormalized,
              type='scattergl', mode='marker', marker=list(size=12)) %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~PercNormalized,
              type='scattergl', mode='text') %>%
    add_trace(data=tibble(y=c(mean(St$PercNormalized[St$Condition==cond]),mean(St$PercNormalized[St$Condition==cond])), x=c(0,10)), x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=1.45,y=1.03,text=paste0(cond)),
           xaxis=list(range=c(-0.1,11), title="Control"),
           yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
}) %>% subplot(., nrows=1) %>% layout(title="I felt I was in control of the fisherman reeling in the fish.", showlegend=F, yaxis=list(title="Perceived Control"), xaxis=list(title="Participant"))
fig_c
orca(fig_c, "fig/participant_perceived_control.pdf", width=1150, height=350)

#Pacing
fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~PacingNormalized,
              type='scattergl', mode='marker', marker=list(size=12)) %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~PacingNormalized,
              type='scattergl', mode='text') %>%
    add_trace(data=tibble(y=c(mean(St$PacingNormalized[St$Condition==cond]),mean(St$PacingNormalized[St$Condition==cond])), x=c(0,10)), x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=1.45,y=1.03,text=paste0(cond)),
           xaxis=list(range=c(-0.1,11), title="Control"),
           yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, title="It felt like the game went fast.", yaxis=list(title="Pacing Rating"), xaxis=list(title="Participant"))
fig_c
orca(fig_c, "fig/participant_pacing.pdf", width=1150, height=350)

#HowMuchHelp
fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~HowMuchHelpNormalized,
              type='scattergl', mode='marker', marker=list(size=12)) %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~HowMuchHelpNormalized,
              type='scattergl', mode='text') %>%
    add_trace(data=tibble(y=c(mean(St$HowMuchHelpNormalized[St$Condition==cond]),mean(St$HowMuchHelpNormalized[St$Condition==cond])), x=c(0,10)), x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=1.45,y=1.03,text=paste0(cond)),
           xaxis=list(range=c(-0.1,11), title="Control"),
           yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, title="“How much did you feel the game helped you?”", yaxis=list(title="Help Quantity Rating"), xaxis=list(title="Participant"))
fig_c
orca(fig_c, "fig/participant_howmuchhelp.pdf", width=1150, height=350)

#HowMuchHelp
fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~LikedHelpNormalized,
              type='scattergl', mode='marker', marker=list(size=12)) %>%
    add_trace(data=St %>% filter(Condition == cond), x=~Participant, y=~LikedHelpNormalized,
              type='scattergl', mode='text') %>%
    add_trace(data=tibble(y=c(mean(St$LikedHelpNormalized[St$Condition==cond]),mean(St$LikedHelpNormalized[St$Condition==cond])), x=c(0,10)), x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=1.45,y=1.03,text=paste0(cond)),
           xaxis=list(range=c(-0.1,11), title="Control"),
           yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, title="“I liked how the game helped me.” ", yaxis=list(title="Help Appeal Rating"), xaxis=list(title="Participant"))
fig_c
orca(fig_c, "fig/participant_likedhelp.pdf", width=1150, height=350)


fig1 <- fig %>%
  add_trace(x=factor(St$Condition, levels=c("50C0F", "50C15F","50C30F","50C50F")), y=n_clip(jitter(St$PercNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.3,
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.08, color=I('darkgray')) %>%
  add_trace(data=Lines, x=~Condition, y=~perc_mean, type='scatter',mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=~perc_ci)) %>%
  #add_trace(data=PercPureCurve, x=~x, y=~y, type='scatter', line=list(dash='dot'), symbol=I('square-x-open'), mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  #add_trace(data=PercLines[["PercFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  layout(showlegend=F, yaxis = list(range=c(-0.02,1.1), title="Perceived Control", violinmode = 'overlay', violingap = 0, zeroline=F),
         xaxis=list(title="Fabrication Rate (%)", tickmode='array', tickvals=c(0,1,2,3), ticktext=c('+0%', '+15%', '+30%', '+50%')))
fig1
orca(fig1, "fig/study2-level-of-control-perceived.pdf", width=350, height=350)

#############
# Conditions themselves
#############

# Comparison of variables that we test in terms of variation between conditions.
fig_c <- fig %>%
  add_trace(name="fishCaught", data=St, x=~Condition, y=~jitter(fishCaught_n,amount=.035),
            hovertext=~fishCaught, symbol=I('o'),
            type='scatter', mode='marker', marker=list(size=6),color=I('rgba(180,180,180,1.0)')) %>%
  add_trace(name="fishCaught", data=tibble(y=c(mean(St$fishCaught_n[St$Condition=="AS"]),
                            mean(St$fishCaught_n[St$Condition=="IO"]),
                            mean(St$fishCaught_n[St$Condition=="MF"]),
                            mean(St$fishCaught_n[St$Condition=="NO"])), 
                        x=as.factor(c("AS","IO","MF","NO"))),
            x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(160,160,160,1.0)')) %>%
  add_trace(name="fishReel", data=St, x=~Condition, y=~jitter(fishReel_n,amount=.035),
            hovertext=~fishReel_a, symbol=I('o'),
            type='scatter', mode='marker', marker=list(size=6),color=I('rgba(195,195,195,1.0)')) %>%
  add_trace(name="fishReel", data=tibble(y=c(mean(St$fishReel_n[St$Condition=="AS"]),
                                               mean(St$fishReel_n[St$Condition=="IO"]),
                                               mean(St$fishReel_n[St$Condition=="MF"]),
                                               mean(St$fishReel_n[St$Condition=="NO"])), 
                                           x=as.factor(c("AS","IO","MF","NO"))),
            x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(195,195,195,1.0)')) %>%
  add_trace(name="fishUnreel", data=St, x=~Condition, y=~jitter(fishUnreel_n,amount=.035),
            hovertext=~fishUnreel, symbol=I('o'),
            type='scatter', mode='marker', marker=list(size=6),color=I('rgba(125,125,125,1.0)')) %>%
  add_trace(name="fishUnreel", data=tibble(y=c(mean(St$fishUnreel_n[St$Condition=="AS"]),
                                             mean(St$fishUnreel_n[St$Condition=="IO"]),
                                             mean(St$fishUnreel_n[St$Condition=="MF"]),
                                             mean(St$fishUnreel_n[St$Condition=="NO"])), 
                                         x=as.factor(c("AS","IO","MF","NO"))),
            x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(125,125,125,1.0)')) %>%
  add_trace(name="Duration", data=St, x=~Condition, y=~jitter(time_total_n,amount=.035),
            hovertext=~time_total, symbol=I('o'),
            type='scatter', mode='marker', marker=list(size=6),color=I('rgba(110,110,110,1.0)')) %>%
  add_trace(name="Duration", data=tibble(y=c(mean(St$time_total_n[St$Condition=="AS"]),
                                               mean(St$time_total_n[St$Condition=="IO"]),
                                               mean(St$time_total_n[St$Condition=="MF"]),
                                               mean(St$time_total_n[St$Condition=="NO"])), 
                                           x=as.factor(c("AS","IO","MF","NO"))),
            x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(110,110,110,1.0)')) %>%
  add_trace(name="text annotations",
            data=tibble(y=c(mean(St$fishCaught_n[St$Condition=="NO"]+0.025),
                            mean(St$fishReel_n[St$Condition=="NO"]),
                            mean(St$fishUnreel_n[St$Condition=="NO"]-0.21),
                            mean(St$time_total_n[St$Condition=="NO"])), 
                        x=c("NO","NO","NO","NO"),
                        text=c("<b>  Fish Caught  </b>","<b>  Fish Reel  </b>","<b>  Fish Unreel  </b>","<b>  Duration  </b>")),
            x=~x, y=~y, text=~text, textposition="top left", type='scatter', mode='text', color=I('black')) %>%
  layout(showlegend=F, title="Condition Variability",
         xaxis=list(title="Condition", range=c(-0.1,3.1), tickmode='array',
                    tickvals=c(0,1,2,3), ticktext=c('Aug.<br>Success', 'Input<br>Override', 'Mit.<br>Failure', 'Ref.')),
         yaxis=list(range=c(-0.1,1.1), title=" "))
fig_c
orca(fig_c, "fig/condition_variability.pdf", width=480, height=480)

# This is effectively a visualization of what the mechanisms 'imply' to the game.
# AS provides similar levels of unreels, less reels but higher number of caught fish.
# IO provides higher caught fish, by altering the number of successful reels/unreels.
# MF does not in itself boost the number of reels (? verify), but alters unreels.

#fishCaught
fig_c <- fig %>%
  add_trace(data=St, x=~Condition, y=~jitter(fishCaught,amount=.2),opacity=.45,
            type='scattergl', mode='marker', marker=list(size=12),showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,4), title="Condition"),
         yaxis=list(range=c(-0.1,11), title="No. of Fish Caught"))
fig_c
orca(fig_c, "fig/condition_no_fish_caught.pdf", width=480, height=480)

#fishReel
fig_c <- fig %>%
  add_trace(data=St, x=~Condition, y=~jitter(fishReel,amount=.2),opacity=.45,
            type='scattergl', mode='marker', marker=list(size=12),showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,4), title="Condition"),
         yaxis=list(range=c(-0.1,17), title="No. of Fish Reel"))
fig_c
orca(fig_c, "fig/condition_no_fish_reel.pdf", width=480, height=480)

#fishUnreel
fig_c <- fig %>%
  add_trace(data=St, x=~Condition, y=~jitter(fishUnreel,amount=.2),opacity=.45,
            type='scattergl', mode='marker', marker=list(size=12),showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,4), title="Condition"),
         yaxis=list(range=c(-0.1,17), title="No. of Fish Unreel"))
fig_c
orca(fig_c, "fig/condition_no_fish_unreel.pdf", width=480, height=480)

#fishLost
fig_c <- fig %>%
  add_trace(data=St, x=~Condition, y=~jitter(fishLost,amount=.2),opacity=.45,
            type='scattergl', mode='marker', marker=list(size=12),showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,4), title="Condition"),
         yaxis=list(range=c(-0.1,17), title="No. of Fish Lost"))
fig_c
orca(fig_c, "fig/condition_no_fish_lost.pdf", width=480, height=480)

#Positive Trials
fig_c <- fig %>%
  add_trace(data=St, x=~Condition, y=~jitter(trial_rate_positive,amount=.02),opacity=.45,
            type='scattergl', mode='marker', marker=list(size=12),showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,4), title="Condition"),
         yaxis=list(range=c(-0.1,1.1), title="% Positive Trials"))
fig_c
orca(fig_c, "fig/condition_positive_trials.pdf", width=480, height=480)

#Accepted Trials
fig_c <- fig %>%
  add_trace(data=St, x=~Condition, y=~jitter(trial_rate_accept,amount=.02),opacity=.45,
            type='scattergl', mode='marker', marker=list(size=12),showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,4), title="Condition"),
         yaxis=list(range=c(-0.1,1.1), title="% Accepted Trials"))
fig_c
orca(fig_c, "fig/condition_accept.pdf", width=480, height=480)

#Accepted Blinks
fig_c <- fig %>%
  add_trace(data=St, x=~Condition, y=~jitter(rate_blink,amount=.02),opacity=.45,
            type='scattergl', mode='marker', marker=list(size=12),showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,4), title="Condition"),
         yaxis=list(range=c(-0.1,1.1), title="% Accepted Blinks"))
fig_c
orca(fig_c, "fig/condition_accept_blinks.pdf", width=480, height=480)

#Hardest Condition
fig_c <- fig %>%
  add_trace(data=St %>% group_by(Condition) %>% summarize(Hardest = sum(Hardest)), x=~Condition, y=~Hardest,opacity=.45,
            type='bar', text=~Hardest, textposition = 'auto', marker=list(size=12),showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,4), title="Condition"),
         yaxis=list(range=c(-0.1,20), title="Hardest Condition"))
fig_c
orca(fig_c, "fig/condition_hardest.pdf", width=480, height=480)


#Easiest Condition
fig_c <- fig %>%
  add_trace(data=St %>% group_by(Condition) %>% summarize(Easiest = sum(Easiest)), x=~Condition, y=~Easiest,opacity=.45,
            type='bar', text=~Easiest, textposition = 'auto', marker=list(size=12),showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,4), title="Condition"),
         yaxis=list(range=c(-0.1,20), title="Hardest Condition"))
fig_c
orca(fig_c, "fig/condition_easiest.pdf", width=480, height=480)

#############
#  Older material.
#############
PercLine <- list("NO" = p_lin(St %>% filter(Condition == "NO"),"PercNormalized", "trial_rate_positive"),
                 "AS" = p_lin(St %>% filter(Condition == "AS"),"PercNormalized", "trial_rate_positive"),
                 "MF" = p_lin(St %>% filter(Condition == "MF"),"PercNormalized", "trial_rate_positive"),
                 "IO" = p_lin(St %>% filter(Condition == "IO"),"PercNormalized", "trial_rate_positive"))

FrustLine <- list("NO" = p_lin(St %>% filter(Condition == "NO"),"FrustNormalized", "trial_rate_positive"),
                  "AS" = p_lin(St %>% filter(Condition == "AS"), "FrustNormalized", "trial_rate_positive"),
                  "MF" = p_lin(St %>% filter(Condition == "MF"),"FrustNormalized", "trial_rate_positive"),
                  "IO" = p_lin(St %>% filter(Condition == "IO"),"FrustNormalized", "trial_rate_positive"))

CombLine <- list("NO" = p_lin(St %>% filter(Condition == "NO"),"FrustNormalized", "PercNormalized"),
                 "AS" = p_lin(St %>% filter(Condition == "AS"), "FrustNormalized", "PercNormalized"),
                 "MF" = p_lin(St %>% filter(Condition == "MF"),"FrustNormalized", "PercNormalized"),
                 "IO" = p_lin(St %>% filter(Condition == "IO"),"FrustNormalized", "PercNormalized"))

# Perceived Control to Condition

fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(PercNormalized,amount=.02), color=I('rgba(0.9,0.9,0.9,0.05)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(PercNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~mean(trial_rate_positive), y=~mean(PercNormalized), color=I('red'), 
              type='scatter', mode='markers') %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Positive Feedback', range=c(-0.1,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Perceived Control', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Perceived Control"), xaxis=list(title="Positive Feedback"))
fig_c
orca(fig_c, "fig/patients_perc_control_pos_feedback.pdf", width=1150, height=350)

fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~Participant, y=~jitter(FrustNormalized,amount=.02), color=I('rgba(0.9,0.9,0.9,0.05)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~Participant, y=~jitter(FrustNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Positive Feedback', range=c(-0.1,21.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Perceived Control', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Perceived Control"), xaxis=list(title="Positive Feedback"))
fig_c
orca(fig_c, "fig/patients_perc_control_pos_feedback.pdf", width=1150, height=350)



fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(FrustNormalized,amount=.02), color=I('rgba(0.9,0.9,0.9,0.05)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(FrustNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~mean(trial_rate_positive), y=~mean(FrustNormalized), color=I('red'), 
              type='scatter', mode='markers') %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Positive Feedback', range=c(-0.1,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Frustration', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Perceived Control"), xaxis=list(title="Positive Feedback"))
fig_c
orca(fig_c, "fig/patients_frustration_pos_feedback.pdf", width=1150, height=350)


fig_c <- lapply(unique(St$Participant), function(pid) {
  fig %>%
    add_trace(data=St %>% filter(Participant != pid), name=pid,
              marker=list(size=7), x=~Condition, y=~jitter(LikedHelpNormalized,amount=.02), color=I('rgba(0.9,0.9,0.9,0.05)'), 
              type='scatter', mode='bars', showlegend=F) %>%
    add_trace(data=St %>% filter(Participant == pid), name=pid,text=~Condition, symbol=I('o'),
              marker=list(size=16), x=~Condition, y=~jitter(LikedHelpNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='bars') %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(pid)),
           xaxis=list(zeroline=F,showgrid=F,title='Positive Feedback', range=c(-0.1,5.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Frustration', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=3) %>% layout(showlegend=F, yaxis=list(title="Perceived Control"), xaxis=list(title=" "))
fig_c

f <- St %>% group_by(Condition) %>% arrange(PercNormalized) %>%
  do(p = fig %>%
       add_trace(name=~Condition, ., marker=list(size=7), x=~Participant,
                 y=~jitter(PercNormalized,amount=.02), color=I('rgba(0.9,0.9,0.9,0.05)'), 
                 type='scatter', mode='bars', showlegend=F) %>%
       layout(xaxis=list(zeroline=F,showgrid=F,title='Participant', range=c(-0.1,5.1)),
              yaxis=list(zeroline=F,showgrid=F,title='PercNormalized', range=c(-0.1,1.1)))) %>%
  subplot(nrows = 8, margin=0.01, shareX=T, shareY=F) %>%
  layout(showlegend=F, xaxis = list(range=c(-1.1,1.1)))

f
orca(f, "figures/baking-tray-histograms.pdf", width=750, height=1300)


fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~jitter(PercNormalized,amount=.02), y=~jitter(FrustNormalized,amount=.02), color=I('rgba(0.8,0.8,0.8,0.15)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=CombLine[["NO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=CombLine[["AS"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=CombLine[["MF"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=CombLine[["IO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~jitter(PercNormalized,amount=.02), y=~jitter(FrustNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    add_trace(data=CombLine[[cond]], x=~x, y=~y, color=I('black'), type='scatter', mode='lines', showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Perceived Control', range=c(-0.1,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Frustration', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Frustration"), xaxis=list(title="Perceived Control"))
fig_c
orca(fig_c, "fig/patients_frust_perc_control.pdf", width=1150, height=350)

figf <- fig %>%
  add_trace(name= "NO", data = St %>% filter(Condition == "NO"), x=~trial_rate_positive, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("black"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name= "AS", data = St %>% filter(Condition == "AS"), x=~trial_rate_positive, text=St$Condition, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("blue"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name= "MF", data = St %>% filter(Condition == "MF"), x=~trial_rate_positive, text=St$Condition, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("red"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name= "IO", data = St %>% filter(Condition == "IO"), x=~trial_rate_positive, text=St$Condition,  y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("orange"), symbol=I('circle'), marker=list(size=8)) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Perceived Control"), xaxis = list(range=c(-0.05,1.1), title="MI Control"))
figf


fig_p <- fig %>%
  add_trace(data=St, x=~Participant, y=~PercNormalized, color=~Condition,
            type='scattergl', mode='marker', marker=list(size=20)) %>%
  add_trace(x=~Participant, y=~PercNormalized, text=~Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,15), title="Control"),
         yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))

fig_p

fig_f <- fig %>%
  add_trace(x=~trial_rate_accept, y=~FrustNormalized, color=~Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=~trial_rate_accept, y=~FrustNormalized, text=~Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Control"),
         yaxis=list(range=c(-0.1,1.1), title="Frustration"))


fig_f


vis <- subplot(fig_f, fig_p) %>% layout(showlegend=FALSE)
vis


#############
# Visualize Lost Fish / Caught Fish
#############

fig_p <- fig %>%
  add_trace(x=jitter(St$fishCaught,amount=.05), y=jitter(St$PercNormalized,amount=.02), color=St$Condition, opacity=.6,
            type='scattergl', mode='markers+text', marker=list(size=20), hoverinfo="text", text=paste(St$Participant, St$Condition)) %>%
  layout(xaxis=list(range=c(-0.1,11)),
         yaxis=list(range=c(-0.1,1.1)))
fig_p

fig_p <- fig %>%
  add_trace(x=St$fishCaught, y=St$PercNormalized, color=St$Condition,
            type='scattergl', mode='markers', marker=list(size=20), hoverinfo="text", text=paste(St$Participant, St$Condition)) %>%
  add_trace(x=St$fishCaught, y=St$PercNormalized, text=St$Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,11), title="Fish Caught"),
         yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
fig_p

#############
# Hypothesis Confirmation
#############

#1: Perc. Control and Blink Conversion Rate
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(rate_blink,amount=.02), 
            y=~jitter(PercNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Blink Rate"),
         yaxis=list(range=c(-0.1,1.1), title="Perc. Control"))
fig_p
orca(fig_p, "fig/scatter_perc_blinkrate.pdf", width=450, height=300)

#1: Perc. Control and Help Rate
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(rate_help,amount=.02), 
            y=~jitter(PercNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Help Rate"),
         yaxis=list(range=c(-0.1,1.1), title="Perc. Control"))
fig_p
orca(fig_p, "fig/scatter_perc_helprate.pdf", width=450, height=300)

#1: Perc. Control and Fish Caught
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(fishCaught,amount=.02), 
            y=~jitter(PercNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Caught"),
         yaxis=list(range=c(-0.1,1.1), title="Perc. Control"))
fig_p
orca(fig_p, "fig/scatter_perc_fishCaught.pdf", width=450, height=300)

#1: Perc. Control and Fish Reel
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(fishReel,amount=.02), 
            y=~jitter(PercNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Reel"),
         yaxis=list(range=c(-0.1,1.1), title="Perc. Control"))
fig_p
orca(fig_p, "fig/scatter_perc_fishReel.pdf", width=450, height=300)

#1: Perc. Control and Fish Lost
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(fishLost,amount=.02), 
            y=~jitter(PercNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Lost"),
         yaxis=list(range=c(-0.1,1.1), title="Perc. Control"))
fig_p
orca(fig_p, "fig/scatter_perc_fishLost.pdf", width=450, height=300)

#1: Perc. Control and Fish Unreel
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(fishUnreel,amount=.02), 
            y=~jitter(PercNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Unreel"),
         yaxis=list(range=c(-0.1,1.1), title="Perc. Control"))
fig_p
orca(fig_p, "fig/scatter_perc_fishUnreel.pdf", width=450, height=300)

#2: Frustration and Fish Lost
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(fishLost,amount=.02), 
            y=~jitter(FrustNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Lost"),
         yaxis=list(range=c(-0.1,1.1), title="Frustration"))
fig_p
orca(fig_p, "fig/scatter_frust_fishLost.pdf", width=450, height=300)

#1: Frustration and Fish Unreel
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(fishUnreel,amount=.02), 
            y=~jitter(PercNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Unreel"),
         yaxis=list(range=c(-0.1,1.1), title="Frustration"))
fig_p
orca(fig_p, "fig/scatter_frust_fishUnreel.pdf", width=450, height=300)

#2: Frustration and Blink Conv Rate
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(rate_blink,amount=.02), 
            y=~jitter(FrustNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Blink rate"),
         yaxis=list(range=c(-0.1,1.1), title="Frustration"))
fig_p
orca(fig_p, "fig/scatter_frust_fishLost.pdf", width=450, height=300)

#2: Frustration and Help Rate
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(rate_help,amount=.02), 
            y=~jitter(FrustNormalized,amount=.2), color=~Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Help rate"),
         yaxis=list(range=c(-0.1,1.1), title="Frustration"))
fig_p
orca(fig_p, "fig/scatter_frust_rate_help.pdf", width=450, height=300)

#3: Help Quantity and Help Rate
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se) %>% filter(Condition != "NO"),
            x=~jitter(rate_help,amount=.02), 
            y=~jitter(HowMuchHelpNormalized,amount=.2), color=~Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Help rate"),
         yaxis=list(range=c(-0.1,1.1), title="Help Quantity"))
fig_p
orca(fig_p, "fig/scatter_howmuchhelp_rate_help.pdf", width=450, height=300)

#3: Help Quantity and Fish Lost
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se)  %>% filter(Condition != "NO"),
            x=~jitter(fishLost,amount=.02), 
            y=~jitter(HowMuchHelpNormalized,amount=.2), color=~Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish lost (n)"),
         yaxis=list(range=c(-0.1,1.1), title="Help Quantity"))
fig_p
orca(fig_p, "fig/scatter_howmuchhelp_fish_lost.pdf", width=450, height=300)

#1: Help Quantity and Fish Unreel
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se)  %>% filter(Condition != "NO"),
            x=~jitter(fishUnreel,amount=.02), 
            y=~jitter(HowMuchHelpNormalized,amount=.2), color=~Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Unreel"),
         yaxis=list(range=c(-0.1,1.1), title="Help Quantity"))
fig_p
orca(fig_p, "fig/scatter_howmuchhelp_fishUnreel.pdf", width=450, height=300)

#4: Help Appeal and fish Lost
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se)  %>% filter(Condition != "NO"),
            x=~jitter(fishLost,amount=.02), 
            y=~jitter(LikedHelpNormalized,amount=.2), color=~Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish lost (n)"),
         yaxis=list(range=c(-0.1,1.1), title="Help Appeal"))
fig_p
orca(fig_p, "fig/scatter_likedhelp_fish_lost.pdf", width=450, height=300)

#1: Help Quantity and Fish Unreel
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se) %>% filter(Condition != "NO"),
            x=~jitter(fishUnreel,amount=.02), 
            y=~jitter(LikedHelpNormalized,amount=.2), color=~Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Unreel"),
         yaxis=list(range=c(-0.1,1.1), title="Help Appeal"))
fig_p
orca(fig_p, "fig/scatter_likedhelp_fishUnreel.pdf", width=450, height=300)

#4: Help Appeal and fish caught
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se) %>% filter(Condition != "NO"),
            x=~jitter(fishCaught,amount=.02), 
            y=~jitter(LikedHelpNormalized,amount=.2), color=~Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish caught (n)"),
         yaxis=list(range=c(-0.1,1.1), title="Help Appeal"))
fig_p
orca(fig_p, "fig/scatter_likedhelp_fish_caught.pdf", width=450, height=300)

#4: Help Appeal and fish reel
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se) %>% filter(Condition != "NO"),
            x=~jitter(fishReel,amount=.02), 
            y=~jitter(LikedHelpNormalized,amount=.2), color=~Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish reel (n)"),
         yaxis=list(range=c(-0.1,1.1), title="Help Appeal"))
fig_p
orca(fig_p, "fig/scatter_likedhelp_fish_reel.pdf", width=450, height=300)

#5: Pacing and Duration
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(time_total,amount=.02), 
            y=~jitter(PacingNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,200), title="Duration (s)"),
         yaxis=list(range=c(-0.1,1.1), title="Pacing"))
fig_p
orca(fig_p, "fig/scatter_duration_pacing.pdf", width=450, height=300)

#6: Irritation and Fish Lost
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(fishLost,amount=.02), 
            y=~jitter(IrritationNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Lost (n)"),
         yaxis=list(range=c(-0.1,1.1), title="Irritation"))
fig_p
orca(fig_p, "fig/scatter_fishLost_Irritation.pdf", width=450, height=300)

#1: Irritation and Fish Unreel
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(fishUnreel,amount=.02), 
            y=~jitter(IrritationNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,15), title="Fish Unreel"),
         yaxis=list(range=c(-0.1,1.1), title="Irritation"))
fig_p
orca(fig_p, "fig/scatter_irritation_fishUnreel.pdf", width=450, height=300)

#6: Irritation and Blink Conv Rate
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(rate_blink,amount=.02), 
            y=~jitter(IrritationNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Blink Rate"), 
         yaxis=list(range=c(-0.1,1.1), title="Irritation"))
fig_p
orca(fig_p, "fig/scatter_rate_blink_irritation.pdf", width=450, height=300)

#6: Irritation and help rate
fig_p <- fig %>%
  add_trace(data=St %>% left_join(Se),
            x=~jitter(rate_help,amount=.02), 
            y=~jitter(IrritationNormalized,amount=.2), color=St$Condition,
            opacity=.6,type='scatter', mode='markers+text', marker=list(size=7)) %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Help Rate"),
         yaxis=list(range=c(-0.1,1.1), title="Irritation"))
fig_p
orca(fig_p, "fig/scatter_rate_help_irritation.pdf", width=450, height=300)
