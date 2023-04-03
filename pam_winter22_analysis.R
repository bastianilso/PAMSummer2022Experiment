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





#############
# Summaries
#############

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
            posTrial = sum(TrialResult == "OverrideInput" | TrialResult == "AccInput" | TrialResult == "AugSuccess" | TrialResult == "ExplicitSham" | TrialResult == "AssistSuccess", na.rm=T),
            triggerTrial = sum(TrialResult == "AccInput" | TrialResult == "AugSuccess" | TrialResult == "AssistSuccess", na.rm=T),
            assistInput = sum(TrialResult %in% c("AssistSuccess", "AugSuccess"), na.rm=T),
            explicitSham = sum(TrialResult %in% c("ExplicitSham", "OverrideInput"), na.rm=T),
            mitigateFail = sum(TrialResult %in% c("AssistFail","MitigateFail"), na.rm=T),
            totalTrials2 = sum(!is.na(TrialResult), na.rm=T),
            totalTrials = rejInput+accInput+assistInput+explicitSham+mitigateFail,
            #fishCaught = sum(FishEvent == "FishCaught", na.rm=T),
            #fishCaught2 = sum(Event == "GameDecision" & fishFeedback == 1, na.rm=T),
            fishCaught = sum(TrialFeedback == "FishCaught", na.rm=T),
            fishReel = sum(TrialFeedback == "Reel", na.rm=T),
            fishStay = sum(TrialFeedback == "Stay", na.rm=T),
            fishUnreel = sum(TrialFeedback == "Unreel", na.rm=T),
            fishLost = sum(TrialFeedback == "FishLost", na.rm=T),
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
            Participant.f = unique(Participant.f)
            )



# Blink counts
St <- D %>% ungroup() %>% group_by(Participant, Condition) %>%
  summarize(blinks_total = sum(Event == "EyeOpening" & Period == "OpenPeriod")
  ) %>% right_join(St)

# Group by input window. Count the number of attempts in each window.
St <- D %>% ungroup() %>% filter(Period %in% c("OpenPeriod")) %>% group_by(Participant, Condition, InputWindowOrderFilledSoft) %>%
  summarize(blink_recog_window = ifelse(sum(Event %in% c("EyeOpening","EyeClosing") > 0), 1,0), #Whether Blinks happened in the window
            blink_recog_window_count = sum(Event %in% c("EyeOpening","EyeClosing")), #How much Blink happened in the window
            time_window = sum(time_delta)) %>%
  filter(InputWindowOrderFilledSoft > -1) %>% ungroup() %>% group_by(Participant, Condition) %>%
  summarize(blink_recog_trial = sum(blink_recog_window > 0),
            blink_recog_window = sum(blink_recog_window),
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

fig %>% add_trace(x=~InputWindowOrderFilled, y=~feedback_delay, data=Si, type='scatter')

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
    FrustNormalized_SD = sd(FrustNormalized),
    rate_feedback_SD = sd(rate_feedback),
    rate_blink_SD = sd(rate_blink),
    blink_recog_SD = sd(accRecogRate),
    fishCaught_SD = sd(fishCaught),
    fishLost_SD = sd(fishLost),
    time_total_SD = sd(time_total),
    HowMuchHelpNormalized_SD = sd(HowMuchHelpNormalized),
    LikedHelpNormalized_SD = sd(LikedHelpNormalized),
    PacingNormalized_SD = sd(PacingNormalized),
    IrritationNormalized_SD = sd(IrritationNormalized),
    PercNormalized = mean(PercNormalized),
    FrustNormalized = mean(FrustNormalized),
    rate_feedback = mean(rate_feedback),
    rate_blink = mean(rate_blink),
    blink_recog = mean(accRecogRate),
    fishCaught = mean(fishCaught),
    fishLost = mean(fishLost),
    HowMuchHelpNormalized = mean(HowMuchHelpNormalized),
    LikedHelpNormalized = mean(LikedHelpNormalized),
    PacingNormalized = mean(PacingNormalized),
    IrritationNormalized = mean(IrritationNormalized),
    Hardest = sum(Hardest),
    Easiest = sum(Easiest),
    time_total = mean(time_total)
  )



#############
# Correlations
#############

GGally::ggcorr(St)

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
Sq <- St %>% select(Participant,
                    `"I liked it when she took the fish up \n a notch at times, when I couldn’t"` = IOPositiveQuote,
                    `"It irritated me that she interefered \n with the game."` = IONegativeQuote,
                    `"I think it was useful that he got \n strong & helped me reel in the fish."` = ASPositiveQuote,
                    `"He got stronger, but I didn’t think \n it helped me much."` = ASNegativeQuote,
                    `"When the fish stood still, it was \n like saying “Let’s just try that again!"` = MFPositiveQuote,
                    `"When the fish stood still, it felt \n like the game went slower."` = MFNegativeQuote)

Sq <- St %>% select(Participant,
                    IOPositiveQuote,
                    IONegativeQuote,
                    ASPositiveQuote,
                    ASNegativeQuote,
                    MFPositiveQuote,
                    MFNegativeQuote)

Sq <- Sq %>% pivot_longer(cols=-c("Participant"), names_to = "Variable") %>% drop_na() %>%
  mutate(y = 1)


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


fig_p <- fig %>%
  add_trace(data=Sq %>% filter(Variable == "ASPositiveQuote"), text =~paste0("<b>",Participant,"</b>"), textfont=list(size=14), textangle=0,textposition="inside",
            x=~value, y=~y, type='bar', name=~Participant, marker=list(color=I("lightgrey"), line=list(width=2, color=I('white'))),
                marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=1, r=1, t=1, b=1), barmode='stack',
         xaxis=list(range=c(0.50,7.50), ticks='none', visible=F, dtick=1, title="", showline=F, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(-0.05,8.05), title="", tickvals=NULL, ticks='none', showline=F, shownumbers=F, visible=F, showgrid=F, mirror=T))

orca(fig_p, "fig/hist_ASPositive.pdf", width=160, height=150)

fig_p <- fig %>%
  add_trace(data=Sq %>% filter(Variable == "ASNegativeQuote"), text =~paste0("<b>",Participant,"</b>"), textfont=list(size=14), textangle=0,textposition="inside",
            x=~value, y=~y, type='bar', name=~Participant, marker=list(color=I("lightgrey"), line=list(width=2, color=I('white'))),
            marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=1, r=1, t=1, b=1), barmode='stack',
         xaxis=list(range=c(0.50,7.50), ticks='none', visible=F, dtick=1, title="", showline=F, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(-0.05,8.05), title="", tickvals=NULL, ticks='none', showline=F, shownumbers=F, visible=F, showgrid=F, mirror=T))

orca(fig_p, "fig/hist_ASNegative.pdf", width=160, height=150)

fig_p <- fig %>%
  add_trace(data=Sq %>% filter(Variable == "IONegativeQuote"), text =~paste0("<b>",Participant,"</b>"), textfont=list(size=14), textangle=0,textposition="inside",
            x=~value, y=~y, type='bar', name=~Participant, marker=list(color=I("lightgrey"), line=list(width=2, color=I('white'))),
            marker = list(line = list(color = "black", width = 1.5))) %>%
  layout(showlegend=F, margin=list(l=1, r=1, t=1, b=1), barmode='stack',
         xaxis=list(range=c(0.50,7.50), ticks='none', visible=F, dtick=1, title="", showline=F, tickvals=NULL, showgrid=F, mirror=T),
         yaxis=list(range=c(-0.05,8.05), title="", tickvals=NULL, ticks='none', showline=F, shownumbers=F, visible=F, showgrid=F, mirror=T))

orca(fig_p, "fig/hist_IONegative.pdf", width=160, height=150)

fig_p <- fig %>%
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
             lv_frust = rev(lv_perc),
             lv_rate = c(-0.1, 0.35, 0.55, 0.85, 1.1, 1.5, 2.2),
             colors = c("g0","g1", "g2", "g3", "g4","g4","g4"),
             lv_fish = c(0,2,4,6,10,12,14),
             lv_diff = rev(c(0,2,4,6,10,12,14)),
             lv_lost = c(8,4,2,1,0,-1,-2),
             lv_time = c(115,130,145,160,175,190,205),
             lv_icc = c(-0.1,0.5,0.73,0.75,0.77,0.90,1.00))

St_table <- St %>% group_by(Condition) %>% select(Participant, PercNormalized, FrustNormalized, rate_blink, accRecogRate, rate_feedback,
                                                  fishCaught, fishLost, HowMuchHelpNormalized, LikedHelpNormalized,
                                                  PacingNormalized, IrritationNormalized, time_total) %>%
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
    time_c = t_color(time_total, cri$lv_time, cri$colors),
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
    time_total = paste0("\\cellcolor{", time_c, "}", time_total),
    perc_c = NULL, frust_c = NULL, rate_c = NULL, rate_acc_c = NULL, feedback_c = NULL, time_c = NULL,
    lost_c = NULL, fish_c = NULL, irritation_c = NULL, pacing_c = NULL, likedhelp_c = NULL, muchhelp_c = NULL,
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
# ICC Scores
#############

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
    pivot_wider(names_from = Participant, values_from = HowMuchHelpNormalized) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  LikedHelpNormalized = St %>% ungroup() %>% select(Participant, LikedHelpNormalized, Condition) %>%
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
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]]
) %>% 
  select(`Perc. Control` = PercNormalized, `Frustration` = FrustNormalized, `How Much Help` = HowMuchHelpNormalized,
         `Liked Help` = LikedHelpNormalized, `Perc. Pacing` = PacingNormalized, `Irritation` = IrritationNormalized) %>%
  pivot_longer(cols=everything(), names_to = "Variable", values_to="ICC3")

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
    frust_c = t_color(FrustNormalized, cri$lv_frust, cri$colors),
    rate_c = t_color(rate_blink, cri$lv_rate, cri$colors),
    feedback_c = t_color(rate_feedback, cri$lv_rate, cri$colors),
    rate_acc_c = t_color(blink_recog, cri$lv_rate, cri$colors),
    fish_c = t_color(fishCaught, cri$lv_fish, cri$colors),
    lost_c = t_color(fishLost, cri$lv_lost, cri$colors),
    muchhelp_c = t_color(HowMuchHelpNormalized, cri$lv_perc, cri$colors),
    likedhelp_c = t_color(LikedHelpNormalized, cri$lv_perc, cri$colors),
    pacing_c = t_color(PacingNormalized, cri$lv_perc, cri$colors),
    irritation_c = t_color(IrritationNormalized, cri$lv_perc, cri$colors),
    hardest_c = t_color(Hardest, cri$lv_diff, cri$colors),
    easiest_c = t_color(Easiest, cri$lv_diff, cri$colors),
    time_c = t_color(time_total, cri$lv_time, cri$colors),
    HowMuchHelpNormalized = format(round(HowMuchHelpNormalized,2), nsmall = 2),
    LikedHelpNormalized = format(round(LikedHelpNormalized,2), nsmall = 2),
    PacingNormalized = format(round(PacingNormalized,2), nsmall = 2),
    IrritationNormalized = format(round(IrritationNormalized,2), nsmall = 2),
    PercNormalized = format(round(PercNormalized,2), nsmall = 2),
    FrustNormalized = format(round(FrustNormalized,2), nsmall = 2),
    blink_recog = format(round(blink_recog,2), nsmall = 2),
    time_total = format(round(time_total,2), nsmall = 2),
    HowMuchHelpNormalized_SD = format(round(HowMuchHelpNormalized_SD,2), nsmall = 2),
    LikedHelpNormalized_SD = format(round(LikedHelpNormalized_SD,2), nsmall = 2),
    PacingNormalized_SD = format(round(PacingNormalized_SD,2), nsmall = 2),
    IrritationNormalized_SD = format(round(IrritationNormalized_SD,2), nsmall = 2),
    PercNormalized_SD = format(round(PercNormalized_SD,2), nsmall = 2),
    FrustNormalized_SD = format(round(FrustNormalized_SD,2), nsmall = 2),
    rate_feedback_SD = format(round(rate_feedback_SD,2), nsmall = 2),
    rate_blink_SD = format(round(rate_blink_SD,2), nsmall = 2),
    blink_recog_SD = format(round(blink_recog_SD,2), nsmall = 2),
    fishCaught_SD = format(round(fishCaught_SD,2), nsmall = 2),
    fishLost_SD = format(round(fishLost_SD,2), nsmall = 2),
    time_total_SD = format(round(time_total_SD,2), nsmall = 2),
    rate_blink = paste0(format(round(rate_blink * 100,0), nsmall = 0),"\\%"),
    rate_feedback = paste0(format(round(rate_feedback * 100, 0), nsmall = 0), "\\%"),
    HowMuchHelpNormalized = paste0("\\cellcolor{", muchhelp_c, "}", HowMuchHelpNormalized, " (", HowMuchHelpNormalized_SD, ")"),
    LikedHelpNormalized = paste0("\\cellcolor{", likedhelp_c, "}", LikedHelpNormalized, " (", LikedHelpNormalized_SD, ")"),
    PacingNormalized = paste0("\\cellcolor{", pacing_c, "}", PacingNormalized, " (", PacingNormalized_SD, ")"),
    IrritationNormalized = paste0("\\cellcolor{", irritation_c, "}", IrritationNormalized, " (", IrritationNormalized_SD, ")"),
    PercNormalized = paste0("\\cellcolor{", perc_c, "}", PercNormalized, " (", PercNormalized_SD, ")"),
    FrustNormalized = paste0("\\cellcolor{", frust_c, "}", FrustNormalized, " (", FrustNormalized_SD, ")"),
    rate_blink = paste0("\\cellcolor{", rate_c, "}", rate_blink, " (", rate_blink_SD,")"),
    rate_feedback = paste0("\\cellcolor{", feedback_c, "}", rate_feedback,  " (", rate_feedback_SD, ")"),
    fishCaught = paste0("\\cellcolor{", fish_c, "}", fishCaught, " (", fishCaught_SD, ")"),
    fishLost = paste0("\\cellcolor{", lost_c, "}", fishLost, " (", fishLost_SD, ")"),
    blink_recog = paste0("\\cellcolor{", rate_acc_c, "}", blink_recog, " (", blink_recog_SD, ")"),
    Hardest = paste0("\\cellcolor{", hardest_c, "}", Hardest),
    Easiest = paste0("\\cellcolor{", easiest_c, "}", Easiest),
    time_total = paste0("\\cellcolor{", time_c, "}", time_total, " (", time_total_SD, ")"),
    perc_c = NULL, frust_c = NULL, rate_c = NULL, feedback_c = NULL, easiest_c = NULL, hardest_c = NULL, time_c = NULL,
    lost_c = NULL, fish_c = NULL, rate_acc_c = NULL, irritation_c = NULL, pacing_c = NULL, likedhelp_c = NULL, muchhelp_c = NULL,
    across(everything(), as.character)) %>% arrange(Condition) %>%
  select(`Perc. Control` = PercNormalized, `Frustration` = FrustNormalized, `How Much Help` = HowMuchHelpNormalized,
         `Liked Help` = LikedHelpNormalized, `Perc. Pacing` = PacingNormalized, `Irritation` = IrritationNormalized,
         Hardest, Easiest, `Blink Conv. Rate` = rate_blink,
         `Blink Recognition` = blink_recog,  `Pos. Feedback` = rate_feedback, `Fish Caught` = fishCaught, `Fish Lost` = fishLost,
         Duration = time_total) %>%
  select(-c(ends_with("_SD"))) %>%
  pivot_longer(cols=-c(Condition), names_to = "Variable") %>%
  pivot_wider(names_from = Condition, values_from = value)

# Add ICC Scores
Sc_table <- Sc_table %>% left_join(Sicc) %>% 
  mutate(ICC3 = ifelse(is.na(ICC3),0,ICC3),
         ICC3_c = t_color(ICC3, cri$lv_ICC, cri$colors))
         #ICC3 = ifelse(is.na(ICC3),"-",ICC3))


#,
#ICC3 = format(round(ICC3,2), nsmall = 2)

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


clmms = list(predictors = c("Frust.f", "Perc.f"),
             #random = c("bci_experience.f","Fatigue.f"),
             fixed = c("rate_feedback", "Condition.f", "Gender.f", "Order.f", "HowMuchHelp.f","LikedHelp.f","Pacing.f",
                       "pam_rate", "fishCaught","fishReel","fishUnreel", "fishLost"),
             null = c("Participant.f"),
             threshold = 0.05,
             df = St)

table = g_clmm_table(clmms)

#H1/2: No variables significantly predicted/affected participants' perceived control and frustration.

clmms = list(predictors = c("HowMuchHelp.f", "LikedHelp.f"),
             #random = c("bci_experience.f","Fatigue.f"),
             fixed = c("rate_feedback", "Condition.f", "Gender.f", "Order.f", "Pacing.f",
                       "pam_rate", "fishCaught","fishReel","fishUnreel", "fishLost"),
             null = c("Participant.f"),
             threshold = 0.05,
             df = St)
table = g_clmm_table(clmms)
#St$Condition.f = factor(St$Condition.f, levels=c("MF","IO","AS","NO"))
model.null = clm(HowMuchHelp.f ~ 1 + (1|Participant), data=St %>% filter(Condition %in% c("AS","IO","MF")))

model.condition = clmm(HowMuchHelp.f ~ 1 + (1|Participant) + Condition.f , data=St %>% filter(Condition %in% c("AS","IO","MF")))

anova(model.condition, model.null)

modelcond.summary = summary(model.condition)
modelcond.summary

#St$Condition.f = factor(St$Condition.f, levels=c("AS","MF","IO","NO"))

model.null = clm(LikedHelp.f ~ 1 + (1|Participant), data=St %>% filter(Condition %in% c("AS","IO","MF")))

model.condition = clmm(LikedHelp.f ~ 1 + (1|Participant) + Condition.f , data=St %>% filter(Condition %in% c("AS","IO","MF")))

anova(model.condition, model.null)

modelcond.summary = summary(model.condition)
modelcond.summary
#H3/4: Quantity of Help comes from 

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
# Means and ratings by participant
#############

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
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, title="“How much did you feel the game helped you?”", yaxis=list(title="How Much Help Rating"), xaxis=list(title="Participant"))
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
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, title="“I liked how the game helped me.” ", yaxis=list(title="Liked Help Rating"), xaxis=list(title="Participant"))
fig_c
orca(fig_c, "fig/participant_likedhelp.pdf", width=1150, height=350)

#############
# Conditions themselves
#############

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

