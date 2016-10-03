setwd("~/Analyses/TrumpClinton1")


library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(cognizer)
library(reshape2)
library(scales)
library(labeling)

dat <- read.csv("debate.csv", stringsAsFactors = FALSE)

## Cognizer ----------------


dat <- filter(dat, Speaker != "Audience", Speaker != "CANDIDATES")
dat$rownum <- seq(1:nrow(dat))

# Sentiment Analysis
SERVICE_API_KEY = "key"
sentiment <- text_sentiment(dat$Text, SERVICE_API_KEY)

a <- ldply(1:length(sentiment), function(x) sentiment[[x]]$status)
b <- ldply(1:length(sentiment), function(x) sentiment[[x]]$docSentiment$type)
c <- ldply(1:length(sentiment), function(x) sentiment[[x]]$docSentiment$score)
c$V1 <- as.numeric(c$V1)

datSent <- bind_cols(dat,a) %>%
    rename(status=V1) %>%
    filter(status=="OK") %>%
    bind_cols(b) %>%
    rename(sentiment=V1)

datSent1 <- bind_cols(dat,a) %>%
    rename(status=V1) %>%
    filter(status=="OK") %>%
    bind_cols(b) %>%
    rename(sentiment=V1) %>%
    filter(sentiment != "neutral") %>%
    bind_cols(c) %>%
    rename(score=V1)

dat <- left_join(dat, datSent)
dat <- left_join(dat, datSent1)

# Tone Analyzer
SERVICE_USERNAME_PASSWORD = "username:password"
tone <- text_tone(dat$Text, SERVICE_USERNAME_PASSWORD)
saveRDS(tone, file="tone.RDS")

social_tone <- ldply(1:length(tone), function(x) spread(tone[[x]]$document_tone$tone_categories$tones[[3]][,c(1,3)], tone_name, score))
analytic_tone <- ldply(1:length(tone), function(x) spread(tone[[x]]$document_tone$tone_categories$tones[[2]][,c(1,3)], tone_name, score))
emotional_tone <- ldply(1:length(tone), function(x) spread(tone[[x]]$document_tone$tone_categories$tones[[1]][,c(1,3)], tone_name, score))

dat <- bind_cols(dat, emotional_tone)
dat <- bind_cols(dat, analytic_tone)
dat <- bind_cols(dat, social_tone)

write.csv(dat, "dat_full.csv", row.names = FALSE)

# Graphs 
SentDist <- dat %>%
    filter(Speaker != "Holt") %>%
    ggplot(aes(x=score,fill=Speaker)) +
    geom_density(alpha=.5) +
    scale_fill_manual(values=c("blue","red")) +
    xlab("Statement Sentiment Score") +
    ggtitle("Sentiment Distribution of Candidates Statements: Debate 1") +
    theme_minimal()

SentScore <- dat %>%
    filter(Speaker != "Holt", sentiment!= "neutral") %>%
    group_by(Speaker, sentiment) %>%
    summarize(statements=n_distinct(Line)) %>%
    ggplot(aes(x=sentiment, y=statements, fill=Speaker, label=statements)) +
    geom_bar(position = "dodge", stat="identity") +
    geom_text(position=position_dodge(width = 1), vjust=1) +
    scale_fill_manual(values=c("blue","red")) +
    xlab("Statement Sentiment") +
    ylab("# of Statements") +
    ggtitle("Sentiment of Candidates Statements: Debate 1") +
    theme_minimal()

emo <- dat %>%
    filter(Speaker != "Holt") %>%
    group_by(Speaker) %>%
    summarize(anger=mean(Anger, na.rm=TRUE), disgust=mean(Disgust, na.rm=TRUE), 
              fear=mean(Fear, na.rm=TRUE), joy=mean(Joy, na.rm=TRUE),
              sadness=mean(Sadness, na.rm=TRUE)) %>%
    melt() %>%
    ggplot(aes(x=variable, y=value, fill=Speaker, label=round(value,2))) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(position = position_dodge(width=1), vjust=1) +
    ggtitle("Mean Emotional Tones by Speaker") +
    scale_fill_manual(values=c("blue","red")) +
    ylab("Score") +
    xlab("Emotional Tone") +
    theme_minimal()

language <- dat %>%
    filter(Speaker != "Holt") %>%
    group_by(Speaker) %>%
    summarize(Analytical=mean(Analytical, na.rm=TRUE),
              Confident=mean(Confident, na.rm=TRUE),
              Tentative=mean(Tentative, na.rm=TRUE)) %>%
    melt() %>%
    ggplot(aes(x=variable, y=value, fill=Speaker, label=round(value,2))) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(position = position_dodge(width=1), vjust=1) +
    ggtitle("Mean Language Tones by Speaker") +
    scale_fill_manual(values=c("blue","red")) +
    ylab("Score") +
    xlab("Language Tone") +
    theme_minimal()

social <- dat %>%
    filter(Speaker != "Holt") %>%
    group_by(Speaker) %>%
    summarize(Agreeableness=mean(Agreeableness, na.rm=TRUE),
              Conscientiousness=mean(Conscientiousness, na.rm=TRUE),
              EmotionalRange=mean(`Emotional Range`, na.rm=TRUE),
              Openness=mean(Openness, na.rm=TRUE),
              Extraversion=mean(Extraversion, na.rm=TRUE)) %>%
    melt() %>%
    ggplot(aes(x=variable, y=value, fill=Speaker, label=round(value,2))) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(position = position_dodge(width=1), vjust=1) +
    ggtitle("Mean Social Tones by Speaker") +
    scale_fill_manual(values=c("blue","red")) +
    ylab("Score") +
    xlab("Social Tone") +
    theme_minimal()
