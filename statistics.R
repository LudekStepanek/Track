install.packages("JWileymisc")

library(knitr)
library(ggplot2)
library(cowplot)
library(MASS)
library(JWileymisc)
library(data.table)


non <- tracks[Line == '8970' & Condition == 'NON', sapply (Speed, mean)]
ind <- tracks[Line == '8970' & Condition == 'IND', sapply (Speed, mean)]

non <- tracks[Line == '8970' & Condition == 'NON', unlist(Speed)]
ind <- tracks[Line == '8970' & Condition == 'IND', unlist(Speed)]

S8970 <- tracks[Line == '8970', .(S = unlist(Speed)), by  = Condition]
S8970log <- tracks[Line == '8970', .(S = log(unlist(Speed))), by  = Condition]

plot(non)

#distribution plots+means
ggplot(S8970, aes(S, colour = Condition) )+geom_density()+geom_vline(data = S8970[,mean(S), by = Condition], aes(xintercept = V1, colour = Condition))
ggplot(S8970log, aes(S, colour = Condition) )+geom_density()+geom_vline(data = S8970log[,mean(S), by = Condition], aes(xintercept = V1, colour = Condition))

#qq plots
ggplot(S8970[Condition == 'NON'], aes(sample = S))+stat_qq()+stat_qq_line()

ggplot(S8970[Condition == 'NON'], aes(sample = S)) + geom_qq(distribution = qlnorm)

# fitting distributions
norm.fit <- fitdistr(non, densfun ="normal")
logLik(norm.fit)

log.fit <- fitdistr(non, densfun ="log-normal", df = 5)
logLik(log.fit)
td <- testDistribution(non, "gamma")

plot(td)
qplot(non, bins = 300)
typeof(non)

#trajectory speed means

S8970 <- tracks[Line == '8970' & n>60, .(S = sapply(Speed, mean)), by  = Condition]
S8970log <- tracks[Line == '8970' & n>60, .(S = sapply(Speed, function(x) mean(log(x)))), by  = Condition]

ggplot(S8970, aes(S, colour = Condition) )+geom_density()+geom_vline(data = S8970[,mean(S), by = Condition], aes(xintercept = V1, colour = Condition))
ggplot(S8970log, aes(S, colour = Condition) )+geom_density()+geom_vline(data = S8970log[,mean(S), by = Condition], aes(xintercept = V1, colour = Condition))


ggplot(tracks[Line == '8970' & n>60, .(S = sapply(Speed, mean)), by  = Condition], aes(sample = S)) + geom_qq(distribution = qexp)

st <- sample(S8970$S,20000)
hist(st, breaks = 100)
log.fit <- fitdistr(st, densfun ="t")
logLik(log.fit)

params <- as.list(MASS::fitdistr(st, "gamma")$estimate)

rbt <- rbeta(1000, 0.3, 1, ncp = 0)
plot(density(rbt))

#---gamma distribution speed?----
gm <- S8970 <- tracks[Line == '8970' & n>60 & Condition == "NON", sapply(Speed, mean)]
plot(density(gm))
params <- as.list(MASS::fitdistr(gm, "gamma")$estimate)

tdg <- testDistribution(gm, "gamma")
plot(tdg)

tdn <- testDistribution(gm, "normal")
plot(tdn)

tdln <- testDistribution(log(gm), "normal")
plot(tdln)

rgm <- rgamma(100000,  4.756935, 0.7618235)
plot(density(rgm))

#---lognormal distribution speed?----
gm <- S8970 <- tracks[ n>60 & Condition == "NON", sapply(Speed, mean)]
gmi <- S8970 <- tracks[ n>60 & Condition == "IND", sapply(Speed, mean)]



plot(density(gmi))
params <- as.list(MASS::fitdistr(gmi, "lognormal")$estimate)

mean(log(gmi))
log(mean(gmi))

tdg <- testDistribution(gmi, "gamma")
plot(tdg)

tdn <- testDistribution(gm, "normal")
plot(tdn)



tdln <- testDistribution(log(gmi), "normal")
plot(tdln)

rgm <- rgamma(100000,  4.756935, 0.7618235)
plot(density(rgm))

#---lognormal distribution shape?----
sh <- S8970 <- tracks[ n>60 & Condition == "NON", sapply(DP_simplify, mean)]
shi <- S8970 <- tracks[ n>60 & Condition == "IND", sapply(DP_simplify, mean)]



plot(density(sh))
params <- as.list(MASS::fitdistr(gmi, "lognormal")$estimate)

mean(shi)
mean(log(shi))


tdg <- testDistribution(sh, "gamma")
plot(tdg)

tdn <- testDistribution(sh, "normal")
plot(tdn)



tdln <- testDistribution(log(sh), "normal")
plot(tdln)

rgm <- rgamma(100000,  4.756935, 0.7618235)
plot(density(rgm))


names(tracks)


# from https://cran.r-project.org/doc/contrib/Ricci-distributions-en.pdf
x.norm<-rnorm(n=200,m=10,sd=2) 
hist(x.norm,main="Histogram of observed data")
plot(density(x.norm),main="Density estimate of data") 
plot(ecdf(x.norm),main='Empirical cumulative distribution function')  
z.norm<-(x.norm-mean(x.norm))/sd(x.norm)
hist(z.norm)
qqnorm(z.norm)

at <- t.test(log(gm), log(gmi))
at$df
unique(tracks$Experiment)

tracks[n>60 & Experiment %in% c("2019_12_02", "2019_12_05"),
       .(S = sapply(Speed, function(x) mean(log(x)))),
       by = .(Line, Condition)
       ][,
        t.test(.SD[Condition == "NON", S], .SD[Condition == "IND", S]), by = Line][order(-statistic)]
fh[,
  t.test(.SD[Condition == "NON", S], .SD[Condition == "IND", S]) , by = Line]


# are speed and shape correlated?
#means per line
tracks[ n > 60,
           .(Shape = mean(sapply(DP_simplify, sum)),
             Log_speed = mean(sapply(Speed, function(x) (mean(log(x))))),
             Shape_SE = sd(sapply(DP_simplify, sum))/sqrt(length(DP_simplify)),
             Log_speed_SE = sd(sapply(Speed, function(x) (mean(log(x)))))/sqrt(length(DP_simplify)),
             min_Y = min(sapply(DP_simplify, sum)),
             min_X = min(sapply(Speed, function(x) (mean(log(x))))),
             max_Y = max(sapply(DP_simplify, sum)),
             max_X = max(sapply(Speed, function(x) (mean(log(x)))))
           ),
           by = .(Line, Condition)
           ][,ggplot(.SD, aes(Log_speed, Shape, colour = Condition))+
               geom_point()+
               #geom_errorbar(aes(ymin=Shape-Shape_SE, ymax=Shape+Shape_SE), width=.005) +
               #geom_errorbarh(aes(xmin=Log_speed - Log_speed_SE, xmax = Log_speed + Log_speed_SE), height=.05) +
               geom_text(aes(label=Line),hjust= -0.1, vjust= -0.1, size = 4) + 
               scale_x_continuous( limits = c(first(min_X), first(max_X))) +
               scale_y_continuous( limits = c(first(min_Y), first(max_Y)))
             ]

# per movie
tracks[ n > 60,
        .(Shape = mean(sapply(DP_simplify, sum)),
          Log_speed = mean(sapply(Speed, function(x) (mean(log(x))))),
          Shape_SE = sd(sapply(DP_simplify, sum))/sqrt(length(DP_simplify)),
          Log_speed_SE = sd(sapply(Speed, function(x) (mean(log(x)))))/sqrt(length(DP_simplify)),
          min_Y = min(sapply(DP_simplify, sum)),
          min_X = min(sapply(Speed, function(x) (mean(log(x))))),
          max_Y = max(sapply(DP_simplify, sum)),
          max_X = max(sapply(Speed, function(x) (mean(log(x)))))
        ),
        by = .(Line, Condition, movie = paste(Experiment, Field))
        ][,ggplot(.SD, aes(Log_speed, Shape, group = movie, colour = Condition))+
            geom_point()+
            #geom_errorbar(aes(ymin=Shape-Shape_SE, ymax=Shape+Shape_SE), width=.005) +
            #geom_errorbarh(aes(xmin=Log_speed - Log_speed_SE, xmax = Log_speed + Log_speed_SE), height=.05) +
            #geom_text(aes(label=paste(Line, Condition)),hjust= -0.1, vjust= -0.1, size = 0)+
            facet_wrap(~Line)+
            labs(title = "Mean shape and log_speed for individual movies")+
            
            scale_x_continuous( limits = c(first(min_X), first(max_X))) +
            scale_y_continuous( limits = c(first(min_Y), first(max_Y)))
          ]

#individual tracks
tracks[ n > 60 & Line == '3870',
        .(Shape_mean = mean(sapply(DP_simplify, sum)),
          Log_speed_mean = mean(sapply(Speed, function(x) (mean(log(x))))),
          Shape = (sapply(DP_simplify, sum)),
          Log_speed = (sapply(Speed, function(x) (mean(log(x)))))
          
        ),
        by = .(Line, Condition, movie = paste(Experiment, Field))
        ][,ggplot(.SD, aes(Log_speed_mean, Shape_mean, group = movie, colour = movie))+
            
            geom_point(aes(Log_speed, Shape, colour = Condition), size = 0.1, alpha = 0.2)+
            geom_point(colour = "black")+
            facet_wrap(~movie)+
            labs(title = "Mean shape and log_speed for individual movies")+
            theme(legend.position = 'none')
          ]

tracks[ n > 60 & Condition == 'NON',
        .(
          Shape = (sapply(DP_simplify, sum)),
          Speed = (sapply(Speed, function(x) (mean((x)))))
        ),
        by = .(Line, Condition, movie = Field)
        ][,
          ggplot(.SD)+
            geom_boxplot(aes(factor(movie, levels = 1:20), Speed))+
            labs(title = "Mean speed for individual imaging positions")+
            theme(legend.position = 'none')+
            coord_cartesian(ylim=c(3, 8))
          ]

tracks[ n > 60 & Condition == 'NON',
        .(
          Shape = (sapply(DP_simplify, sum)),
          Speed = (sapply(Speed, function(x) (mean((x)))))
        ),
        by = .(Line, Condition, movie = Field)
        ][,
          ggplot(.SD)+
            geom_boxplot(aes(factor(movie, levels = 1:20), Shape))+
            labs(title = "Mean speed for individual imaging positions")+
            theme(legend.position = 'none')+
            coord_cartesian(ylim=c(7, 17))
          ]
