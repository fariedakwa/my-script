# my-script
#determining the probabilities of the different variables while holding  the other variables constant
bs = read.csv(file.choose(), header = T)
rbt = read.csv(file.choose(), header = T)
eg = read.csv(file.choose(), header = T)
attach(mydata)

library(nnet)
mymodel1 = multinom(out ~ Disturber + OrigPos + Site + Season, data = bs)
mymodel2 = multinom(out ~ Disturber + OrigPos + Site + Season, data = eg)
mymodel3 = multinom(out ~ Disturber + OrigPos + Site + Season, data = rbt)
summary(mymodel)

dwrite <- data.frame(Season = rep(c("EW", "MW", "LW")), OrigPos = rep(c("a","b1","b2","c","d","e"),5), 
                     Site= rep(c("Ngweshla", "Makwa", "Kennedy1","Nyamandlovu", "Guvalala"),6),
                     Disturber= rep(c("BOP","Herbivores", "OtherBirds", "Unknown","People", "GroundCarnivores"),5))
pp.write <- cbind(dwrite, predict(mymodel, newdata = dwrite, type = "probs", se = TRUE))
by(pp.write[, 5:7], pp.write$Season, colMeans)

#Red-billed Teal
#1.1 season
rbt = read.csv(file.choose(), header = T)
attach(rbt)
dteal1 <- data.frame(Season = rep(c("EW", "MW", "LW"),10), OrigPos = rep(c("a","b1","b2","c","d")), 
                    Site= rep(c("Ngweshla","Nyamandlovu", "Guvalala"),10),
                    Disturber= rep(c("BOP","Herbivores", "OtherBirds","People", "GroundCarnivores"),6)) 
pp.teal <- cbind(dteal1, predict(mymodel1, newdata = dteal1, type = "probs", se = TRUE))
by(pp.teal[, 5:7], pp.teal$OrigPos, colMeans)

#Egyptian Goose
#1.1 season
eg = read.csv(file.choose(), header = T)
attach(eg)
degyptian <- data.frame(Season = rep(c("EW", "MW", "LW"), 10), OrigPos = rep(c("b1","b2","c","d", "e")), 
                        Site= rep(c("Ngweshla","Nyamandlovu", "Guvalala", "Kennedy1", "Makwa"),6),
                        Disturber= rep(c("BOP","Herbivores", "OtherBirds","People", "GroundCarnivores", "Unknown"),5)) 
pp.egyptian <- cbind(degyptian, predict(mymodel1, newdata = degyptian, type = "probs", se = TRUE))
by(pp.egyptian[, 5:7], pp.egyptian$Disturber, colMeans)

#Blacksmith Lapwing
bs = read.csv(file.choose(), header = T)
attach(bs)
dblacksmith <- data.frame(Season = rep(c("EW", "MW", "LW"), 20), OrigPos = rep(c("b2","c","d", "e"), 15), 
                        Site= rep(c("Ngweshla","Nyamandlovu", "Guvalala", "Kennedy1", "Makwa"),12),
                        Disturber= rep(c("BOP","Herbivores", "OtherBirds","People", "GroundCarnivores", "Unknown"))) 
pp.blacksmith <- cbind(dblacksmith, predict(mymodel1, newdata = dblacksmith, type = "probs", se = TRUE))
by(pp.blacksmith[, 5:7], pp.blacksmith$Disturber, colMeans)

