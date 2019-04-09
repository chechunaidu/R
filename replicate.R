set.seed(2006)
beads<-rep(c("blues","reds"),times=c(2,3))
beads
sample(beads,1)
B<-10000
events<-replicate(B,sample(beads,1))
tab <- table(events)
tab
prop.table(tab)
events<-sample(beads,B,replace = TRUE)
prop.table(table(events))


suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five",
             "Six", "Seven", "Eight", "Nine", "Ten",
             "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)
kings <- paste("King", suits)
mean(deck %in% kings)
