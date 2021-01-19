#script that plays monopoly way to many times
library(ggplot2)

n = 10000000 #10,000,000 rolls took 11 minutes.
start = 1 #Start at Go (space number 1).
landed = 0 #Dummy value, needs to be 0 for the for-loop to work.
out <- c() #Allocate space.
over = c() #Allocate space.
rolls = c() #Allocate space.

for(i in 1:n){ #Run the for-loop for n number of times.
	diceRoll<-c() #Allocate space each roll of the dice.
	start <- landed #Start where you're last turn ended.
	diceRoll <- sum(rowSums(replicate(1, sample(6, 2, replace=T)))) #Rolling two dice that have values 1-6, and sum those dice.
	landed <- start+diceRoll
	
	if(landed > 40)
	{landed = (landed - 40)}
	else
	{landed = landed}
	
	out[i] <- landed}

df<-aggregate(data.frame(count = out), list(value = out), length)

df$names<-c("Go","Med. Ave","Community Chest 1","Baltic Ave",
	"Income Tax","Reading Rail.","Oriental Ave.","Chance 1","Vermont Ave",
	"Conn. Ave","Jail","St. Charles","Electric Comp","States Ave","Virginia Ave",
	"Penn. Railroad","St. James","Community Chest 2","Tenn. Ave","NY Ave","Free Parking",
	"Kentucky Ave","Chance 2","Indiana Ave","Ill. Ave","B&O Rail.","Atlantic Ave","Venture Ave.",
	"Water Works","Marvin Gardens","Go to Jail","Pacific Ave.","NC Ave","Community Chest 3",
	"Penn. Ave","Short Line","Chance 3","Park Place","Luxury Tax","Boardwalk")

df$sub<-df$count-min(df$count)
df$rel_chance<-df$count/n
df$per_rel_chance<-df$rel_chance*100

plot<-ggplot(df)+
	geom_point(aes(reorder(names,-per_rel_chance,sum),per_rel_chance))+
	theme(axis.text.x = element_text(angle=90,vjust = 0.2))+
	xlab("")+
	ylab("")

plot2<-ggplot(df)+
	geom_density(aes(per_rel_chance),cex=2)+
	theme_light()+
	xlab("percent relative chance of landing on a given space")

#print(plot2)

print(plot)


