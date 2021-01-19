#this script calculates cost beneft analysis of different properties in Monopoly
library(ggplot2)


properties<-c("Med. Ave","Baltic Ave","Oriental Ave","Vermont Ave","Conn. Ave","St. Charles Pl.","States Ave","Virginia Ave","St. James Pl.","Tenn. Ave","NY Ave","Kent. Ave","Indiana Ave","Ill. Ave","Atlantic Ave","Vent. Ave","Marvin Gardens","Pacific Ave.","NC Ave","Penn. Ave","Park Place","Boardwalk")
price<-c(60,60,100,100,120,140,140,160,180,180,200,220,220,240,260,260,280,300,300,320,350,400)
housePrice<-c(50,50,50,50,50,100,100,100,100,100,100,150,150,150,150,150,150,200,200,200,200,200)
rent<-c(2,4,6,6,8,10,10,12,14,14,16,18,18,20,22,22,24,26,26,28,35,50)
rent_1_house<-c(10,20,30,30,40,50,50,60,70,70,80,90,90,100,110,110,120,130,130,150,175,200)
rent_2_house<-c(30,60,90,90,100,150,150,180,200,200,220,250,250,300,330,330,360,390,390,450,500,600)
rent_3_house<-c(90,180,270,270,300,450,450,500,550,550,600,700,700,750,800,800,850,900,900,1000,1100,1400)
rent_4_house<-c(160,320,400,400,450,625,625,700,750,750,800,875,875,925,975,975,1025,1100,1100,1200,1300,1700)
rent_hotel<-c(250,450,550,550,600,750,750,900,950,950,1000,1050,1050,1100,1150,1150,1200,1275,1275,1400,1500,2000)
mortgage<-c(30,30,50,50,60,70,70,80,90,90,100,110,110,120,130,130,140,150,150,160,175,200)

df<-data.frame(properties,price,housePrice,rent,rent_1_house,rent_2_house,rent_3_house,rent_4_house,rent_hotel,mortgage)

df$return_no_house<-rent/price

plot_1 <- ggplot(df)+
	geom_point(aes(x=reorder(properties,-return_no_house,sum),y=return_no_house))+
	theme(axis.text.x = element_text(angle = 90))+
	xlab("")+
	ylab("economic return with no houses or hotels")

df$return_1_house<-(price+(1*housePrice))/rent_1_house
df$return_2_house<-(price+(2*housePrice))/rent_2_house
df$return_3_house<-(price+(3*housePrice))/rent_3_house
df$return_4_house<-(price+(4*housePrice))/rent_4_house
df$return_hotel<-(price+(5*housePrice))/rent_hotel

plot_2 <- ggplot(df)+
	geom_point(aes(x=reorder(properties,-return_hotel,sum),y=return_hotel))+
	theme(axis.text.x = element_text(angle = 90))+
	xlab("")+
	ylab("economic return with \n4 houses and a hotel")

print(plot_1)
print(plot_2)
