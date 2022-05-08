#Exploratory data analysis
#boxplot bedrooms and price
#excluded level 11 and 33 from visualization cause only one data per level
dati$facbedrooms<-factor(dati$bedrooms)
df1 <- dati %>% filter(facbedrooms %in% c(1,2,3,4,5,6,7,8,9,10)) 
df1 <- df1 %>% mutate(facbedrooms=droplevels(facbedrooms))
ggplot(df1, aes(y=log(price), x=facbedrooms,fill=facbedrooms))+geom_boxplot(outlier.color="blue")+ theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))+ labs(x="N. of bedrooms", y="price (log)", title="Number of bedrooms vs price")+theme(legend.position="none")+scale_fill_hue(c=45, l=80)
# https://c-woo.github.io/bar_plots_for_analysis
#scatterplot price vs. lot size and waterfront
highlight_df <- dati %>% filter(waterfront==1)
dati %>% ggplot(aes(x=log(sqft_lot), y=log(price),color=waterfront)) + geom_point(alpha=.4)+ geom_point(data=highlight_df, aes(x=log(sqft_lot),y=log(price)),alpha=0.3)+labs(x="size lot(sqft)", y="price (log)", title="Size lot (sqft) vs price and waterfront")

#check data type of each column in the dataset
str(dati)
#transform into factor data type
columns<-c("waterfront","view","condition","grade") 
dati[columns]<-lapply(dati[columns],factor)
#remove NA values
dati<-drop_na(dati)

#boxplot view vs. price
ggplot(dati, aes(y=log(price), x=view))+geom_boxplot(fill="Pink",outlier.color="blue")+ labs(x="view", y="price (log)", title="View vs price")

#boxplot condition vs price
ggplot(df1, aes(y=log(price), x=condition))+geom_boxplot(fill="Pink",outlier.color="blue")+ labs(x="condition", y="price (log)", title="Condition vs price")
#boxplot grade vs price
df1$facgrade<-as.factor(df1$grade)
ggplot(df1, aes(y=log(price), x=grade,fill=grade))+geom_boxplot(outlier.color="blue")+ labs(x="grade", y="price (log)", title="Grade vs price")+theme(legend.position="none")

#scatterplot bathrooms vs. price
dati %>% ggplot(aes(x=round(bathrooms), y=log(price))) + geom_point()+labs(x="size lot(sqft)", y="price (log)", title="Size lot (sqft) vs price and waterfront")+scale_x_continuous(breaks=seq(0,10,1))+geom_smooth(method='lm', formula= y~x)+labs(x="n. of bathrooms", y="price (log)", title="N. of bathrooms vs price")
#boxplot bathrooms vs price
df1$facbathrooms<-as.factor(df1$bathrooms)
ggplot(df1, aes(y=log(price), x=facbathrooms,fill=facbathrooms))+geom_boxplot(outlier.color="blue")+ theme(axis.text.x=element_text(angle = 90,vjust=1,hjust=1))+ labs(x="N. of bathrooms", y="price (log)", title="Number of bathrooms vs price")+theme(legend.position="none") + scale_fill_hue(c=45, l=80)
dati$facbathrooms<-as.factor(dati$bathrooms)
ggplot(dati, aes(y=log(price), x=facbathrooms,fill=facbathrooms))+geom_boxplot(outlier.color="blue")+ theme(axis.text.x=element_text(angle = 90,vjust=1,hjust=1))+ labs(x="N. of bathrooms", y="price (log)", title="Number of bathrooms vs price")+theme(legend.position="none") + scale_fill_hue(c=45, l=80)

#scatterplot living area, price and having basement or not
dati$basement<- ifelse(dati$sqft_basement > 0,"1","0")
ggplot(data = dati, aes(x = sqft_living15, y = price, group=basement, color=basement))+
xlab("Living Area")+ylab("Price") +ylim(0,4000000)+xlim(0,8000)+geom_point(alpha = 0.4)+
geom_smooth(method='lm', aes(color=basement),fullrange=TRUE)+
ggtitle("Price, Living area and Basement")+scale_color_manual(values=c("#EA9999", "#56B4E9"))
#scatterplot living_space15 vs living_space
ggplot(data=dati)+
 geom_point(aes(x=sqft_living,y=price,colour="sqft_living"),alpha=0.5)+
geom_point(aes(x=sqft_living15,y=price,colour="sqft_living15"),alpha=0.5)+ylim(0,6000000)+xlim(0,9000)+
xlab("Living Area")+ylab("Price")+scale_color_manual(values=c("#EA9999", "#56B4E9"))+labs(x="Living Area",y="Price",title="Living space neighbors vs. living space property")
#boxplot waterfront view
ggplot(data = dati, aes(x = facwaterfront, y = log(price),fill=facwaterfront))+ 
xlab("waterfront")+ylab("Price")+geom_boxplot()+
ggtitle("Price vs waterfront view")+theme(legend.position = "none")
## maybe trash ggplot(data = dati, aes(x = facwaterfront, y = price, fill=facwaterfront))+ ylim(0,5000000)+
xlab("waterfront")+ylab("Price")+geom_boxplot()+
geom_smooth(method='lm', aes(color=basement),fullrange=TRUE)+
ggtitle("Price vs waterfront view")+scale_color_manual(values=c("#EA9999", "#56B4E9"))+theme(legend.position = "none")+scale_fill_hue(c=45,l=80)

#boxplot price vs view
dati%>%group_by(facview)%>%
ggplot(aes(y=log(price), x=facview,fill=facview)) +
geom_boxplot()+ ylim(0,5000000) + labs(x="View",y="Price",title="Price vs. View") + theme(legend.position="none")+scale_fill_hue(c=45, l=80)