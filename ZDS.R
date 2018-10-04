C012016 = 6174 # number of communities listed in january 2016
L012016 = 4 # number of leads/communities listed in january 2016

TC012016 = 38000 # total number of active new construction community in US 
MSP = 350000 # median sales price for home
MC = 0.01 # 1% marketing cost 
AP = 0.05 # 5% profit after expenses

LG2016 = 0.05 # Leads growth MOM 2016
LG2017 = 0.04 # Leads growth MOM 2017
LG2018 = 0.01 # Leads growth MOM 2018

CG2016 = 0.06 # New construction community advertising growth MOM 2016 for the Pay Per Lead 
CG2017 = 0.04 # New construction community advertising growth MOM 2017 for the Pay Per Lead 
CG2018 = 0.02 # New construction community advertising growth MOM 2018 for the Pay Per Lead 

PM1 = 40 # $40/lead
PM2 = 400 # $400/commiunity/month

months = seq(as.Date("2016/1/1"), by = "month", length.out = 36)
year = as.numeric(format(months,'%y'))

df=0
df= data.frame(df)
df = cbind(df,months,year)

df['Leads_Growth_MoM'] <-0
df$Leads_Growth_MoM <- ifelse(df$year==16,LG2016,df$Leads_Growth_MoM)
df$Leads_Growth_MoM <- ifelse(df$year==17,LG2017,df$Leads_Growth_MoM)
df$Leads_Growth_MoM <- ifelse(df$year==18,LG2018,df$Leads_Growth_MoM)

df['Comm_Growth_MoM'] <-0
df$Comm_Growth_MoM <- ifelse(df$year==16,CG2016,df$Comm_Growth_MoM)
df$Comm_Growth_MoM <- ifelse(df$year==17,CG2017,df$Comm_Growth_MoM)
df$Comm_Growth_MoM <- ifelse(df$year==18,CG2018,df$Comm_Growth_MoM)

df['Total_Adv_communities'] <- 0
df$Total_Adv_communities[1] <- 6174
for (i in 2:length(df$months)) {
  df$Total_Adv_communities[i] <- (df$Comm_Growth_MoM[i]*df$Total_Adv_communities[i-1])+df$Total_Adv_communities[i-1] 
}

df['Total_leads'] <- 0
df$Total_leads[1] <- 4
for (i in 2:length(df$months)) {
  df$Total_leads[i] <- (df$Leads_Growth_MoM[i]*df$Total_leads[i-1])+df$Total_leads[i-1] 
}

df['PM1_price'] <- 0
df$PM1_price <- df$Total_Adv_communities*df$Total_leads*40

df['Comm_Growth_MoM1'] <-0
df$Comm_Growth_MoM1 <- ifelse(df$year==16,CG2016*0.9,df$Comm_Growth_MoM1)
df$Comm_Growth_MoM1 <- ifelse(df$year==17,CG2017*0.9,df$Comm_Growth_MoM1)
df$Comm_Growth_MoM1 <- ifelse(df$year==18,CG2018*0.9,df$Comm_Growth_MoM1)

df['Total_Adv_communities1'] <- 0
df$Total_Adv_communities1[1] <- 6174
for (i in 2:length(df$months)) {
  df$Total_Adv_communities1[i] <- (df$Comm_Growth_MoM1[i]*df$Total_Adv_communities1[i-1])+df$Total_Adv_communities1[i-1] 
}

df['PM2_price'] <- 0
df$PM2_price <- df$Total_Adv_communities1*400

# plot listing by both pricing models
library('ggplot')
ggplot(df, aes(x = months, y = Total_Adv_communities)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
