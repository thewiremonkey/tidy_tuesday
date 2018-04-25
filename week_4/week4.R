library(tidyverse)

#read in data
raw_dat<-read_csv("week4_australian_salary.csv")

#set colors
pal<-c("DarkMagenta", "OliveDrab")

#transform data
#calculate female income 'f_income', male income 'f_income' for each occupation
#calculate average income for occupation
#calculate female count of individuals f_count, 
#calculate male count of individuals m_count
#calculate total number of individtual workers 'tot_workers'
#calculate the female income ratio to the average income per occupation
#calculate the male income ratio to the average income per occupation
#calculate the ratio of female to male workers for each occupation
#greater the proportion of women, the closer to 1

dat<-raw_dat %>% group_by(occupation) %>% 
  summarise(f_income=mean(average_taxable_income[gender=="Female"]),
            m_income=mean(average_taxable_income[gender=="Male"]),
            avg_income=mean(average_taxable_income),
            f_count=mean(individuals[gender=="Female"]),
            m_count=mean(individuals[gender=="Male"]),
            tot_workers=sum(individuals)) %>% 
  mutate(f_ratio_income=f_income/avg_income, 
         m_ratio_income=m_income/avg_income, 
         f_ratio_workers=f_count/tot_workers,
         diff=abs(m_ratio_income-f_ratio_income)) %>% 
  filter(!is.nan(m_income), !is.nan(f_income))

#create a long df with the key gender for grouping/coloring in plot
plot_dat<-select(dat,occupation, f_ratio_workers, f_ratio_income, m_ratio_income) %>% 
  gather(key=gender, value=income_ratio, 3:4 )


#plot the gendered income ratio showing proportion above or below average,
#group by gender to produce separate gam lines, use color=gender to separate out
#the points
#average income proportion = 1, marked by blue line

ggplot(plot_dat, aes(x=f_ratio_workers, y=income_ratio, group=gender))+
  geom_point(aes(color=gender), alpha=0.3)+
  scale_color_manual(values=pal,
                     breaks=c("m_ratio_income", "f_ratio_income"),
                     labels=c("Male", "Female"))+
  geom_smooth(aes(x=f_ratio_workers, y=income_ratio), size=1, color="red", alpha=1.0)+
  geom_hline(aes(yintercept=1), size=1,color="blue")+
  labs(x="Proportion of Female Workers in Occupation", y="Ratio of Income vs Average Income\nFor Occupation")+
  theme_bw()
