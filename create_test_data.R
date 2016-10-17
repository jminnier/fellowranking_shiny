library(babynames)
dat = read_csv("fellow_data.csv") # real data
datc = read_csv("fellow_data_cleaned.csv") # real data cleaned

set.seed(111)
allnames = sample(babynames$name,size=50)
i_names = allnames[1:17]
f_names = allnames[18:50]

dat$Name = factor(dat$Name,labels=f_names[1:31])
dat$Interviewer = factor(dat$Interviewer,labels=i_names)
write_csv(dat,path ="sample_data.csv")

datc$Name = factor(datc$Name,labels=f_names[1:31])
datc$Interviewer = factor(datc$Interviewer,labels=i_names[1:15])
write_csv(datc,path ="sample_data_cleaned.csv")
