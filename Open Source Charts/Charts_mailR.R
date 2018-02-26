#Sys.setenv(JAVA_HOME='C://Program Files/Java/jre1.8.0_144')
#library(rJava)
#install_github("rpremraj/mailR")
library(mailR)

sender <- "johnpaulnicola@msn.com"

recipients <- c("jp19nicola@gmail.com", "carlamnicola@msn.com", "josephpnicolajr@msn.com", "jpnicola@sisterson.com")

email <- send.mail(from = sender,
          to = recipients,
          subject = paste("Daily Weather Charts -", Sys.Date()),
          body = "See attached charts. Have nice day!

JP",
          smtp = list(host.name = "smtp.gmail.com", port = 25, user.name = "jp19nicola@gmail.com",
          passwd = "liverpool19", ssl = TRUE, tls = TRUE),
          authenticate = TRUE,
          send = TRUE,
          attach.files = c(sprintf("C://Users/johnp/Desktop/Temp_Charts/Temp_Plot_Nicola House_%s.jpeg",Sys.Date()),
                           sprintf("C://Users/johnp/Desktop/Temp_Charts/Temp_Plot_Tampa_%s.jpeg",Sys.Date()),
                           sprintf("C://Users/johnp/Desktop/Muggy_Charts/Muggy_Plot_Nicola House_%s.jpeg",Sys.Date()),
                           sprintf("C://Users/johnp/Desktop/Precip_Charts/Precip_Plot_Nicola House_%s.jpeg",Sys.Date()),
                           sprintf("C://Users/johnp/Desktop/Precip_Charts/Precip_Plot_Tampa_%s.jpeg",Sys.Date())))
