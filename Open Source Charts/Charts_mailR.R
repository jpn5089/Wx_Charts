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
          attach.files = c(sprintf("C:\\Users\\John\\Desktop\\Temp_Plots\\Temp_Plot_Nicola House_%s.jpeg",Sys.Date()),
                           sprintf("C:\\Users\\John\\Desktop\\Temp_Plots\\Temp_Plot_Tampa_%s.jpeg",Sys.Date()),
                           sprintf("C:\\Users\\John\\Desktop\\Muggy_Plots\\Muggy_Plot_Nicola House_%s.jpeg",Sys.Date()),
                           sprintf("C:\\Users\\John\\Desktop\\Precip_Plots\\Precip_Plot_Nicola House_%s.jpeg",Sys.Date()),
                           sprintf("C:\\Users\\John\\Desktop\\Precip_Plots\\Precip_Plot_Tampa_%s.jpeg",Sys.Date())))