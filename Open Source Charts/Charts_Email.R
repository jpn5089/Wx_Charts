library(RDCOMClient)

OutApp <- COMCreate("Outlook.Application")


outMail = OutApp$CreateItem(0)

outMail[["To"]] = "jp19nicola@gmail.com; carlamnicola@msn.com; josephpnicolajr@msn.com; jpnicola@sisterson.com"

outMail[["subject"]] = "Daily Weather Charts"
outMail[["body"]] = "See charts attached.

JP"

outMail[["Attachments"]]$Add(sprintf("C:\\Users\\John\\Desktop\\Temp_Plots\\Temp_Plot_Pittsburgh_%s.jpeg",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("C:\\Users\\John\\Desktop\\Temp_Plots\\Temp_Plot_Tampa_%s.jpeg",Sys.Date()))
#outMail[["Attachments"]]$Add(sprintf("C:\\Users\\John\\Desktop\\Muggy_Plots\\Muggy_Plot_Pittsburgh_%s.jpeg",Sys.Date()))
#outMail[["Attachments"]]$Add(sprintf("C:\\Users\\John\\Desktop\\Muggy_Plots\\Muggy_Plot_Tampa_%s.jpeg",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("C:\\Users\\John\\Desktop\\Precip_Plots\\Precip_Plot_Pittsburgh_%s.jpeg",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("C:\\Users\\John\\Desktop\\Precip_Plots\\Precip_Plot_Tampa_%s.jpeg",Sys.Date()))

outMail$Send()

rm(OutApp,outMail)
