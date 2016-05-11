library(RDCOMClient)

OutApp <- COMCreate("Outlook.Application")


outMail = OutApp$CreateItem(0)

outMail[["To"]] = "jpnicola@sisterson.com; carlamnicola@msn.com, josephpnicolajr@msn.com"

outMail[["subject"]] = "Temperature Plots"
outMail[["body"]] = "See charts attached.

JP"

outMail[["Attachments"]]$Add(sprintf("C:\\Users\\John\\Desktop\\Temp_Plots\\Temp_Plot_Jackson Hole_%s.jpeg",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("C:\\Users\\John\\Desktop\\Temp_Plots\\Temp_Plot_Tampa_%s.jpeg",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("C:\\Users\\John\\Desktop\\Temp_Plots\\Temp_Plot_Pittsburgh_%s.jpeg",Sys.Date()))


outMail$Send()

rm(OutApp,outMail)
