library(RDCOMClient)

OutApp <- COMCreate("Outlook.Application")


outMail = OutApp$CreateItem(0)

outMail[["To"]] = ""

outMail[["subject"]] = "Daily Weather Charts"
outMail[["body"]] = "Good Morning. See charts attached.

JP"

outMail[["Attachments"]]$Add(sprintf("T:\\Weather Charts\\Temp_Plot_Akron_%s.pdf",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("T:\\Weather Charts\\Temp_Plot_Allentown_%s.pdf",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("T:\\Weather Charts\\Temp_Plot_Cleveland_%s.pdf",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("T:\\Weather Charts\\Temp_Plot_Columbus_%s.pdf",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("T:\\Weather Charts\\Temp_Plot_Philadelphia_%s.pdf",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("T:\\Weather Charts\\Temp_Plot_Pittsburgh_%s.pdf",Sys.Date()))
outMail[["Attachments"]]$Add(sprintf("T:\\Weather Charts\\Temp_Plot_Toledo_%s.pdf",Sys.Date()))

outMail$Send()

rm(OutApp,outMail)
