install.packages("pdftools")
library("pdftools")
pdf_subset('C:/Users/micha/OneDrive - University of Glasgow/PhD/teaching/BioStats/BioStatsBookdown/_book/BioStatsBookdown.pdf',
           pages = 74:82, output = "NotesWeek7.pdf")
pdf_subset('C:/Users/micha/OneDrive - University of Glasgow/PhD/teaching/BioStats/BioStatsBookdown/_book/BioStatsBookdown.pdf',
           pages = 82:95, output = "NotesWeek8.pdf")
pdf_subset('C:/Users/micha/OneDrive - University of Glasgow/PhD/teaching/BioStats/BioStatsBookdown/_book/BioStatsBookdown.pdf',
           pages = 95:103, output = " NotesWeek9.pdf")
