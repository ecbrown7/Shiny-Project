# This is the server logic of a Shiny web application. You can run the

library(shiny)
library(data.table)
library(ggplot2)

#Reading in data, modifying data, creating master file called "study" that contains all information in AR2 Antagonist Ref 128 Study

# read in dpid_128 chem plate map#
dpid.128 = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/validation_chem.csv", stringsAsFactors = F, skipNul = T)
dpid.128 = as.data.table(dpid.128)

# read in control plate map#
control = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/antagonist_control_chem.csv", stringsAsFactors = F, skipNul = T)
control = as.data.table(control)

# read in cohort dispense maps#
dispmap1 = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/valid_ar_antag_cohort_1.map.csv", stringsAsFactors = F, skipNul = T)
dispmap1$cohort = 1
dispmap2 = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/valid_ar_antag_cohort_2.map.csv", stringsAsFactors = F, skipNul = T)
dispmap2$cohort = 2
dispmap3 = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/valid_ar_antag_cohort_3.map.csv", stringsAsFactors = F, skipNul = T)
dispmap3$cohort = 3

dispmap = rbind(dispmap1, dispmap2, dispmap3)
dispmap = data.table(Source.Plate = dispmap$Source.Plate.Barcode, Source.Well = dispmap$Source.Well, Destination.Well = dispmap$Destination.Well, 
                     Transfer.Volume = dispmap$Transfer.Volume,
                     cpid = dispmap$apid, cohort = dispmap$cohort)

# merge in chnm and stock.mm from source plate maps#
dispmap$chnm = ifelse(dispmap$Source.Plate == "DPID_128", dpid.128$chnm[match(dispmap$Source.Well, dpid.128$well)], 
                      ifelse(dispmap$Source.Plate == "CNTRL", control$chnm[match(dispmap$Source.Well, control$well)], 
                             ifelse(dispmap$Source.Well == "D1", "DMSO", NA)))
dispmap$stock.mm = ifelse(dispmap$Source.Plate == "DPID_128", dpid.128$stock.mM[match(dispmap$Source.Well, dpid.128$well)], 
                          ifelse(dispmap$Source.Plate == "CNTRL", control$stock.mM[match(dispmap$Source.Well, control$well)], NA))

# read in filemap #
filemap = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/AR_Antagonist128_Met_filemap.csv", stringsAsFactors = F, skipNul = T)
filemap = data.table(filemap)

### map data data ###
filemap = filemap[!TR %in% 2874:2913,]

study = NULL
for (j in 1:nrow(filemap)){
    
    file.name = paste("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/data/TRno", filemap$TR[j], ".CSV", sep = "")
    sub = read.csv(file.name, stringsAsFactors = F, skipNul = T)
    sub$X = NULL
    names(sub) = c("rowi", "coli", "sample", "RLU")
    sub$apid = filemap$TR[j]
    sub$well = paste(sub$rowi,sub$coli,sep="")
    sub$replicate = filemap$replicate[j]
    sub$biogroup = filemap$biogroup[j]
    sub$assay = filemap$assay[j]
    
    dat = data.table(Destination.Well = sub$well, RLU = sub$RLU, biogroup = sub$biogroup, replicate = sub$replicate, apid = sub$apid,
                     assay = sub$assay)
    merged = merge.data.frame(dispmap[cohort == filemap$cohort[j] & cpid == filemap$cpid[j],], dat, by = "Destination.Well")
    
    study = rbind(study,merged)
    
}
study = as.data.table(study)

study[, final.um := stock.mm * Transfer.Volume / (45 + (Transfer.Volume + 7.5)/1000)]
study[, logc := log10(final.um)]

bval.table = study[, median(RLU), by = .(apid, chnm)][chnm == "DMSO",]
study$bval = bval.table$V1[match(study$apid, bval.table$apid)]

pval1.table = study[, median(RLU), by = .(apid, chnm, logc)][chnm == "BICAL" & logc > 1.9,]
study$pval1 = pval1.table$V1[match(study$apid, bval.table$apid)]

pval2.table = study[, median(RLU), by = .(apid, chnm, logc)][chnm == "DCLN" & logc > 1.9,]
study$pval2 = pval2.table$V1[match(study$apid, bval.table$apid)]

study[, nval := ifelse(assay == "AR", (RLU - bval)/(pval1-bval)*100, (RLU - bval)/(pval2-bval)*100)]
setkey(study, chnm)
cmpd1 = study[!is.na(logc),]
#Finalized data set
AR2study_complete = cmpd1[assay == "AR",]



#plot dose-response AR data#
#Source code written to plot hill curves through EPA offline pipeline
#source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcplFit.R")
#source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcpl_Fit_Lite_Sample_Data.R")
#source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcplObjCnst.R")
#source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcplObjHill.R")
#source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcplObjGnls.R")

#hill_curve = function(hill_tp, hill_ga, hill_gw, lconc){
#    return(hill_tp/(1+10^((hill_ga - lconc)*hill_gw)))
#}


#Shiny Server
shinyServer(function(input, output) {

    output$filteredData <- renderDataTable({
         if(input$fullDataCheck == 0 && input$chemCheck == 0 && input$cypCheck == 0){sample1 <- AR2study_complete %>% select(2,7,13,10,5:9,11:12,14:19)
                                                                                     sample1}
    else if(input$fullDataCheck == 0 && input$chemCheck == 1 && input$cypCheck == 0){filtered <- AR2study_complete %>% filter(chnm == input$chemList)
                                                                                     filtered2 <- filtered %>% select(2,7,13,10,5:9,11:12,14:19)
                                                                                     filtered2}
    else if(input$fullDataCheck == 1 && input$chemCheck == 0 && input$cypCheck == 0){AR2study_complete}
    else if(input$fullDataCheck == 1 && input$chemCheck == 1 && input$cypCheck == 0){filtered <- AR2study_complete %>% filter(chnm == input$chemList)
                                                                                     filtered}
    else if(input$fullDataCheck == 1 && input$chemCheck == 1 && input$cypCheck == 1){filteredCyp <- AR2study_complete %>% filter(chnm == input$chemList) %>% filter(biogroup == input$cypList)
                                                                                     filteredCyp}
    else if(input$fullDataCheck == 0 && input$chemCheck == 1 && input$cypCheck == 1){filteredCyp2 <- AR2study_complete %>% filter(chnm == input$chemList) %>% filter(biogroup == input$cypList)
                                                                                     filteredCyp2 <- filteredCyp2 %>% select(2,7,13,10,5:9,11:12,14:19)
                                                                                     filteredCyp2}
    else if(input$fullDataCheck == 1 && input$chemCheck == 0 && input$cypCheck == 1){fullCypFilter <- AR2study_complete %>% filter(biogroup == input$cypList)
                                                                                     fullCypFilter}
    else if(input$fullDataCheck == 0 && input$chemCheck == 0 && input$cypCheck == 1){cypFinal <- AR2study_complete %>% filter(biogroup == input$cypList) %>% select(2,7,13,10,5:9,11:12,14:19)
                                                                                     cypFinal}
    })

})


