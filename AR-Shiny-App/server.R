# This is the server logic of a Shiny web application. You can run the

library(shiny)
library(data.table)
library(ggplot2)
library(caret)
library(randomForest)
library(gbm)

#Reading in data, modifying data, creating master file called "study" that contains all information in AR2 Antagonist Ref 128 Study

# read in dpid_128 chem plate map#
dpid.128 = read.csv("../ar_shiny_data/validation_chem.csv", stringsAsFactors = F, skipNul = T)
dpid.128 = as.data.table(dpid.128)

# read in control plate map#
control = read.csv("../ar_shiny_data/antagonist_control_chem.csv", stringsAsFactors = F, skipNul = T)
control = as.data.table(control)

# read in cohort dispense maps#
dispmap1 = read.csv("../ar_shiny_data/valid_ar_antag_cohort_1.map.csv", stringsAsFactors = F, skipNul = T)
dispmap1$cohort = 1
dispmap2 = read.csv("../ar_shiny_data/valid_ar_antag_cohort_2.map.csv", stringsAsFactors = F, skipNul = T)
dispmap2$cohort = 2
dispmap3 = read.csv("../ar_shiny_data/valid_ar_antag_cohort_3.map.csv", stringsAsFactors = F, skipNul = T)
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
filemap = read.csv("../ar_shiny_data/AR_Antagonist128_Met_filemap.csv", stringsAsFactors = F, skipNul = T)
filemap = data.table(filemap)

### map data data ###
filemap = filemap[!TR %in% 2874:2913,]

study = NULL
for (j in 1:nrow(filemap)){
    
    file.name = paste("../ar_shiny_data/data/TRno", filemap$TR[j], ".CSV", sep = "")
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

########For hit calls - #gives the global, zero-centered (median = 0) mad for DMSO
bmad1 = study[assay == "AR" & chnm == "DMSO", mad(nval)] 
threshold = 5*bmad1
med.resp.by.chnm.dose.biogroup = study[assay == "AR" & chnm != "DMSO", .(med.resp = median(nval)), by = .(chnm, logc, biogroup)]
hits.all.logc.and.biogroups = med.resp.by.chnm.dose.biogroup[med.resp > threshold,]
unique.hits.antagonist = data.table(unique(hits.all.logc.and.biogroups$chnm))
########

cmpd1 = study[!is.na(logc),]
#Finalized data set
AR2study_complete = cmpd1[assay == "AR",]

#plot dose-response AR data#
#Source code written to plot hill curves through EPA offline pipeline
source("../ar_shiny_data/tcplFit.R")
source("../ar_shiny_data/tcpl_Fit_Lite_Sample_Data.R")
source("../ar_shiny_data/tcplObjCnst.R")
source("../ar_shiny_data/tcplObjHill.R")
source("../ar_shiny_data/tcplObjGnls.R")

hill_curve = function(hill_tp, hill_ga, hill_gw, lconc){
    return(hill_tp/(1+10^((hill_ga - lconc)*hill_gw)))}

#Sample data is a data frame with 5 chemicals. Change to data table, then loop over chemical name through tcplFit
#tcplFit needs something called bmad, but it doesn't impact the results, 
#but can limit the number of curves attempted to be fit, so set smaller than you think it really is
bmad = .001

#Set dataset to plot with without overriding AR2study_complete
AR2plotting <- AR2study_complete
AR2plotting$biogroup <- factor(AR2plotting$biogroup, levels = c("noRNA","Bgal","CYP1A2","CYP2A6","CYP2B6","CYP2C8","CYP2C9","CYP2C19","CYP2D6","CYP2E1","CYP2E1-WT","CYP2J2","CYP3A4"))

AR2plotting$spid = AR2plotting$biogroup
AR2plotting$resp = AR2plotting$nval



###########################################
### plot dose-response Alamar Blue data ###
AB = cmpd1[assay == "Viability",]
ABplotting <- AB
ABplotting$biogroup <- factor(AR2plotting$biogroup, levels = c("noRNA","Bgal","CYP1A2","CYP2A6","CYP2B6","CYP2C8","CYP2C9","CYP2C19","CYP2D6","CYP2E1","CYP2E1-WT","CYP2J2","CYP3A4"))
ABplotting$spid = ABplotting$biogroup
ABplotting$resp = ABplotting$nval



##########For modeling and hit calls
#read in chemotypes
chemotypes = read.csv("../ar_shiny_data/ref_chemotypes.csv", stringsAsFactors = F, skipNul = T)
colnames(chemotypes)[1] = "Chemical"
chemotypes$hitcall <- as.factor(chemotypes$hitcall)
chemotypeModel <- chemotypes[,-1]
hitsData <- chemotypes[chemotypes$hitcall == 1,]
sample_chemotypes = read.csv("../ar_shiny_data/sample_chemotypes.csv", stringsAsFactors = F, skipNul = T)
sample_chemotypes <- sample_chemotypes[-6,]
subsetChemotypes <- chemotypes[, colSums(chemotypes != 0) > 0]
subsetChemotypesB <- subsetChemotypes[,-1]
subsetChemotypesC <- subsetChemotypesB[c(47, 67, 73, 88,89),-1]
chemoHist = read.csv("../ar_shiny_data/chemoHist.csv", stringsAsFactors = F, skipNul = T)

######Creating list of most frequent chemotypes######
#testg <- ggplot(chemoHist, aes(y = chemoHist$chemonum, x = chemoHist$numchemos)) + geom_histogram(stat = "identity") + 
#  xlab("Number of Chemotypes") + ylab("Frequency") + ggtitle("Frequency of Number of Chemotypes Including Zero")

#chemoHist1 <- chemoHist[chemoHist$numchemos != 0,]
#testg1 <- ggplot(chemoHist1, aes(y = chemoHist1$chemonum, x = chemoHist1$numchemos)) + geom_histogram(stat = "identity") + 
#  xlab("Number of Chemotypes") + ylab("Frequency") + ggtitle("Frequency of Number of Chemotypes Excluding Zero")

#chemoHist2 <- chemoHist1[chemoHist1$numchemos > 20,]
#colnames(chemoHist2) = c("Num Chemotypes", "Chemotype Number")
#rownames(chemoHist2) = NULL
#chemoHist2$`Chemotype Number`

subsetChemotypesD <- chemotypes[,c(1,2, 38, 72,88, 124, 125, 130, 186, 214, 303, 425, 433, 436, 437, 438, 439, 440, 477, 478, 587)]
subsetChemotypesE <- subsetChemotypesD[,-1]
###########



#Shiny Server
shinyServer(function(input, output) {

    output$filteredData <- renderDataTable({
         if(input$fullDataCheck == 0 && input$chemCheck == 0 && input$cypCheck == 0){AR2study_complete}
    else if(input$fullDataCheck == 0 && input$chemCheck == 1 && input$cypCheck == 0){filtered <- AR2study_complete %>% filter(chnm == input$chemList)
                                                                                     filtered}
    else if(input$fullDataCheck == 1 && input$chemCheck == 0 && input$cypCheck == 0){filter <- AR2study_complete %>% select(7,10,13,11,14,15,19)
                                                                                     filter}
    else if(input$fullDataCheck == 1 && input$chemCheck == 1 && input$cypCheck == 0){filtered <- AR2study_complete %>% select(7,10,13,11,14,15,19) %>% filter(chnm == input$chemList)
                                                                                     filtered}
    else if(input$fullDataCheck == 1 && input$chemCheck == 1 && input$cypCheck == 1){filteredCyp <- AR2study_complete %>% select(7,10,13,11,14,15,19) %>% 
                                                                                                    filter(chnm == input$chemList) %>% filter(biogroup == input$cypList)
                                                                                     filteredCyp}
    else if(input$fullDataCheck == 0 && input$chemCheck == 1 && input$cypCheck == 1){filteredCyp2 <- AR2study_complete %>% filter(chnm == input$chemList) %>% filter(biogroup == input$cypList)
                                                                                     filteredCyp2}
    else if(input$fullDataCheck == 1 && input$chemCheck == 0 && input$cypCheck == 1){fullCypFilter <- AR2study_complete %>% select(7,10,13,11,14,15,19) %>% filter(biogroup == input$cypList)
                                                                                     fullCypFilter}
    else if(input$fullDataCheck == 0 && input$chemCheck == 0 && input$cypCheck == 1){cypFinal <- AR2study_complete %>% filter(biogroup == input$cypList)
                                                                                     cypFinal}
    })
    
    observe({ if(input$saveButton == 0){} 
      else if(input$saveButton == 1){
        
        if(input$fullDataCheck == 0 && input$chemCheck == 0 && input$cypCheck == 0){write.csv(AR2study_complete, file = "AR2_FullData", row.names = FALSE)}
        else if(input$fullDataCheck == 0 && input$chemCheck == 1 && input$cypCheck == 0){filtered <- AR2study_complete %>% filter(chnm == input$chemList)
        write.csv(filtered, file = "AR2_FilteredData1", row.names = FALSE)}
        else if(input$fullDataCheck == 1 && input$chemCheck == 0 && input$cypCheck == 0){filter <- AR2study_complete %>% select(7,10,13,11,14,15,19)
        write.csv(filter, file = "AR2_FilteredData2", row.names = FALSE)}
        else if(input$fullDataCheck == 1 && input$chemCheck == 1 && input$cypCheck == 0){filteredb <- AR2study_complete %>% select(7,10,13,11,14,15,19) %>% filter(chnm == input$chemList)
        write.csv(filteredb, file = "AR2_FilteredData3", row.names = FALSE)}
        else if(input$fullDataCheck == 1 && input$chemCheck == 1 && input$cypCheck == 1){filteredCyp <- AR2study_complete %>% select(7,10,13,11,14,15,19) %>% 
          filter(chnm == input$chemList) %>% filter(biogroup == input$cypList)
        write.csv(filteredCyp, file = "AR2_FilteredData4", row.names = FALSE)}
        else if(input$fullDataCheck == 0 && input$chemCheck == 1 && input$cypCheck == 1){filteredCyp2 <- AR2study_complete %>% filter(chnm == input$chemList) %>% filter(biogroup == input$cypList)
        write.csv(filteredCyp2, file = "AR2_FilteredData5", row.names = FALSE)}
        else if(input$fullDataCheck == 1 && input$chemCheck == 0 && input$cypCheck == 1){fullCypFilter <- AR2study_complete %>% select(7,10,13,11,14,15,19) %>% filter(biogroup == input$cypList)
        write.csv(fullCypFilter, file = "AR2_FilteredData6", row.names = FALSE)}
        else if(input$fullDataCheck == 0 && input$chemCheck == 0 && input$cypCheck == 1){cypFinal <- AR2study_complete %>% filter(biogroup == input$cypList)
        write.csv(cypFinal, file = "AR2_FilteredData7", row.names = FALSE)}
    }
    })
    
    output$plot <- renderPlot({
    
        ##########
        #Begin user input for plotting
        chemtitle.test = NULL
        chemtitle.test = input$chemName
        biogroup.test = input$biogroup
        
        sub.dt = AR2plotting[chnm == chemtitle.test & biogroup %in% biogroup.test,]
        
        dat_evan = sub.dt
        setkey(dat_evan, spid)
        dat_evan = dat_evan[,spid := as.factor(spid)]
        
        #Get hill parameters for each spid
        dat_hill = data.table(spid = unique(dat_evan[,spid]))
        
        for(chem in dat_hill[,spid]){
            pipefit = tcplFit_Lite(dat_evan[spid == chem,logc], dat_evan[spid == chem,resp], bmad)
            
            aic.vals = c(pipefit$hill_aic, pipefit$gnls_aic, pipefit$cnst_aic)
            
            hill_wtp = ifelse(min(aic.vals) == pipefit$hill_aic, pipefit$hill_tp, 
                              ifelse(min(aic.vals) == pipefit$gnls_aic, pipefit$gnls_tp, 0))
            hill_wga = ifelse(min(aic.vals) == pipefit$hill_aic, pipefit$hill_ga, 
                              ifelse(min(aic.vals) == pipefit$gnls_aic, pipefit$gnls_ga, 0))
            hill_wgw = ifelse(min(aic.vals) == pipefit$hill_aic, pipefit$hill_gw, 
                              ifelse(min(aic.vals) == pipefit$gnls_aic, pipefit$gnls_gw, 0))
            
            dat_hill[spid == chem, hill_tp := hill_wtp]
            dat_hill[spid == chem, hill_ga := hill_wga]
            dat_hill[spid == chem, hill_gw := hill_wgw]
        }
        
        #Create cyp color palette with grey controls
        #Make them responsive to user input, but retain same ordering always
        cyp.palette <- c("#808080", "#525252", "#FFBF00", "#DE3163",
                         "#008000", "#DCC715", "#6495ED", "#E67E22", 
                         "#008080", "#0000FF", "#000080", "#800080", "#15DAA1")
        groups = c("noRNA","Bgal","CYP1A2","CYP2A6","CYP2B6","CYP2C8","CYP2C9","CYP2C19","CYP2D6","CYP2E1","CYP2E1-WT","CYP2J2","CYP3A4")
        user.selected.biogroups = input$biogroup
        userPalette = cyp.palette[match(user.selected.biogroups, groups)]
        
        
        #Make AR Antagonist plots
        curves_plot <- ggplot(dat_evan, aes(x=logc, y=resp, color=spid)) +
            theme_bw() +
            geom_hline(yintercept=(4*bmad1), color="red", size=.8) +
            geom_point(size = 1.5) + 
            coord_cartesian(ylim=c(-25,125)) +
            ylab("% AR Inhibtion") +
            xlab("[cmpd] log(uM)") +
            ggtitle(dat_evan$chnm) +
            scale_colour_manual(values=userPalette, name = "Biogroup") +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=14),
                  axis.title = element_text(size=16),
                  legend.text = element_text(size=14),
                  legend.title = element_text(size=16, face = "bold"),
                  title = element_text(size=16, face = "bold"))
        
        #plot the fitted curves, color by spid
        k = 1L
        for(chem in dat_hill[,spid]){
            chemical_fit_hill_tp = dat_hill[spid == chem, hill_tp]
            chemical_fit_hill_ga = dat_hill[spid == chem, hill_ga]
            chemical_fit_hill_gw = dat_hill[spid == chem, hill_gw]
            curves_plot = curves_plot +
                stat_function(fun = hill_curve, args=list(hill_tp = chemical_fit_hill_tp, 
                                                          hill_ga = chemical_fit_hill_ga, 
                                                          hill_gw = chemical_fit_hill_gw),
                              color = userPalette[k], 
                              size=1) 
            k = k + 1L
        } 
        
        if(input$chemName %in% hitsData$Chemical){curves_plot_hit <- curves_plot  + 
            annotate("text", x = -3.1, y = 115, label = "AR Hit" , color="red", size=6, fontface="bold")
            curves_plot_hit}
        else if(input$chemName %in% c("BICAL")){curves_plot_cntrl <- curves_plot  + 
            annotate("text", x = -1.75, y = 115, label = "AR Control" , color="#F5B041", size=6, fontface="bold")
        curves_plot_cntrl}
        else if(input$chemName %in% c("DCLN")){curves_plot_cntrl2 <- curves_plot  + 
            annotate("text", x = -1.4, y = 115, label = "Viability Control" , color="#F5B041", size=6, fontface="bold")
        curves_plot_cntrl2}
        else{curves_plot}   
        
    }, width = 800, height = 400)
    
    
    
    output$ABplot <- renderPlot({
        
       if(input$Viability == 1){
         ##########
        #Begin user input for plotting
        chemtitle.test2 = NULL
        chemtitle.test2 = input$chemName
        biogroup.test2 = input$biogroup
        
        sub.dt2 = ABplotting[chnm == chemtitle.test2 & biogroup %in% biogroup.test2,]
        
        dat_evan2 = sub.dt2
        setkey(dat_evan2, spid)
        dat_evan2 = dat_evan2[,spid := as.factor(spid)]
        
        #Get hill parameters for each spid
        dat_hill = data.table(spid = unique(dat_evan2[,spid]))
        
        for(chem in dat_hill[,spid]){
            pipefit = tcplFit_Lite(dat_evan2[spid == chem,logc], dat_evan2[spid == chem,resp], bmad)
            
            aic.vals = c(pipefit$hill_aic, pipefit$gnls_aic, pipefit$cnst_aic)
            
            hill_wtp = ifelse(min(aic.vals) == pipefit$hill_aic, pipefit$hill_tp, 
                              ifelse(min(aic.vals) == pipefit$gnls_aic, pipefit$gnls_tp, 0))
            hill_wga = ifelse(min(aic.vals) == pipefit$hill_aic, pipefit$hill_ga, 
                              ifelse(min(aic.vals) == pipefit$gnls_aic, pipefit$gnls_ga, 0))
            hill_wgw = ifelse(min(aic.vals) == pipefit$hill_aic, pipefit$hill_gw, 
                              ifelse(min(aic.vals) == pipefit$gnls_aic, pipefit$gnls_gw, 0))
            
            dat_hill[spid == chem, hill_tp := hill_wtp]
            dat_hill[spid == chem, hill_ga := hill_wga]
            dat_hill[spid == chem, hill_gw := hill_wgw]
        }
        
        
        #Create cyp color palette with grey controls
        #Make them responsive to user input, but retain same ordering always
        cyp.palette <- c("#808080", "#525252", "#FFBF00", "#DE3163",
                         "#008000", "#DCC715", "#6495ED", "#E67E22", 
                         "#008080", "#0000FF", "#000080", "#800080", "#15DAA1")
        groups = c("noRNA","Bgal","CYP1A2","CYP2A6","CYP2B6","CYP2C8","CYP2C9","CYP2C19","CYP2D6","CYP2E1","CYP2E1-WT","CYP2J2","CYP3A4")
        user.selected.biogroups = input$biogroup
        userPalette = cyp.palette[match(user.selected.biogroups, groups)]
        
        #Make AR Viability plots
        curves_plot2 <- ggplot(dat_evan2, aes(x=logc, y=resp, color=spid)) +
            theme_bw() +
            geom_point(size = 1.5, shape = "triangle") + 
            coord_cartesian(ylim=c(-25,125)) +
            ylab("% Cyotoxicity") +
            xlab("[cmpd] log(uM)") +
            scale_colour_manual(values=userPalette, name = "Biogroup") +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=14),
                  axis.title = element_text(size=16),
                  legend.text = element_text(size=14),
                  legend.title = element_text(size=16, face = "bold"),
                  title = element_text(size=16, face = "bold"))
        
        k = 1L
        for(chem in dat_hill[,spid]){
            chemical_fit_hill_tp = dat_hill[spid == chem, hill_tp]
            chemical_fit_hill_ga = dat_hill[spid == chem, hill_ga]
            chemical_fit_hill_gw = dat_hill[spid == chem, hill_gw]
            curves_plot2 = curves_plot2 +
                stat_function(fun = hill_curve, args=list(hill_tp = chemical_fit_hill_tp, 
                                                          hill_ga = chemical_fit_hill_ga, 
                                                          hill_gw = chemical_fit_hill_gw),
                              color = userPalette[k], 
                              size=1) 
            k = k + 1L
        }
        
       curves_plot2 
       
        
        }}, width = 800, height = 400)
    
    
    
    #####################################
    ######Chemotype Data display#########
    
    #subsetChemos2 = "Chemotypes Present in Chemical List", "Full Chemotype Dataset", "Low-Dimensional Dataset"))
    
    output$hitsData <- renderDataTable({
        if(input$subsetChemos2 == "Chemotypes Present in Chemical List"){subsetChemotypes}
        else if(input$subsetChemos2 == "Full Chemotype Dataset"){chemotypes}
        else if(input$subsetChemos2 == "Low-Dimensional Dataset"){subsetChemotypesD}
    })
    
    output$displayChemos <- renderText({
      if(input$subsetChemos2 == "Chemotypes Present in Chemical List"){paste0("The number of chemotype predictors shown is: ", ncol(subsetChemotypes) - 2)}
      else if(input$subsetChemos2 == "Full Chemotype Dataset"){paste0("The number of chemotype predictors shown is: ", ncol(chemotypes) - 2)}
      else if(input$subsetChemos2 == "Low-Dimensional Dataset"){paste0("The number of chemotype predictors shown is: ", ncol(subsetChemotypesD) - 2)}
    })
    
    
    ##########################################
   ############################################
observeEvent(input$fitmodels, {    
    
    output$treeData <- renderTable({
        
      
      if(input$modeldata == "subsetChemotypesB$hitcall"){
        
        #Set fixed sampling 
        set.seed(18)
        
        #splitting data using createDataPartition from caret package
        index <- createDataPartition(y = subsetChemotypesB$hitcall, p = input$split, list = FALSE)
        training <- subsetChemotypesB[index,]
        testing <- subsetChemotypesB[-index,]
        ##Fit random forest model##
        rfFit2 <- randomForest(hitcall ~ ., data = training, mtry = (ncol(training)/3), ntree = 100, importance = TRUE)
        
        ##Random Forest prediction
        set.seed(18)
        rfPred2 <- predict(rfFit2, newdata = testing)
        b <- confusionMatrix(rfPred2, testing$hitcall)
        b1 <- data.table(b$table)
        accPre <- b$overall
        acc <- c("Accuracy", accPre[1])
        b2 <- b1 %>% rbind(acc, fill = TRUE)
        colnames(b2) = c("Prediction", "Reference", "Frequency", "Model Accuracy")
        
        b2
      }
      
      else if(input$modeldata == "chemotypeModel$hitcall"){
        
        #Set fixed sampling 
        set.seed(18)
        
        #splitting data using createDataPartition from caret package
        index <- createDataPartition(y = chemotypeModel$hitcall, p = input$split, list = FALSE)
        training <- chemotypeModel[index,]
        testing <- chemotypeModel[-index,]
        ##Fit random forest model##
        rfFit2 <- randomForest(hitcall ~ ., data = training, mtry = (ncol(training)/3), ntree = 100, importance = TRUE)
        
        ##Random Forest prediction
        set.seed(18)
        rfPred2 <- predict(rfFit2, newdata = testing)
        b <- confusionMatrix(rfPred2, testing$hitcall)
        b1 <- data.table(b$table)
        accPre <- b$overall
        acc <- c("Accuracy", accPre[1])
        b2 <- b1 %>% rbind(acc, fill = TRUE)
        colnames(b2) = c("Prediction", "Reference", "Frequency", "Model Accuracy")
        
        b2
      }
})

    
    output$treeDataPlot <- renderPlot({
      
      if(input$modeldata == "subsetChemotypesB$hitcall"){
      #Set fixed sampling 
      set.seed(18)
      
      #splitting data using createDataPartition from caret package
      index <- createDataPartition(y = subsetChemotypesB$hitcall, p = input$split, list = FALSE)
      training <- subsetChemotypesB[index,]
      testing <- subsetChemotypesB[-index,]
      
      ##Fit random forest model##
      rfFit2 <- randomForest(hitcall ~ ., data = training, mtry = (ncol(training)/3), ntree = 100, importance = TRUE)
      
      ##Random Forest prediction
      set.seed(18)
      rfPred2 <- predict(rfFit2, newdata = testing)
      b <- confusionMatrix(rfPred2, testing$hitcall)
      b1 <- data.table(b$table)
      accPre <- b$overall
      acc <- c("Accuracy", accPre[1])
      b2 <- b1 %>% rbind(acc, fill = TRUE)
      colnames(b2) = c("Prediction", "Reference", "Frequency", "Model Accuracy")
      
      #Look at variable importance plot
      var <- varImp(rfFit2)
      varOrdered = var[order(var$`1`, decreasing = TRUE), ]
      varPlot <- varOrdered[1:20,]
      names <- rownames(varPlot)
      varPlot2 <- cbind(names, varPlot)
      colnames(varPlot2) = c("Chemical", "Gini")
      rownames(varPlot2) = NULL
      varPlot2 = varPlot2[,-3]
      minV = min(varPlot2$Gini) - 0.25
      maxV = max(varPlot2$Gini) + 0.25
      
      g <- ggplot(varPlot2, aes(x = Gini, y = reorder(Chemical, Gini))) + geom_point(size = 3) + theme_bw() +
            scale_x_continuous(name = "Mean Dec. Gini", limits = c(minV, maxV)) + ggtitle("Chemotype Importance")+ ylab("")
      g
    }
      
    else if(input$modeldata == "chemotypeModel$hitcall"){
      #Set fixed sampling 
      set.seed(18)
      
      #splitting data using createDataPartition from caret package
      index <- createDataPartition(y = chemotypeModel$hitcall, p = input$split, list = FALSE)
      training <- chemotypeModel[index,]
      testing <- chemotypeModel[-index,]
      
      ##Fit random forest model##
      rfFit2 <- randomForest(hitcall ~ ., data = training, mtry = (ncol(training)/3), ntree = 100, importance = TRUE)
      
      ##Random Forest prediction
      set.seed(18)
      rfPred2 <- predict(rfFit2, newdata = testing)
      b <- confusionMatrix(rfPred2, testing$hitcall)
      b1 <- data.table(b$table)
      accPre <- b$overall
      acc <- c("Accuracy", accPre[1])
      b2 <- b1 %>% rbind(acc, fill = TRUE)
      colnames(b2) = c("Prediction", "Reference", "Frequency", "Model Accuracy")
      
      #Look at variable importance plot
      var <- varImp(rfFit2)
      varOrdered = var[order(var$`1`, decreasing = TRUE), ]
      varPlot <- varOrdered[1:20,]
      names <- rownames(varPlot)
      varPlot2 <- cbind(names, varPlot)
      colnames(varPlot2) = c("Chemical", "Gini")
      rownames(varPlot2) = NULL
      varPlot2 = varPlot2[,-3]
      minV = min(varPlot2$Gini) - 0.25
      maxV = max(varPlot2$Gini) + 0.25
      
      g <- ggplot(varPlot2, aes(x = Gini, y = reorder(Chemical, Gini))) + geom_point(size = 3) + theme_bw() +
        scale_x_continuous(name = "Mean Dec. Gini", limits = c(minV, maxV)) + ggtitle("Chemotype Importance")+ ylab("")
      g
      
      }
}, width = 600, height = 500)
    
    
    output$glm <- renderTable({
        
      
      if(input$modeldataGLM == "Low-Dimensional Chemotypes Dataset"){
        #Set fixed sampling 
        set.seed(18)
        
        #splitting data using createDataPartition from caret package
        index <- createDataPartition(y = subsetChemotypesE$hitcall, p = input$split, list = FALSE)
        training <- subsetChemotypesB[index,]
        testing <- subsetChemotypesB[-index,]
        
        #Fit Generalized linear model
        GLMfit <- glm(hitcall ~., data = training, family = "binomial") 
        GLMfit
        prediction <- data.frame(predict(GLMfit, newdata = testing, type = "response"))
        prediction <- prediction %>% mutate(testing$hitcall)
        colnames(prediction) = c("Probability", "Reference")
        prediction$Prediction <- ifelse(prediction$Probability <= 0.5, 0,1)
        as.factor(prediction$Reference)
        as.factor(prediction$Prediction)
        c <- data.table(postResample(prediction$Prediction, prediction$Reference))
        c2 <- pivot_wider(c, names_from = V1, values_from = V1)
        colnames(c2) = c("Accuracy", "Kappa")
        rownames(c2) = NULL
        c2
        }
    })
    
    ############Not including due to page fit constraints##########
    output$glmsummary <- renderTable({
      
      if(input$modeldata == "subsetChemotypesB$hitcall"){
        #Set fixed sampling 
        set.seed(18)
        
        #splitting data using createDataPartition from caret package
        index <- createDataPartition(y = subsetChemotypesE$hitcall, p = input$split, list = FALSE)
        training <- subsetChemotypesB[index,]
        testing <- subsetChemotypesB[-index,]
        
        #Fit Generalized linear model
        GLMfit <- glm(hitcall ~., data = training, family = "binomial") 
        GLMfit
        prediction <- data.frame(predict(GLMfit, newdata = testing, type = "response"))
        prediction <- prediction %>% mutate(testing$hitcall)
        colnames(prediction) = c("Probability", "Reference")
        prediction$Prediction <- ifelse(prediction$Probability <= 0.5, 0,1)
        as.factor(prediction$Reference)
        as.factor(prediction$Prediction)
        c <- data.table(postResample(prediction$Prediction, prediction$Reference))
        c2 <- pivot_wider(c, names_from = V1, values_from = V1)
        colnames(c2) = c("Accuracy", "Kappa")
        rownames(c2) = NULL
        
        
        data.frame(GLMfit$coefficients)
      }
      else if(input$modeldata == "chemotypeModel$hitcall"){
        #Set fixed sampling 
        set.seed(18)
        
        #splitting data using createDataPartition from caret package
        index <- createDataPartition(y = chemotypeModel$hitcall, p = input$split, list = FALSE)
        training <- chemotypeModel[index,]
        testing <- chemotypeModel[-index,]
        
        #Fit Generalized linear model
        GLMfit <- glm(hitcall ~., data = training, family = "binomial") 
        GLMfit
        prediction <- data.frame(predict(GLMfit, newdata = testing, type = "response"))
        prediction <- prediction %>% mutate(testing$hitcall)
        colnames(prediction) = c("Probability", "Reference")
        prediction$Prediction <- ifelse(prediction$Probability <= 0.5, 0,1)
        as.factor(prediction$Reference)
        as.factor(prediction$Prediction)
        c <- data.table(postResample(prediction$Prediction, prediction$Reference))
        c2 <- pivot_wider(c, names_from = V1, values_from = V1)
        colnames(c2) = c("Accuracy", "Kappa")
        rownames(c2) = NULL
        
        
        data.frame(GLMfit$coefficients)
      }
    })
    
    

    output$basictreeData <- renderTable({
        
        if(input$modeldata == "subsetChemotypesB$hitcall"){
            
            #Set fixed sampling 
            set.seed(18)
            
            #splitting data using createDataPartition from caret package
            index <- createDataPartition(y = subsetChemotypesB$hitcall, p = input$split, list = FALSE)
            training <- subsetChemotypesB[index,]
            testing <- subsetChemotypesB[-index,]
            ####
            
            #Classification Tree ---
            tree_fit <- train(hitcall ~ ., data = training,
                              method = "rpart", 
                              trControl = trainControl(method = "repeatedcv", number = input$cvnum, repeats = input$cvrep),
                              tuneGrid = data.frame(cp = seq(0,0.1,by = 0.001)))
            
            #Predict
            tree_pred <- predict(tree_fit, newdata = testing)
            #Evaluate predicted with observed
            treeacc <- postResample(tree_pred, testing$hitcall)
            #look at confusion matrix
            b <- confusionMatrix(tree_pred, testing$hitcall)
            b1 <- data.table(b$table)
            accPre <- b$overall
            acc <- c("Accuracy", accPre[1])
            b2 <- b1 %>% rbind(acc, fill = TRUE)
            colnames(b2) = c("Prediction", "Reference", "Frequency", "Model Accuracy")
            
            b2
        }
        
        else if(input$modeldata == "chemotypeModel$hitcall"){
            
            #Set fixed sampling 
            set.seed(18)
            
            #splitting data using createDataPartition from caret package
            index <- createDataPartition(y = chemotypeModel$hitcall, p = input$split, list = FALSE)
            training <- chemotypeModel[index,]
            testing <- chemotypeModel[-index,]
            ####
            
            #Classification Tree ---
            tree_fit <- train(hitcall ~ ., data = training,
                              method = "rpart", 
                              trControl = trainControl(method = "repeatedcv", number = input$cvnum, repeats = input$cvrep),
                              tuneGrid = data.frame(cp = seq(0,0.1,by = 0.001)))
            
            #Predict
            tree_pred <- predict(tree_fit, newdata = testing)
            #Evaluate predicted with observed
            treeacc <- postResample(tree_pred, testing$hitcall)
            #look at confusion matrix
            b <- confusionMatrix(tree_pred, testing$hitcall)
            b1 <- data.table(b$table)
            accPre <- b$overall
            acc <- c("Accuracy", accPre[1])
            b2 <- b1 %>% rbind(acc, fill = TRUE)
            colnames(b2) = c("Prediction", "Reference", "Frequency", "Model Accuracy")
            
            b2
        }
    })

})
    
    
    output$prediction <- renderText({
        
        #Set fixed sampling 
        set.seed(18)
        
        #splitting data using createDataPartition from caret package
        index <- createDataPartition(y = subsetChemotypesB$hitcall, p = .75, list = FALSE)
        training <- subsetChemotypesB[index,]
        testing <- subsetChemotypesB[-index,]
        
        if(input$modelpredict == "Random Forest"){
        ##Fit random forest model##
        rfFit2 <- randomForest(hitcall ~ ., data = training, mtry = (ncol(training)/3), ntree = 100, importance = TRUE)
        
        ##Random Forest prediction
        set.seed(18)
        
        value <- as.numeric(input$predictChems)
        rfPred2 <- predict(rfFit2, newdata = data.frame(sample_chemotypes[value,]))
        rfPred2 <- data.frame(rfPred2)
        colnames(rfPred2) = "Prediction"
        rfPred3 <- if(rfPred2$Prediction == 0){rfPred3 = "Non-hit"} else if(rfPred2$Prediction == 1){rfPred3 = "Hit"}
        print(rfPred3)}
        
        else if(input$modelpredict == "Classification Tree"){

        #Classification Tree ---
        tree_fit <- train(hitcall ~ ., data = training,
                              method = "rpart", 
                              trControl = trainControl(method = "repeatedcv", number = 3, repeats = 2),
                              tuneGrid = data.frame(cp = seq(0,0.1,by = 0.001)))
            
        set.seed(18)
        
        value <- as.numeric(input$predictChems)
        treePred2 <- predict(tree_fit, newdata = data.frame(sample_chemotypes[value,]))
        treePred2 <- data.frame(treePred2)
        colnames(treePred2) = "Prediction"
        treePred3 <- if(treePred2$Prediction == 0){treePred3 = "Non-hit"} else if(treePred2$Prediction == 1){treePred3 = "Hit"}
        print(treePred3)}
        
        
    })
    
    
    
    
    
    
    
    ####Chem images and descriptions on prediction tab#####
    
    output$chemDes <- renderText({
        if(input$predictChems == 1){"2,6-Dichlorobenzonitrile is an herbicide commonly used to control weeds in gardens, lawns, near ornamental trees, and various other settings. It also controls aquatic (water) weeds such as cattail and purple loosestrife. Dichlobenil stops seed germination, cellulose (cell wall) formation, and growth in plant roots and shoots."}
        else if(input$predictChems == 2){"Faslodex is a prescription medicine used to treat advanced breast cancer or breast cancer that has spread to other parts of the body (metastatic)."}
        else if(input$predictChems == 3){"4,4'-((1H-1,2,4-triazol-1-yl)methylene)dibenzonitrile  is an aromatase inhibitor used in the treatment of breast cancer."}
        else if(input$predictChems == 4){"2,2,4,4,6,6,8,8-Octamethyl-1,3,5,7,2,4,6,8-tetraoxatetrasilocane is an industrial chemical used as a monomer in the manufacture of polymeric materials, which are widely used in various industrial and/or medical applications, such as breast implants."}
        else if(input$predictChems == 5){"Ronilan is a fungicide commonly used in the wine industry."}
        
    })
    
    output$chemImage <- renderImage({
       if(input$predictChems == 1){list(src = 'www/dichlobenil.JPG')}
       else if(input$predictChems == 2){list(src = 'www/faslodex.JPG')}
        else if(input$predictChems == 3){list(src = 'www/letro.JPG')}
        else if(input$predictChems == 4){list(src = 'www/oct1.JPG')}
        else if(input$predictChems == 5){list(src = 'www/ronilan.JPG')}  
        
    }, deleteFile = FALSE)
    
    
})
####END########################################################################




























