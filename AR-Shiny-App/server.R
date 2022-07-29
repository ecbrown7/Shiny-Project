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

########For hit calls - #gives the global, zero-centered (median = 0) mad for DMSO
bmad1 = study[assay == "AR" & chnm == "DMSO", mad(nval)] 
threshold = 6*bmad1
med.resp.by.chnm.dose.biogroup = study[assay == "AR" & chnm != "DMSO", .(med.resp = median(nval)), by = .(chnm, logc, biogroup)]
hits.all.logc.and.biogroups = med.resp.by.chnm.dose.biogroup[med.resp > threshold,]
unique.hits.antagonist = data.table(unique(hits.all.logc.and.biogroups$chnm))
########

cmpd1 = study[!is.na(logc),]
#Finalized data set
AR2study_complete = cmpd1[assay == "AR",]

#plot dose-response AR data#
#Source code written to plot hill curves through EPA offline pipeline
source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcplFit.R")
source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcpl_Fit_Lite_Sample_Data.R")
source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcplObjCnst.R")
source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcplObjHill.R")
source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/tcplObjGnls.R")

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
chemotypes = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ST558/Shiny-Project/ar_shiny_data/ref_chemotypes.csv", stringsAsFactors = F, skipNul = T)
colnames(chemotypes)[1] = "Chemical"
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
            geom_hline(yintercept=(6*bmad1), color="red", size=1) +
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
        
        if(input$chemName %in% unique.hits.antagonist$V1){curves_plot_hit <- curves_plot  + 
            annotate("text", x = -3, y = 95, label = "Hit" , color="red", size=7, fontface="bold")
            curves_plot_hit}
        else{curves_plot}   
        
    }, width = 800, height = 425)
    
    
    
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
        
        }}, width = 800, height = 425)
})










































    
    
    
    
        




