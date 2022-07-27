library(data.table)
library(ggplot2)

# read in dpid_128 chem plate map#
dpid.128 = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/validation_chem.csv", stringsAsFactors = F, skipNul = T)
dpid.128 = as.data.table(dpid.128)

# read in control plate map#
control = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/antagonist_control_chem.csv", stringsAsFactors = F, skipNul = T)
control = as.data.table(control)

# read in cohort dispense maps#
dispmap1 = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/valid_ar_antag_cohort_1.map.csv", stringsAsFactors = F, skipNul = T)
dispmap1$cohort = 1
dispmap2 = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/valid_ar_antag_cohort_2.map.csv", stringsAsFactors = F, skipNul = T)
dispmap2$cohort = 2
dispmap3 = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/valid_ar_antag_cohort_3.map.csv", stringsAsFactors = F, skipNul = T)
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
filemap = read.csv("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/AR_Antagonist128_Met_filemap.csv", stringsAsFactors = F, skipNul = T)
filemap = data.table(filemap)

### map data data ###
filemap = filemap[!TR %in% 2874:2913,]

study = NULL
for (j in 1:nrow(filemap)){
  
  file.name = paste("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/data/TRno", filemap$TR[j], ".CSV", sep = "")
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


#plot dose-response AR data#

source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/tcplFit.R")
source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/tcpl_Fit_Lite_Sample_Data.R")
source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/tcplObjCnst.R")
source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/tcplObjHill.R")
source("C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/tcplObjGnls.R")

hill_curve = function(hill_tp, hill_ga, hill_gw, lconc){
  return(hill_tp/(1+10^((hill_ga - lconc)*hill_gw)))
}
##########
#Sample data is a data frame with 5 chemicals. Change to data table, then loop over chemical name through tcplFit
#tcplFit needs something called bmad, but it doesn't impact the results, 
#but can limit the number of curves attempted to be fit, so set smaller than you think it really is
bmad = .001
##########

AR2 = cmpd1[assay == "AR",]
AR2$spid = AR2$biogroup
AR2$resp = AR2$nval

for (i in 1:length(unique(AR2$chnm))){
  for (m in 3:length(unique(AR2$biogroup))){
  
  chemtitle = NULL
  chemtitle = unique(AR2$chnm)[i]
  
  sub.dt = AR2[chnm == unique(AR2$chnm)[i] & biogroup %in% unique(AR2$biogroup)[c(1,2,m)],]
  #sub.dt = ar2.by.chnm[chnm == unique(ar2.by.chnm$chnm)[i] & biogroup %in% unique(biogroups[c(1,2,j)]),]
  
  dat_steve = sub.dt
  setkey(dat_steve, spid)
  dat_steve = dat_steve[,spid := as.factor(spid)]
  
  #Get hill parameters for each spid
  dat_hill = data.table(spid = unique(dat_steve[,spid]))
  
  for(chem in dat_hill[,spid]){
    pipefit = tcplFit_Lite(dat_steve[spid == chem,logc], dat_steve[spid == chem,resp], bmad)
    
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
  
  # The palette with black:
  cbbPalette = c("#000000", "#0000ff", "#888888")#rainbow(6)[1:5]) 
  
  curves_plot = ggplot(dat_steve, aes(x=logc, y=resp, color = spid))
  
  #plot the fitted curves, color by spid. This code may need to be modified if you have more colors than in cbbPalette
  k = 1L
  for(chem in dat_hill[,spid]){
    chemical_fit_hill_tp = dat_hill[spid == chem, hill_tp]
    chemical_fit_hill_ga = dat_hill[spid == chem, hill_ga]
    chemical_fit_hill_gw = dat_hill[spid == chem, hill_gw]
    curves_plot = curves_plot +
      stat_function(fun = hill_curve, args=list(hill_tp = chemical_fit_hill_tp, 
                                                hill_ga = chemical_fit_hill_ga, 
                                                hill_gw = chemical_fit_hill_gw),
                    color = cbbPalette[k], 
                    size=1) 
    k = k + 1L
  }
  
  #dat_hill$AC50.uM = ifelse(pipefit$max_med > threshold, round(10^dat_hill$hill_ga, digits = 3), NA)
  
  #ac50.row.basal = data.frame(spid = unique(dt2$spid)[i], chmn = unique(dt2$chmn[dt2$spid == unique(dt2$spid)[i]]), 
  #method = paste("OCR", methods[j], sep = "."), AC50.uM = dat_hill$AC50.uM)
  
  #ac50.table.basal = rbind(ac50.table.basal, ac50.row.basal)
  
  #spid.ac50 = ifelse(is.na(dat_hill$AC50.uM), "AC50 = NA", paste("AC50 = ", dat_hill$AC50.uM, "uM", sep = ""))
  
  curves_plot = curves_plot +
    theme_bw() +
    geom_point(size = 2.5) + 
    #geom_hline(yintercept = threshold, color = "#ff0000", linetype = 5) +
    coord_cartesian(ylim=c(-25,125))+
    scale_colour_manual(values=cbbPalette, name = "Biogroup") +
    ylab("% AR Inhibtion") +
    xlab("[cmpd] (log uM)") +
   # geom_text(aes(x = min(dat_steve$logc + 0.5), y = -40, label = spid.ac50), size = 3, color = "#000000", show.legend = FALSE) +
    ggtitle(chemtitle) + theme (axis.title=element_text(size=14,face="bold"),
                                plot.title=element_text(size=18,face="bold", hjust = 0.5)) 
  
  
  mytheme = gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1)),
    colhead = list(fg_params=list(cex = 1)),
    rowhead = list(fg_params=list(cex = 0)))
  
  #curves_plot = curves_plot + annotation_custom(tableGrob(dat_summ , theme = mytheme), 
  #xmin=0, xmax=0.6, ymin=100, ymax=150)
  
  file.name = paste(unique(AR2$chnm)[i], unique(AR2$biogroup)[m],"AR2", "png", sep = ".")
  file.path = "C:/Users/Ebrown08/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/AR_Shiny/plots"
  ggsave(plot = curves_plot, filename = file.name, 
         path = file.path)
  
  }
}


### plot dose-response Alamar Blue data ###
AB = cmpd1[assay == "Viability",]
AB$spid = AB$biogroup
AB$resp = AB$nval

for (i in 1:length(unique(AB$chnm))){
  for (m in 3:length(unique(AB$biogroup))){
    
    chemtitle = NULL
    chemtitle = unique(AB$chnm)[i]
    
    sub.dt = AB[chnm == unique(AB$chnm)[i] & biogroup %in% unique(AB$biogroup)[c(1,2,m)],]
    #sub.dt = ar2.by.chnm[chnm == unique(ar2.by.chnm$chnm)[i] & biogroup %in% unique(biogroups[c(1,2,j)]),]
    
    dat_steve = sub.dt
    setkey(dat_steve, spid)
    dat_steve = dat_steve[,spid := as.factor(spid)]
    
    #Get hill parameters for each spid
    dat_hill = data.table(spid = unique(dat_steve[,spid]))
    
    for(chem in dat_hill[,spid]){
      pipefit = tcplFit_Lite(dat_steve[spid == chem,logc], dat_steve[spid == chem,resp], bmad)
      
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
    
    # The palette with black:
    cbbPalette = c("#000000", "#ff0000", "#888888")#rainbow(6)[1:5]) 
    
    curves_plot = ggplot(dat_steve, aes(x=logc, y=resp, color = spid))
    
    #plot the fitted curves, color by spid. This code may need to be modified if you have more colors than in cbbPalette
    k = 1L
    for(chem in dat_hill[,spid]){
      chemical_fit_hill_tp = dat_hill[spid == chem, hill_tp]
      chemical_fit_hill_ga = dat_hill[spid == chem, hill_ga]
      chemical_fit_hill_gw = dat_hill[spid == chem, hill_gw]
      curves_plot = curves_plot +
        stat_function(fun = hill_curve, args=list(hill_tp = chemical_fit_hill_tp, 
                                                  hill_ga = chemical_fit_hill_ga, 
                                                  hill_gw = chemical_fit_hill_gw),
                      color = cbbPalette[k], 
                      size=1) 
      k = k + 1L
    }
    
    dat_hill$AC50.uM = ifelse(pipefit$max_med > threshold, round(10^dat_hill$hill_ga, digits = 3), NA)
    
    #ac50.row.basal = data.frame(spid = unique(dt2$spid)[i], chmn = unique(dt2$chmn[dt2$spid == unique(dt2$spid)[i]]), 
    #method = paste("OCR", methods[j], sep = "."), AC50.uM = dat_hill$AC50.uM)
    
    #ac50.table.basal = rbind(ac50.table.basal, ac50.row.basal)
    
    #spid.ac50 = ifelse(is.na(dat_hill$AC50.uM), "AC50 = NA", paste("AC50 = ", dat_hill$AC50.uM, "uM", sep = ""))
    
    curves_plot = curves_plot +
      theme_bw() +
      geom_point(size = 2.5) + 
      #geom_hline(yintercept = threshold, color = "#ff0000", linetype = 5) +
      coord_cartesian(ylim=c(-25,125))+
      scale_colour_manual(values=cbbPalette, name = "Biogroup") +
      ylab("% Cytotoxicity") +
      xlab("[cmpd] (log uM)") +
      geom_text(aes(x = min(dat_steve$logc + 0.5), y = -40, label = spid.ac50), size = 3, color = "#000000", show.legend = FALSE) +
      ggtitle(chemtitle) + theme (axis.title=element_text(size=14,face="bold"),
                                  plot.title=element_text(size=18,face="bold", hjust = 0.5)) 
    
    
    mytheme = gridExtra::ttheme_default(
      core = list(fg_params=list(cex = 1)),
      colhead = list(fg_params=list(cex = 1)),
      rowhead = list(fg_params=list(cex = 0)))
    
    #curves_plot = curves_plot + annotation_custom(tableGrob(dat_summ , theme = mytheme), 
    #xmin=0, xmax=0.6, ymin=100, ymax=150)
    
    file.name = paste(unique(AB$chnm)[i], unique(AB$biogroup)[m],"AB", "png", sep = ".")
    file.path = "C:/Users/Ssimmons/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/Projects/NanoBit-NRs/AR/ARAntagonist_Metabolism_Validation/plots/Alamar_Blue"
    ggsave(plot = curves_plot, filename = file.name, 
           path = file.path)
    
  }
}
