# This is the user-interface definition of a Shiny web application. You can

library(tidyverse)
library(data.table)
library(shiny)
library(shinythemes)

#UI
shinyUI(navbarPage("AR2 Assay Data App", theme = shinytheme("flatly"),
                   
                   #Introduction Panel
                   tabPanel("About",
                            titlePanel("Introduction to AR2 Assay"),
                    sidebarLayout(
                    sidebarPanel(
                      strong(h5("More Information")),
                      h6(HTML("<p><a href='https://www.epa.gov/endocrine-disruption/what-endocrine-system'> Endocrine Disrupting Chemicals </a>")),
                      h6(HTML("<p><a href='https://www.epa.gov/endocrine-disruption/endocrine-disruptor-screening-program-edsp-overview'> EPA EDSP </a>")),
                      h6(HTML("<p><a href='https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=351000&Lab=NCCT'> AR Model </a>")),
                      h6(HTML("<p><a href='https://pubmed.ncbi.nlm.nih.gov/29555536/'> Metabolism Retrofit Development </a>")),
                      
                      width = 3),
                    mainPanel(
                      
                      h3("Purpose"),
                      p("This Shiny App is a user friendly way to explore real-life assay validation data for a novel Androgen Receptor (AR) chemical screening assay (AR2 assay) developed at the US Environmental Protection Agency. There are 
                        three pages aside from this introduction, each with a distinct purpose."),
                      p("The Data Exploration page will allow you to scroll through the complete data set or view a selected columns view that can present clearer. 
                        In addition, this page will allow you to filter either data set by chemical name, biogroup, or both. Finally, this page gives you the ability to save all subsets of data as a CSV file on your local machine."),
                      p("The Data Visualization page is a place to view plots of the data. Both AR assay and paralleled cell viability plots are shown by default, but you'll be able to select AR only if desired. This page also allows 
                        you to filter plots by the same metrics as fiound in the Data Exploration page. Curves for these plots were fit using the hill curve function and hits in the AR assay were established using 4*bmad threshold, which is 
                        shown on the plot. Final hit calls, made to distinguish AR specific chemicals from purely cytotoxic chemicals, were made using two criteria (1. Hit in AR assay, 2. deltaAC50 from AR assay to cell viability assay > 1.). 
                        The final hit calls were then recorded and mapped to a dataset comprising the chemotypes for each chemical tested."),
                      p("The Modeling page implements the chemotype-hitcall dataset to predict androgen receptor disrupting chemicals (AR antagonists) from a potential series of 739 chemotype predictors. An example of a predictor 
                        is a benzene ring, but these can be wide ranging and occosionally repetitive. This dataset can be viewed in the Data tab within the Modeling page. The goal of this page is two-fold. 
                        First, you'll be able to visualize through variable importance plots which variables were most important to predicting AR hits. Knowing particular active chemotypes would be particularly useful to 
                        know when looking at future chemicals to flag for further investigation. Second, this page attempts to allow the user to input a specific chemotype and allow the model to predict that chemotype for AR toxicity."),
                      
                      strong(h3("Further Background")),
                      p("In the 1990's, some scientists proposed that certain chemicals might be disrupting the endocrine systems of humans and wildlife. At the time, only a handful of chemicals had been found to disrupt the endocrine 
                        systems of animals in laboratory studies, but compelling evidence showed that endocrine systems of certain fish and wildlife had been affected by chemical contaminants, 
                        resulting in developmental and reproductive problems. Based on this and other evidence, congress passed the Food Quality and Protection Act (FQPA) that requires EPA to screen pesticide chemicals 
                        for their potential to produce effects similar to those by the female hormones (estrogen) in humans. Fast forward several years and those chemicals become of ever increasing concern and are labeled
                        Endocrine Disrupting Chemicals (EDCs). A tremendous amount of progress was made during those years in the early 21st century, but the mountain of over 10,000 chemicals in commerce that have still evaded 
                        thorough evaluation to this day remained an unsolved challenge. In 2015, EPA announced the use of cutting-edge technology including high-throughput animal-free assays and integrated computational
                         models that would shift the approach from a reactive chemical assessment standpoint, to a predictive one."),
                      p("To this day, there are many laboratories including the Simmons Laboratory at EPA in Research Triangle Park that work to develop new endocrine high throughput assays to provide data streams 
                        for the computational model in order to improve prediction. There are three major endocrine pathways of interest, the Androgen pathway, the Estrogen pathway, and the Thyroid pathway. 
                        While the Estrogen model was first to gain exposure, the Androgen model is close behind and of equivalent importance. The Androgen model is build on the back of four indespensible assay types, 
                        one of which being protein homodimerization assays. When several of the streams of data for this assay type became unavailable, the computational modeling team needed an internalized replacement for 
                        collecting data on new chemicals going forward. This is where our laboratory stepped in and began developing replacement assays in 2020 that have now been completed and validated."),
                      
                      strong(h3("Footnote on Biogroups and Assay Specifics")),
                      p("Throughout this app, you will be able to sort data and view data based on biogroups. This assay was developed using the xenobiotic metabolism retrofit method (linked in sidebar). Xenobiotic metabolism is responsible for 
                        rending toxic parent substances into non-toxic metabolites and occurs mainly in your liver. There in the human liver there are over 50 enzymes named Cytochrome p450 enzymes (cyp's) that contribute to this 
                        physiological process. In our assay, we make use of the 11 most prevelant human cyp's and as you filter plots by cyp, you will be able to notice a shifted curve in comparison to our two controls, noRNA and Bgal. 
                        To observe this shift the best and understand the purpose of adding metbaolism to our assay, filter the plots page to chemical = Flutamide, and check biogroup = 1A2 and 2C19. To view plots for our AR assay control, 
                        filter the plots page to chemical = BICAL. To view plots for our viability assay control, filter the plots page to chemical - DCLN. It should be noted that the chemicals tested here are part of an endocrine validation set that
                         is enriched for pro and anti-endocrine function, meaning most chemicals will either be clearly positive or clearly negative, and selected for this purpose. As a result, some of the variable importance conlusions should be 
                        drawn carefully so as not to overgeneralize these results to a slew of unknown chemicals."),
                      br(),
                      br()
                      
                    ))),
                   
                   
                   #Data Exploration Panel
                   tabPanel("Data Exploration",
                            titlePanel("Exploring Study Data"),
                     sidebarLayout(
                     sidebarPanel(strong("Filter Options"),
                       
                     checkboxInput("chemCheck", "Filter by Chemical?", value = FALSE),
                     conditionalPanel(condition = "input.chemCheck == 1", selectInput(
                       "chemList",
                       "Select Chemical: ",
                       choices = c("17-Methyltestosterone", "17alpha-Estradiol", "17alpha-Ethinylestradiol","17beta-Estradiol", "17beta-Trenbolone",
                       "2,2',4,4'-Tetrahydroxybenzophenone","2,4-Dihydroxybenzophenone","2,4-Dinitrophenol","2-Ethylhexylparaben","4,4'-Sulfonyldiphenol",
                       "4-(1,1,3,3-Tetramethylbutyl)phenol","4-(2-Methylbutan-2-yl)phenol","4-Androstene-3,17-dione","4-Cumylphenol","4-Dodecylphenol",
                       "4-Hydroxybenzoic acid","4-Nonylphenol","4-tert-Butylphenol","5alpha-Dihydrotestosterone","Abamectin","Acephate","Afimoxifene (4-Hydroxytamoxifen)",
                       "Amitrole","Anastrozole","Apigenin","Atrazine","BICAL","Benfluralin","Benomyl","Bicalutamide","Bifenthrin","Bis(2-ethylhexyl)hexanedioate",
                       "Bis(2-ethylhexyl)phthalate","Bisphenol A","Bisphenol AF","Bisphenol B","Butylbenzylphthalate","Butylparaben","Carbendazim","Carbofuran",
                       "Chlorothalonil","Chlorpyrifos","Chlorpyrifos-methyl","Clomiphene citrate","Corticosterone","Coumestrol ","Cyfluthrin","Cypermethrin",
                       "Cyproterone acetate","DCLN","Daidzein ","Danazol","Deltamethrin","Diazinon","Dibutyl phthalate","Dichlobenil",
                       "Dichlorodiphenyltrichloroethane","Dicyclohexyl phthalate","Diethyl phthalate","Diethylstilbestrol","Dihexyl phthalate","Dipentyl phthalate",
                       "Enclomiphene hydrochloride","Equilin","Esfenvalerate","Estrone","Ethoprop","Ethylparaben","Exemestane","Fenarimol","Fenitrothion","Fenthion",
                       "Fenvalerate","Finasteride","Flutamide","Flutolanil","Folpet","Formestane","Fulvestrant","Genistein","Hydroxyflutamide",
                       "Hydroxyprogesterone caproate","Iprodione","Kaempferol","Letrozole","Levonorgestrel","Linuron","Malathion","Mestranol","Metalaxyl","Methomyl",
                       "Methoxychlor","Metolachlor","Metribuzin","Mifepristone","Mono(2-ethylhexyl) phthalate","Naringenin","Nilutamide","Norethindrone","Norflurazon",
                       "Octamethylcyclotetrasiloxane","Octylbicycloheptenedicarboximide","Oxamyl","Pentachloronitrobenzene","Pentachlorophenol","Permethrin",
                       "Phenothrin","Phosmet","Prochloraz","Procymidone","Propargite","Propiconazole","Propyzamide","Raloxifene hydrochloride","Simazine",
                       "Spironolactone","Tamoxifen","Tebuconazole","Testosterone propionate","Tetramethrin","Triadimefon","Tributylchlorostannane",
                       "Trifluralin","Vinclozolin","Z-Tetrachlorvinphos" ,"Zearalenone","o,p'-DDT","p,p'-DDD","p,p'-DDE","p-Dichlorobenzene"))),
                     
                     checkboxInput("cypCheck", "Filter by Biogroup?", value = FALSE),
                     conditionalPanel(condition = "input.cypCheck == 1", selectInput("cypList","Select Biogroup: ",
                       choices = c("noRNA","Bgal","CYP1A2","CYP2A6","CYP2B6","CYP2C8","CYP2C9","CYP2C19","CYP2D6","CYP2E1","CYP2E1-WT","CYP2J2","CYP3A4"))),
                     
                     checkboxInput("fullDataCheck", p("Display Key Columns Only", style = "color:blue"), value = FALSE),
                     actionButton("saveButton", strong("Save Dataset as .CSV")), width = 3),
                     
                     mainPanel(dataTableOutput("filteredData")))),
        
                   
                   
                   #Data Visualization Panel
                   tabPanel("Data Visualization",
                            titlePanel("AR Antagonist Plots"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("chemName","Select Chemical: ",
                                choices = c("17-Methyltestosterone", "17alpha-Estradiol", "17alpha-Ethinylestradiol","17beta-Estradiol", "17beta-Trenbolone",
                                    "2,2',4,4'-Tetrahydroxybenzophenone","2,4-Dihydroxybenzophenone","2,4-Dinitrophenol","2-Ethylhexylparaben","4,4'-Sulfonyldiphenol",
                                    "4-(1,1,3,3-Tetramethylbutyl)phenol","4-(2-Methylbutan-2-yl)phenol","4-Androstene-3,17-dione","4-Cumylphenol","4-Dodecylphenol",
                                    "4-Hydroxybenzoic acid","4-Nonylphenol","4-tert-Butylphenol","5alpha-Dihydrotestosterone","Abamectin","Acephate","Afimoxifene (4-Hydroxytamoxifen)",
                                    "Amitrole","Anastrozole","Apigenin","Atrazine","BICAL","Benfluralin","Benomyl","Bicalutamide","Bifenthrin","Bis(2-ethylhexyl)hexanedioate",
                                    "Bis(2-ethylhexyl)phthalate","Bisphenol A","Bisphenol AF","Bisphenol B","Butylbenzylphthalate","Butylparaben","Carbendazim","Carbofuran",
                                    "Chlorothalonil","Chlorpyrifos","Chlorpyrifos-methyl","Clomiphene citrate","Corticosterone","Coumestrol ","Cyfluthrin","Cypermethrin",
                                    "Cyproterone acetate","DCLN","Daidzein ","Danazol","Deltamethrin","Diazinon","Dibutyl phthalate","Dichlobenil",
                                    "Dichlorodiphenyltrichloroethane","Dicyclohexyl phthalate","Diethyl phthalate","Diethylstilbestrol","Dihexyl phthalate","Dipentyl phthalate",
                                    "Enclomiphene hydrochloride","Equilin","Esfenvalerate","Estrone","Ethoprop","Ethylparaben","Exemestane","Fenarimol","Fenitrothion","Fenthion",
                                    "Fenvalerate","Finasteride","Flutamide","Flutolanil","Folpet","Formestane","Fulvestrant","Genistein","Hydroxyflutamide",
                                    "Hydroxyprogesterone caproate","Iprodione","Kaempferol","Letrozole","Levonorgestrel","Linuron","Malathion","Mestranol","Metalaxyl","Methomyl",
                                    "Methoxychlor","Metolachlor","Metribuzin","Mifepristone","Mono(2-ethylhexyl) phthalate","Naringenin","Nilutamide","Norethindrone","Norflurazon",
                                    "Octamethylcyclotetrasiloxane","Octylbicycloheptenedicarboximide","Oxamyl","Pentachloronitrobenzene","Pentachlorophenol","Permethrin",
                                    "Phenothrin","Phosmet","Prochloraz","Procymidone","Propargite","Propiconazole","Propyzamide","Raloxifene hydrochloride","Simazine",
                                    "Spironolactone","Tamoxifen","Tebuconazole","Testosterone propionate","Tetramethrin","Triadimefon","Tributylchlorostannane",
                                    "Trifluralin","Vinclozolin","Z-Tetrachlorvinphos" ,"Zearalenone","o,p'-DDT","p,p'-DDD","p,p'-DDE","p-Dichlorobenzene")),
                              
                               checkboxGroupInput("biogroup","Select Biogroup: ",
                                        choices = c("noRNA","Bgal","CYP1A2","CYP2A6","CYP2B6","CYP2C8","CYP2C9","CYP2C19","CYP2D6","CYP2E1","CYP2E1-WT","CYP2J2","CYP3A4"), selected = c("noRNA", "Bgal")),
                            
                              checkboxInput("Viability", strong("Show Viability Plots"), value = TRUE), width = 4),
                              
                              mainPanel(plotOutput("plot"), plotOutput("ABplot")))),
                   
                   
                   
                  
                   #Modeling Panel
                   tabPanel("Modeling",
                            titlePanel("Chemotype Modeling for AR2 Assay"),
                      sidebarLayout(
                      sidebarPanel(
                        sliderInput("splitProp", strong("Set Proportion of Data to Train On"), min = 0.4, max = 0.85, 0.7, step = 0.05, ticks = FALSE)
                      , width = 3),
                      
                      mainPanel(tabsetPanel(type = "tabs",
                                            tabPanel("Modeling Info"),
                                            tabPanel("Model Fitting", tableOutput("treeData"), plotOutput("treeDataPlot")),
                                            tabPanel("Prediction"),
                                            tabPanel("Chemotype Data", dataTableOutput("hitsData"))))
))))


