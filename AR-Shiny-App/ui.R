# This is the user-interface definition of a Shiny web application. You can

library(tidyverse)
library(data.table)
library(shiny)
library(shinythemes)

#UI
shinyUI(navbarPage("AR2 Assay Data App", theme = shinytheme("flatly"),
                   
                   #Introduction Panel
                   tabPanel("Introduction",
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
                      
                      h1("Introducion to the AR2 Assay"),
                      h3("Background"),
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
                        collecting data on new chemicals going forward."),
                      p("The Simmons Lab team stepped in and began developing replacement assays in 2020 that have now been completed and validated. This Shiny App is a uder friendly way to explore our validation data for the Androgen 
                        Receptor Antagonist protein homodimerization assay. The reference chemical set used contained 128 chemicals enriched for endocrine response and may impact the modeling results. ")
                      
                      
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


