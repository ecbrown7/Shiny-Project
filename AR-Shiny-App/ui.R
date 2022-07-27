# This is the user-interface definition of a Shiny web application. You can

library(tidyverse)
library(data.table)
library(shiny)
library(shinythemes)

#UI
shinyUI(navbarPage("AR2 Assay Data App", theme = shinytheme("flatly"),
                   tabPanel("Introduction",
                            titlePanel("Introduction to AR2 Assay"),
                    sidebarLayout(
                    sidebarPanel("Description"),
                    mainPanel())),
                   
                   tabPanel("Data Exploration",
                            titlePanel("Exploring Study Data"),
                     sidebarLayout(
                     sidebarPanel(
                     selectInput(
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
                       "Trifluralin","Vinclozolin","Z-Tetrachlorvinphos" ,"Zearalenone","o,p'-DDT","p,p'-DDD","p,p'-DDE","p-Dichlorobenzene")),
                     selectInput(
                       "cypList",
                       "Select Biogroup: ",
                       choices = c("noRNA","Bgal","CYP1A2","CYP2A6","CYP2B6","CYP2C8","CYP2C9","CYP2C19","CYP2D6","CYP2E1","CYP2E1-WT","CYP2J2","CYP3A4"))),
                     mainPanel(dataTableOutput("filteredData")))),
        
                   tabPanel("Data Visualization",
                            titlePanel("AR Antagonist Plots"),
                            sidebarLayout(
                              sidebarPanel("Description"),
                              mainPanel())),
                   
                   tabPanel("Modeling",
                            titlePanel("Chemotype Modeling for AR2 Assay Hits"),
                      sidebarLayout(
                      sidebarPanel("Description"),
                      mainPanel()))
))


