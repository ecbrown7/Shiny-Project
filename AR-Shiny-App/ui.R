# This is the user-interface definition of a Shiny web application. You can

library(tidyverse)
library(data.table)
library(shiny)
library(shinythemes)
library(mathjaxr)

#UI
shinyUI(navbarPage("AR2 Assay Data App", theme = shinytheme("flatly"),
                   
                   #Introduction Panel
                   tabPanel("About",
                            titlePanel("Introduction to AR2 Assay"),
                    sidebarLayout(
                    sidebarPanel(
                      h4(strong("More Information")),
                      h5(HTML("<p><a href='https://www.epa.gov/endocrine-disruption/what-endocrine-system'> Endocrine Disrupting Chemicals </a>")),
                      h5(HTML("<p><a href='https://www.epa.gov/endocrine-disruption/endocrine-disruptor-screening-program-edsp-overview'> EPA EDSP </a>")),
                      h5(HTML("<p><a href='https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=351000&Lab=NCCT'> AR Model </a>")),
                      h5(HTML("<p><a href='https://pubmed.ncbi.nlm.nih.gov/29555536/'> Metabolism Retrofit Development </a>")),
                      br(),
                      br(),
                      em("Disclaimer: None of the views or conslusions drawn in this app are necessarily reflective of US EPA. Work was done as part of Toxicology Research Fellowship contracted by Oak Ridge 
                         Institute for Science and Education."),
                      br(),
                     # HTML('<p><img src="ORISElogo.png"/></p>'),
                      img(src='EPAlogo.png', align = "center", width = 70),
                      img(src='ORISElogo.png', align = "center", width = 150),
                      
                      width = 3),
                    mainPanel(
                      
                      h3(strong("Background")),
                      p("In the 1990's, some scientists proposed that certain chemicals might be disrupting the endocrine systems of humans and wildlife. At the time, only a handful of chemicals had been found to disrupt the endocrine 
                        systems of animals in laboratory studies, but compelling evidence showed that endocrine systems of certain fish and wildlife had been affected by chemical contaminants, 
                        resulting in developmental and reproductive problems. Based on this and other evidence, congress passed the Food Quality and Protection Act (FQPA) that requires EPA to screen pesticide chemicals 
                        for their potential to produce effects similar to those by the female hormones (estrogen) in humans. Fast forward several years and those chemicals have become of ever increasing concern and are now labeled
                        Endocrine Disrupting Chemicals (EDCs), as they not only effect the estrogen receptor, but also the androgen receptor. A tremendous amount of progress was made during those years in the early 21st century, 
                        but a mountain of over 10,000 chemicals in commerce have still evaded serious evaluation, and to this day remain an unsolved challenge. However, in 2015, EPA announced the use of cutting-edge technology 
                        including high-throughput animal-free assays and integrated computational models that would shift the approach from a reactive chemical assessment standpoint, to a predictive one."),
                      p("There are many laboratories, including the Simmons Laboratory at EPA in Research Triangle Park, that work to develop new endocrine-focused high throughput assays to provide data streams 
                        for computational models. The goal of this work is to develop assays that can screen and generate data on thousands of chemicals in a relatively short amount of time (less than 1 year), build predictive models based on chemical features (chemotypes), 
                        and use that model to categorize future chemicals into risk bins, where new high risk chemicals can be prioritized for more rigorous toxicological evaluation. 
                        While the estrogen model was first of it's kind to gain exposure, the androgen model is close behind and of equivalent importance. The androgen model was originally built on the back of four indespensible assay types, 
                        one of which was protein homodimerization assays. When streams of data for this assay type became unavailable, the computational modeling team needed an internalized replacement for 
                        collecting data on new chemicals going forward. This is where our laboratory stepped in and began developing a replacement assay in 2020 that has now been completed and validated. This app contains the validation data for this assay."),
                      
                      h3(strong("Purpose")),
                      p("This Shiny App is a user friendly way to explore real-life validation data for a novel androgen receptor chemical screening assay (AR2 assay) developed at the US EPA. Rather than present 
                        the data on paper, this shiny app gives you the ability to interact with the work, explore the plots for each chemical and biogroup, then navigate modeling this data and try some predictions. This app has 
                        three pages aside from this introduction, each with a distinct purpose."),
                      p("The Data Exploration page will allow you to scroll through the complete data set or view selected columns that can trim some unnecesary variables for plotting. 
                        This page will also allow you to filter data by chemical name, biogroup, or both. Finally, this page gives you the ability to save all subsets of data as a CSV file on your local machine. The data used to create plots is the 
                        unfiltered data set that appears on default."),
                      p("The Data Visualization page is a place to view plots of the data. Both the AR assay and cell viability plots are shown by default, but you'll be able to select AR only if desired. This page also allows 
                        you to filter plots by the same metrics as found in the Data Exploration page (chemical, biogroup, or both). Curves for these plots were fit using the hill curve function (from tcpl package) and hits in the AR assay 
                        were established using a 4*bmad threshold, shown on the plot. Final hit calls were made to distinguish AR specific chemicals from purely cytotoxic chemicals. This was done using two criteria: 1. Hit in AR assay, 2. deltaAC50 
                        from AR assay to cell viability assay > 1. Prior to constructing this app, final hit calls were mapped with chemical structural indicators, called chemotypes. This was done for all chemicals tested with available chemotype 
                        information (113/128). This dataset is what is used for chemotype modeling and can be explored in the Data tab within the Modeling page (to note, there are 729 possible chemotype predictors, most chemicals are comprised of
                         fewer than 15 - the Low-Dimension Data set contains only the 19 most common chemotypes for fitting the glm model)."),
                      p("The Modeling page implements the chemotype-hitcall dataset to predict hits in the AR2 assay. The goal of this page is two-fold. 
                        First, you'll be able to visualize which model is the best at predicting on the training set through measure of accuracy, then look at a variable importance plot to see which variables were most important to predicting AR hits. 
                        Knowing which chemotypes are responsible for AR antagonist activity would be particularly useful 
                        when looking at future chemicals to flag for further investigation. Second, this page allows you to view prediction of hitcall for 5 untested chemicals. Most chemicals are only comprised 
                        of a handful of chemotypes, and while it would be desirable to allow for the functionality of inputting any untested chemical and returning a hit call prediction, chemotyping software is complex and not readily linkable with code in R. As a result, 
                        users will only be able to predict the 5 chemicals chemotyped and prepared for prediction. However, with the prediction, you will be able to visualize chemical structure and 
                        if you are extremely familiar with organic chemistry, could compare important chemotypes to the structure displayed to make an educated guess on what the prediction will be (goodluck!)."),
                      
                      h3(strong("Note on Biogroups and Assay Specifics")),
                      p("Throughout this app, you will be able to sort and view data based on biogroups. This assay was developed using the xenobiotic metabolism retrofit method (linked in sidebar). Xenobiotic metabolism is responsible for 
                        rending toxic parent substances into less toxic (typically) metabolites and occurs mainly in your liver. There are over 50 enzymes in the human liver called Cytochrome p450 enzymes (cyp's) that contribute to this 
                        physiological process. In our assay, we make use of the 10 most prevelant human cyp's and this app gives you the functionality to filter based on each one or any combination of the 10. As you filter plots by cyp, 
                        you may be able to notice a shifted curve with some cyp's in comparison to the two controls, noRNA and Bgal. 
                        This is best observed by filtering the plots page to Chemical = Flutamide, and Biogroup = 1A2 and 2C19. There are 13 other Cyp-shifted chemicals in this set. To view plots for our AR assay control, 
                        filter the plots page to Chemical = BICAL. To view plots for our viability assay control, filter the plots page to Chemical = DCLN. Other interesting chemicals are Equilin with Cyp2C19, and Bisphenol A - one of the most well established 
                        EDCs that is banned from commerical use now (you'll see why). It should be noted that the chemicals tested here are part of an endocrine validation set that
                         is enriched for pro and anti-endocrine function, meaning most chemicals will either be clearly positive or clearly negative, and were selected for just this purpose. As a result, some of the variable importance conlusions should be 
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
                            
                              checkboxInput("Viability", strong("Show Viability Plots"), value = TRUE), 
                              p(em("Curves for these plots were fit using the hill curve function (from tcpl package). Hits in the AR assay were established using a 4*bmad threshold, shown on the plots. Final hit calls
                                    made to distinguish AR specific chemicals from cytotoxic chemicals was done using two criteria: 1. Hit in AR assay, 2. deltaAC50 from AR assay to cell viability assay > 1.")),
                              
                              width = 4),
                              
                              
                              mainPanel(plotOutput("plot"), plotOutput("ABplot")))),
                   
                   
                   
                  
                   #Modeling Panel
                   tabPanel("Modeling",
                            titlePanel("Chemotype Modeling for AR2 Assay"),
                      #sidebarLayout(
                      #sidebarPanel(
                      #  sliderInput("splitProp", strong("Set Proportion of Data to Train On"), min = 0.4, max = 0.85, 0.7, step = 0.05, ticks = FALSE)
                      #, width = 3),
                      
                      mainPanel(tabsetPanel(type = "tabs",
                                            tabPanel("Modeling Info",
                                                     h3(strong("Overview")),
                                                     p("Welcome to the modeling page. Here, we will work with the chemotype-hitcall dataset from the AR2 validation set. If you would like to explore the data first, click on the Chemotype Data 
                                                       tab and look around. 113 of our 128 chemicals tested were able to successfully be chemotyped using specialized software. This data set was then mapped to the hit calls from the validation screen 
                                                       that is shown as hit's or non-hits in the Data Visualization page. Each chemotype is a unique predictor in this data set, and the response is binary (0 or 1). If a chemical has a certain chemotype, 
                                                       the value for that chemotype for that chemical is 1. Most chemicals have a handful of distinct features, but their total chemotype count is typically less than 15. As such, there will be several unsused 
                                                       predictors due to the limited number of chemicals. In the Chemotype Data tab, you will have to option to select and view only the predictors with a value for at least 1 chemical, and from there, you will also have 
                                                       the ability to use either the full predictor list or the condensed predictor list to fit the tree-based models. Since generalize linear models require fewer predictors than observations, you'll be able to view and use the lower dimension 
                                                       data set for fitting of that model. This data set includes just the 19 most common chemotypes. "),
                                                     br(),
                                                     h4(strong("Generalized Linear Models")),
                                                     p("A generalized linear model will be the first of three models fit to this data. These models allow for responses from non-normal distributions such as in logistic regression with data from a binomial distribution. 
                                                       Logistic regression estimates the probability of success, such as a hit or a non-hit in this app, based on a given dataset of predictor variables. Since the outcome is a probability, the response is bounded between 0 and 1. 
                                                       In generalized linear models, there is a link function that is used to link the response (hitcall) to the linear function of the parameters. For binary response data, this link 
                                                       function, called the Logit function, is:"),
                                                     withMathJax(), 
                                                     helpText("$$ X \\beta = ln ( \\frac{\\mu}{1- \\mu} )$$"),
                                                     p("This logit link models the log odds of success as the linear function. Here, we'll deem success to be a hit in the AR assay. One advantage of this model over the others is it's 
                                                       relative ease of interpretibility. However, this type of model should really have uncorrelated predictors, but in this type of prediction analysis, many of the predictors may be correlated. In addition, the predictive power 
                                                       is generally lower than tree-based methods."),
                                                     br(),
                                                     h4(strong("Classification Trees")),
                                                     p("A classification tree will be the second type of model fit to this data. The goal of classification trees is to classify group membership, where the most prevelant class is used as prediction. These types of models are 
                                                       advantageous in that they do not require normalization of data, and that missing values do not significantly affect the process of tree building. In addition, the concept is typically easy to understand and explain. However, 
                                                       these models often require more time to train the model and increase complexity over other methods. Additionally, small changes in the data can often cause large shifts in tree structure, potentially resulting in instability. Despite this, 
                                                       using cross validation can typically improve the model. You'll be given the ability to tune the cross validation parameters in the Model Fitting tab."),
                                                     br(),
                                                     h4(strong("Random Forests")),
                                                     p("Random forest are an extension of bagged tree modeling, and are more complex than standard classification trees. Bagged tree models use bootstrap samples to fit several trees and average over all of those resamples. By doing this, the model chosen is tuned to multiple resamples of 
                                                       the data and thus generalizes better to give better prediction on new data than the single tree fit. In random forest models, this same approach is taken but instead of fitting each new tree 
                                                       with all the predictors, a random selection of only about 2/3 of predictors are used. This feature of random variable selection lends to an uncorrelated forest of decision trees. In comparison to a standard decision tree, 
                                                       random forests are more complex and thus lose some interpretibality and require more computational time, but their primary advantage is a significantly improved prediction. Particularly in classification response, 
                                                       random forests are very popular for predictive models. Random forests also help reduce the risk of 
                                                       overfitting and provide additional flexibility to make it easier to determine feature importance. A variable importance plot can be visualized to see which predictors are most important to the fit, and are a good fit statistic to look at."),
                                                       
                                                     br(),
                                                     br()
                                                     
                                                     ),
                                            tabPanel("Model Fitting", 
                                                     sidebarLayout(sidebarPanel(
                                                       p(em("GLM models require fewer predictors that observations. Select the Low-Dimensional dataset that includes only the 20 most frequently observed chemotypes. These can be found in the Chemotype Data tab.")),
                                                     selectInput("modeldataGLM", "Select Data(Predictors) to Fit GLM Model On", choices = c("Low-Dimensional Chemotypes Dataset")),
                                                      p(em("Tree based methods can accomdate high-dimensional data. As such, select either the full predictor dataset or the present predictors dataset. These can be found in the Chemotype Data tab.")),
                                                     selectInput("modeldata", "Select Data(Predictors) To Fit Tree Models On", choices = c("Present Chemotypes Dataset" = "subsetChemotypesB$hitcall", "Full Chemotype Dataset" = "chemotypeModel$hitcall")),
                                                     p(em("This will control the proportion of data to train the model on. Reported fit statistics will be indicative of each models performance on the testing set. In general, a higher training proportion may overfit data.")),
                                                     sliderInput("split", "Select Proportion Of Data for Training", min = 0.5, max = 0.9, 0.7, step = 0.05),
                                                     p(em("These will control cross-validation settings of the tree models. Higher settings will require more computational time.")),
                                                     sliderInput("cvnum", "Select Cross Validation Fold for Tree Models", min = 2, max = 10, 5, step = 1),
                                                     sliderInput("cvrep", "Select Cross Validation Repetitions for Tree Models", min = 1, max = 3, 2, step = 1),
                                                     p(em("This button will fit all 3 models according to your input. Once fit, adjustments can be made live without having to press again. There may be some lag time.")),
                                                     actionButton("fitmodels", "Fit Models")
                                                     ),
                                                     
                                                     
                                                     mainPanel(
                                                       h3(strong("Model Fit Statistics")),
                                                       br(),
                                                       h4(strong("Generalized Linear Model")),
                                                       tableOutput("glm"),
                                                       br(),
                                                       h4(strong("Classification Tree Model")),
                                                       tableOutput("basictreeData"),
                                                       br(),
                                                       h4(strong("Random Forest Model")),
                                                     fluidPage(splitLayout(
                                                       tableOutput("treeData"), plotOutput("treeDataPlot")))
                                                     
                                                    
                                                     ))),
                                            
                                            tabPanel("Prediction",
                                                     sidebarLayout(sidebarPanel(
                                                       h4(strong("Prediction Overview")),
                                                       p("In this tab, you'll be able to use a model to predict future hitcalls. As a result of chemotyping software being sophisticated and not readily linked with R, functionality of predicting your favorite chemical will not be included. However, I have provided a list of several chemotyped chemicals to chose from that were not included in this testing set. Click around to each chemical and look at their predicted hitcall for the AR2 assay."),
                                                      selectInput("predictChems", "Select Chemical to Predict", choices = c("2,6-Dichlorobenzonitrile"="1","Faslodex"="2","4,4'-((1H-1,2,4-triazol-1-yl)methylene)dibenzonitrile"="3","2,2,4,4,6,6,8,8-Octamethyl-1,3,5,7,2,4,6,8-tetraoxatetrasilocane"="4","Ronilan"='5')),
                                                      p(em("Tree based methods provide the best prediction of chemical hits. Both tree models have been optimized for use of prediction in this tab. Select the type of model you would like to predict with.")),
                                                      selectInput("modelpredict", "Select Model Type for Prediction", choices = c("Random Forest", "Classification Tree"), selected = "Random Forest"),
                                                      
                                                      p(
                                                        "If a chemical is predicted as a hit, that implies the chemical is more likely than not an 
                                                         androgen receptor antagonist (i.e. an endocrine disrupter). However, also be aware that this model was fit on 113 chemicals with not every possible chemotype being featured in that list. Some chemicals may contain chemotypes 
                                                         that impact the androgen receptor but were not included in the dataset and thus may predict as a non-hit. For all cases, the solution to this issue is to screen more chemicals, generate more chemotypes, and fit models based on a much 
                                                         larger dataset. Check toxicology journals next summer (2023) for an update on that very solution."
                                                      ),
                                                      
                                                      width = 4),
                                                      
                                                      mainPanel(
                                                        h4(strong("Your Chemical is Predicted as a: ")),
                                                        textOutput("prediction"),
                                                        
                                                        br(),
                                                        h4("Chemical Description:"),
                                                        textOutput("chemDes"),
                                                        #img(src=textOutput("chemImage"), align = "center", width = 150))
                                                        imageOutput("chemImage"))
                                                       
                                                     )),
                                            tabPanel("Chemotype Data", 
                                                     br(),
                                                     radioButtons("subsetChemos2", strong("Select Data to Display"), choices = c("Chemotypes Present in Chemical List", "Full Chemotype Dataset", "Low-Dimensional Dataset")),
                                                     verbatimTextOutput("displayChemos"),
                                                     dataTableOutput("hitsData")))))
))

