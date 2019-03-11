#Oakology Shiny App
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

#Notes
#Function for select box is selectInput.
#For slider it's sliderInput


# load the shiny package
library(shiny)
library(shinythemes)
library(raster)
library(rgdal)
library(leaflet)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(shinyWidgets)
library(colorspace)
library(kableExtra)

ui<-fluidPage(
  theme = shinytheme("yeti"),
              titlePanel(strong("Climate Change Vulnerability Assessment of Island Oaks")),
              navbarPage(h5("Oakology"),
              tabPanel(h5("Summary"),
                       sidebarPanel(width=3,
                        h2(strong("Oakology Group Project")),
                        br(),
                        h4(strong("Creators:")),
                        p("Sofie McComb, Jazmine Uy, Alyssa Winchell, Laura Wolf"),
                        br(),
                         h3(strong("Purpose of the App")),
                          p("This app was created to easily explore the data and selected results
                            from the Oakology Group Project at the Bren School of Environmental 
                            Science & Management, titled Climate Change Vulnerability Assessment 
                            of the Island Oak", em("(Quercus tomentella)."), "The application focuses on 
                            Santa Cruz and Santa Rosa Islands, due to data sharing restrictions 
                            and ease of visualization."),
                          br(),
                         h3(strong("How to Use the App")),
                          p("The tabs in this application provide an overview of the data, methodology,
                            and results used in the analysis of island oaks and their potential climate
                            vulnerability. All tabs provide relevant information for Santa Cruz and 
                            Santa Rosa Islands, the focus of the application. For more in-depth information
                            on what each tab provides, see “Tab Summary” under the Summary tab."),
                         br(),
                         p("For more information on the project and our team, you can go to our 
                           group project website:"),
                        uiOutput("tab1"),
                        br()
                         ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Project Overview",
                                    br(),
                                    img(src = "sri_oaks.png", width=750, height=500),
                                    p(em("Photo: Denise Knapp")),
                                    h3("Background"),
                                    p("Island oak",em("(Quercus tomentella)"),"is the rarest oak species in California and is 
                                      endemic to only six islands in the California Island Archipelago (CAIA): Anacapa, 
                                      Guadalupe, San Clemente, Santa Catalina, Santa Cruz, and Santa Rosa (Pavlik et al., 1991). 
                                      Islands oaks are considered a dominant species in the CAIA’s oak woodlands, where they 
                                      provide forest litter, protective habitat for other species, and most importantly, 
                                      soil moisture through fog drip (McCune, 2005; K. McEachern, Personal comm., 
                                      11 February, 2019). The past introduction of invasive herbivores from ranching 
                                      activity left lasting impacts on the island oaks’ ability to successfully establish 
                                      and disperse. In 2016 the species was listed as endangered by the IUCN."),
                                    p("Though historical threats have largely been removed from the islands in recent years, 
                                      legacy impacts from grazers are still widespread, and island oaks are still encumbered 
                                      by damaged and fragmented habitat. As a spatially-constrained species endemic to the 
                                      CAIA, island oak is particularly susceptible to extinction from habitat loss or 
                                      fragmentation and has limited opportunity to re-establish in more suitable regions 
                                      if additional threats force such movement (Harter et al. 2015). As the effects of 
                                      human-induced climate change intensify, island ecologists and managers would like 
                                      to determine appropriate management practices to ensure survival and health of 
                                      island oak on all six islands across its range."),
                                    h3("Significance"),
                                    p("Our clients, The Nature Conservancy and the Santa Barbara Botanic Garden, are 
                                      concerned with the decline of this species as well as other endemic species on 
                                      the Channel Islands. They tasked us with investigating how climate change might 
                                      affect future island oak distribution to help them adaptively manage this species 
                                      to increase its resilience and likelihood of persistence. Our goal was to model 
                                      the potential distributional changes of the island oak in the future, taking into 
                                      account the wide uncertainty in projections of future climate. Ultimately, our hope 
                                      is that island managers can use our comprehensive analysis in conjunction with prior 
                                      known information about island oaks to inform adaptive management planning on the 
                                      islands to best conserve and protect the species in a changing climate. Furthermore, 
                                      the methodology was developed throughout the process for application to other endemic 
                                      and threatened species of interest on the CAIA in the future."),
                                    h3("Project Objectives"),
                                    p("1. Model the potential distributional changes of the island oak in the future, 
                                      taking into account the wide uncertainty in projections of future climate."),
                                    p("2. Sum these predictions of suitable climate habitat across future scenarios 
                                      to develop an integrated outlook that quantifies the amount and location of the 
                                      long-term suitability of the species on each island and across the islands."),
                                    p("3. Explore the relationships that exist between oaks and climate on each island 
                                      and how oak presence is influenced by climate."),
                                    br()
                                    ),
                           tabPanel("Data", 
                                    br(),
                                    img(src = "stilted_oaks.png", width=750, height=500),
                                    p(em("Photo: Denise Knapp")),
                                    h3("Overview"),
                                    p("We analyzed how climate change will likely impact island oak presence across 
                                      the CAIA. This analysis required oak presence points, present climate data, future climate
                                      projections, and island specific data. All layers were resampled to the resolution of the 
                                      coarsest data, 270 meters, and were projected into the NAD83 Teale-Albers coordinate system.
                                      Sufficient data for the analysis was only available for three of the six islands that 
                                      contain the island oak— Santa Catalina, Santa Cruz, and Santa Rosa— so analyses were 
                                      performed for only these three islands."),
                                    h3("Oak Points"), 
                                    p("Presence data for both Santa Cruz and Santa Rosa were collected from 2003 – 2004, 2006 – 2007, 
                                      and in 2018 by Laura Kindsvater and multiple researchers from The Nature Conservancy 
                                      and the National Park Service. Data were recorded from satellite imagery or from GPS 
                                      coordinates in the field. Oak locations for Santa Cruz were updated in 2018 using 
                                      aerial surveys to estimate GPS coordinates. Oak presence points are a combination 
                                      of individual trees, groups of trees, and entire groves. Stage information for trees 
                                      on Santa Rosa was provided in an attribute table along with the original shapefiles. 
                                      In total, there were 1001 points on Santa Rosa, with 90 adult or young trees and 
                                      202 seedlings or saplings. There was a total of 271 oak locations on Santa Cruz 
                                      with no known age structure information."),
                                    h3("Island Data"),
                                    p("We obtained island specific data from various island managers and sources, including 
                                      island outlines (the National Park Service), elevation layers (NPS), soil data 
                                      (U.S. Geological Survey), and vegetation community shapefiles (NPS, The Nature 
                                      Conservancy). We ultimately did not include any of these layers in the final analyses, 
                                      but they were important for our initial analyses and understanding of the 
                                      characteristics of each island. Elevation is given in meters and is at 270-meter 
                                      resolution. The vegetation layer, shown at 50-meter resolution, has seven classes: 
                                      woodland, chaparral, coastal scrub, grassland, riparian, dune, and other 
                                      (developed, water, etc.)."),
                                    h3("Climate Data"),
                                    p("We acquired current and future climate data and projections from the Basin 
                                      Characterization Model (BCM), a regional hydrologic climate model statistically 
                                      downscaled for California to 270-meter resolution (Flint et al., 2013). BCM provides 
                                      climate data as averaged 30-year summaries for the current time period defined as 
                                      1981-2010 as well as for three future time periods (2010-2039, 2040-2069, 2070-2099). 
                                      We selected four future climate scenarios that capture some of the variability in 
                                      climate futures most likely to occur in California. These climate scenarios include 
                                      MIROC rcp8.5 (“hot-dry”), MIROC rcp4.5 (“warm-dry”), CCSM4 rcp4.5 (“hot-wet”), and 
                                      MPI rcp4.5 (“warm-wet”) projections. We used four climate variables in our analysis 
                                      which are available for viewing: maximum summer temperature (°C), minimum winter 
                                      temperature (°C), annual precipitation (mm), and climatic water deficit (mm). 
                                      Climate water deficit is the amount of additional water in an area that would 
                                      have evaporated or transpired had it been available (potential minus actual 
                                      evapotranspiration), and is an estimate of drought stress (Flint et al., 2013)."),
                                    uiOutput("tab2"),
                                    h3("Fog Data"),
                                    p("BCM provides precipitation and temperature-based climate variables, but does not 
                                      have data available for fog, an important variable for oak species. We obtained 
                                      fog data from Rastogi et al. 2016 that shows the current probability of fog 
                                      inundation for Santa Rosa and Santa Cruz Islands. We developed future fog predictions 
                                      from the current data by projecting historic trends into the future for four possible 
                                      fog scenarios: 1) constant fog, 2) decreasing fog, 3) increasing fog, and 4) change in 
                                      fog based on an elevational threshold."),
                                    p("The constant fog scenario acts as a control. The decreasing fog accounts for the possible 
                                      reductions in fog formation that may occur during the increasing intensity, frequency and 
                                      duration of ENSO events (Perkins et al., 2012). The increasing fog scenario considers the 
                                      past trend of fog occurrence increasing by 60% on average over the islands that is 
                                      reported in the literature, as measured by stations on San Nicolas Island 
                                      (Williams et al., 2015). The fog scenario that includes an elevation threshold was 
                                      considered to account for the changes in cloud height that are responsible for the 
                                      increase in fog occurrence trends measured on San Nicolas (Williams et all, 2015). 
                                      From 1948 – 2014, stratus above 255 meters has been decreased by 55% while stratus 
                                      below 255 meters, which can be considered fog, has increased by 40%. We used these 
                                      fog prediction scenarios in tandem with the climate future scenarios."),
                                    p("We ran SDM scenarios both with and without fog scenarios since we created 
                                      the fog projections ourselves based on the best available information. Given the major 
                                      uncertainties in future fog trends, the four projected fog scenarios aim to account for some of the 
                                      uncertainties, while the no fog scenario removes this uncertainty 
                                      altogether. Fog proved to be an important predictor of suitable habitat and increased 
                                      the fit of each model, but due to the major uncertainties in our projections we thought 
                                      it was important to have an option for which fog was not included.")
                                    
                                    ),
                           tabPanel("Methodology",
                                    br(),
                                    img(src = "soledad_oaks.jpg", width=750, height=500),
                                    p(em("Photo: Denise Knapp")),
                                    h3("Species Distribution Models"),
                                    p("Species distribution models (SDMs) provide statistical estimates of the relationship 
                                      between recorded species presence points and the environmental and spatial characteristics
                                      at those locations that significantly influence species distribution (Franklin 2010). 
                                      SDMs are widely used modeling tools for determining probable species distributions to 
                                      inform conservation management strategies, particularly for endangered and endemic 
                                      species that are otherwise difficult to fully model."),
                                    h3("MaxEnt"),
                                    p("We used MaxEnt species distribution modeling software to determine which of the available 
                                      environmental variables are most influential for predicting the island oak’s niche by
                                      running analyses with present species observations and environmental conditions. MaxEnt is
                                      an open-source java tool that performs presence-only SDM analyses and identifies habitat 
                                      suitability for a species over a landscape (Phillips, Anderson, and Schapire 2006). 
                                      It requires user inputs for species presence points and influential environmental 
                                      variables. MaxEnt then determines relationships between these inputs to define which 
                                      areas within the region of study satisfy a species’ ecological niche, or the species’ 
                                      environmental requirements for survival (Phillips et al. 2017; Phillips, Dudík, and 
                                      Schapire 2004; Hutchinson 1957). The model output raster predicts the probability of 
                                      species presence from 0 to 1, or low to high. User-defined parameters, model validation 
                                      methods, and various output settings make MaxEnt a flexible tool for application to a 
                                      broad range of SDM scenarios."),
                                    br(),
                                    p(em("The figure below shows an idealized representation of the MaxEnt process.")),
                                    img(src = "maxent.png", width=650, height=400),
                                    br()
                         ),
                         tabPanel("Tab Summary",
                                  br(),
                                  img(src = "tree_tunnel.jpg", width=750, height=500),
                                  p(em("Photo: Denise Knapp")),
                                  em("This section provides a detailed explanation of what can be found and
                                    explored on each tab."),
                                  h3("Summary"),
                                  p("The summary tab provides essential information on the project background, 
                                    significance, objectives, data, methodology, and sources, and should be used
                                    as a reference for the other tabs."),
                                  h3("Oaks"),
                                  p("The oak tab displays the island oak presence points on both islands. 
                                    For Santa Cruz, marker color for all oak points is customizable. Santa Rosa has
                                    stage structure information explorable for a subset of its points, so users can 
                                    choose the colors and visualize which of the oak points are known adults and 
                                    seedlings. These are shown in color on top of the rest of the 
                                    oaks on the island without known stage information, represented 
                                    by white circles. Users can interactively zoom into the oak points to see where 
                                    they are found on the terrain."),
                                  h3("Islands"),
                                  p("The island tab visualizes the digital elevation model (DEM) and vegetation class 
                                    layers for Santa Cruz and Santa Rosa. Users can select between these two layers and 
                                    can interactively zoom in for more detail."),
                                  h3("Climate"),
                                  p("The climate tab provides the Basin Characterization Model (BCM) climate layers available
                                    for the islands. Users can select between four different climate variables, four future 
                                    climate projections, and three future time periods. Users can compare differences in climate
                                    between projected future climate and current (historic) climate."),
                                  h3("Fog"),
                                  p("The fog tab visualizes the probability of fog inundation across the islands. Users can select 
                                    between historic fog and different future fog scenarios for three future time periods. Fog scenarios
                                    include: increasing fog, decreasing fog, constant fog, and a change in fog according
                                    to an elevational threshold of 255 meters."),
                                  h3("SDM"),
                                  p("The SDM tab displays results from the species distribution model (SDM) analyses for the island oak.
                                    These results show the predicted current and future probability of presence of island oaks across the
                                    islands. It is the most complex tab, so we recommend thoroughly reading below to have a better
                                    understanding of what we are visualizing. For more information on SDM analysis and the tools
                                    used, see the methodology tab under Summary."),
                                  p("The top panel displays the SDM results for the historic time period (1981 – 2010), showing the
                                    current predicted distribution of the species on Santa Cruz and Santa Rosa. Users can 
                                    select different color palettes, for which color scale best translates the information, 
                                    and can select between two scenarios— ‘no fog’ or ‘fog’—to switch between analyses 
                                    that either included fog or did not include fog."),
                                  p("The bottom panel displays the SDM results for future projections, and can be visualized
                                    for a combination of fog scenarios, climate projections, and time periods, across a 
                                    variety of color palettes. If the ‘no fog’ option is selected for the historic map, then 
                                    ‘no fog’ is the best choice to visualize for the projected map to make an 
                                    accurate comparison. If the ‘fog’ option for historic is selected, users should choose 
                                    one of the four future fog scenarios (constant, increase, decrease, elevation threshold). 
                                    Users can select the play button under the time period slider bar to visualize the change
                                    in results of a future scenario across time."),
                                  p("The tables underneath each map are updated based on the selection of historic or future
                                    projected SDM scenario. The tables provide information on the average test Area Under the
                                    Curve (AUC) metric, highest predicted suitability value, and percent of the island deemed 
                                    suitable island oak climate habitat under each scenario. In the projected table, the 
                                    percent change in predicted suitable habitat compared to the historic scenario is provided.
                                    AUC values are metrics of model fit and model predictive power, and range from 0-1. A 
                                    value close to 1 means that errors are minimized with regards to false positives and 
                                    false negatives. The SDM model output predicts the probability of species presence from 
                                    0 to 1, or low to high potential habitat suitability, as illustrated in the map and given 
                                    by the highest predicted suitability value in the table. Lastly, we determined the percent 
                                    of the island deemed suitable island oak habitat under each scenario. This calculation 
                                    is based on a binary threshold value set by the MaxEnt output maximum training specificity 
                                    + sensitivity value, which is the current practice for selecting MaxEnt suitability 
                                    threshold values (Liu, White, and Newell 2013). The tables give the percent suitable 
                                    area and percent change in suitability depending on these threshold values, which 
                                    typically range from about 0.15-0.4. The binary interpretation of island oak habitat 
                                    suitability facilitates comparisons between future scenarios to better understand the 
                                    range of variability and uncertainty in the persistence of the oak across islands and 
                                    potential futures."),
                                  br(),
                                  br()
                                  ),
                         tabPanel("Sources",
                                  br(),
                                  h3("References"),
                                  p("Flint, Lorraine E., Alan L. Flint, James H. Thorne, and Ryan Boynton. 2013. 
                                    “Fine-Scale Hydrologic Modeling for Regional Landscape Applications: The California
                                    Basin Characterization Model Development and Performance.” Ecological Processes 2
                                    (July): 25. https://doi.org/10.1186/2192-1709-2-25."),
                                  p("Flint, L.E. and Flint, A.L., 2014, California Basin Characterization Model: A Dataset
                                    of Historical and Future Hydrologic Response to Climate Change, (ver. 1.1, May 2017):
                                    U.S. Geological Survey Data Release, https://doi.org/10.5066/F76T0JPB."),
                                  p("Franklin, J. (2010). Mapping Species Distributions: Spatial Inference and Prediction.
                                    Cambridge: Cambridge University Press. https://doi.org/10.1017/CBO9780511810602."),
                                  p("Harter, David E. V., Severin D. H. Irl, Bumsuk Seo, Manuel J. Steinbauer, 
                                      Rosemary Gillespie, Kostas A. Triantis, José-María Fernández-Palacios, and Carl 
                                    Beierkuhnlein. 2015. “Impacts of Global Climate Change on the Floras of Oceanic 
                                    Islands – Projections, Implications and Current Knowledge.” Perspectives in Plant
                                    Ecology, Evolution and Systematics 17 (2): 160–83.
                                    https://doi.org/10.1016/j.ppees.2015.01.003."),
                                  p("Hutchinson, E. (1957). “Concluding Remarks.” Cold Spring Harbor Symposia on 
                                    Quantitative Biology 22 (2): 415–27."),
                                  p("Kindsvater, L. (2006). Conservation and Restoration of the Endemic Island Oak, Quercus 
                                    tomentella in Channel Islands National Park using a Habitat Approach. University of 
                                    California - Davis, Davis, CA."),
                                  p("Kindsvater, L. (2010). Plant communities associated with the rare, paleoendemic oak, 
                                    Quercus tomentella, on Santa Cruz and Santa Rosa, Islands, California. Oak Ecosystem 
                                    Restoration on Santa Catalina Island, California: Proceedings of an on-Island Workshop, 
                                    February 2-4, 2007, 16."),
                                  p("Liu, Canran, Matt White, and Graeme Newell. 2013. “Selecting Thresholds for the Prediction
                                    of Species Occurrence with Presence-Only Data.” Journal of Biogeography 40 (4): 778–89. 
                                    https://doi.org/10.1111/jbi.12058."),
                                  p("McCune, J. (2005). Report on the census and survey of Island oak (Quercus 
                                    tomentella Engelm.) and canyon live oak (Quercus chrysolepis Liebm.) groves
                                    on Catalina Island, 2004 and 2005. Unpublished report prepared for the Catalina
                                    Island Conservancy, Avalon, CA."),
                                  p("McEachern, K., 11 February, 2019. Phone communication."),
                                  p("Pavlik, B.M., P.C. Muick, et al. 1991. Oaks of California. Cachuma Press,
                                    Los Olivos, CA and the California Oak Foundation, Oakland, CA."),
                                  p("Perkins, S.E., Alexander, L.V., and Nairn, J.R. (2012). Increasing frequency, 
                                    intensity and duration of observed global heatwaves and warm spells. Geophysical 
                                    Research Letters, 39(20)."),
                                  p("Phillips, Steven J., Robert P. Anderson, Miroslav Dudík, Robert E. Schapire, and 
                                    Mary E. Blair. 2017. “Opening the Black Box: An Open-Source Release of Maxent.” 
                                    Ecography 40 (7): 887–93. https://doi.org/10.1111/ecog.03049."),
                                  p("Phillips, Steven J., Robert P. Anderson, and Robert E. Schapire. 2006. “Maximum 
                                    Entropy Modeling of Species Geographic Distributions.” Ecological Modelling 190 
                                    (3–4): 231–59. https://doi.org/10.1016/j.ecolmodel.2005.03.026."),
                                  p("Phillips, Steven J., Miroslav Dudík, and Robert E. Schapire. 2004. “A Maximum 
                                    Entropy Approach to Species Distribution Modeling.” In Twenty-First International 
                                    Conference on Machine Learning  - ICML ’04, 83. Banff, Alberta, Canada: ACM Press. 
                                    https://doi.org/10.1145/1015330.1015412."),
                                  p("Rastogi, B., Williams, A.P., Fischer, D.T., Iacobellis, S.F., McEachern, K., Carvalho, 
                                    L., Jones, C., Baguskas, S., and Still, C.J. (2016). Spatial and temporal patterns of 
                                    cloud cover and fog inundation in coastal California: Ecological implications. Earth 
                                    Interactions, 20(15), 1-19."),
                                  p("Williams, A.P., Schwartz, R.E., Iacobellis, S., Seager, R., Cook, B.I., Still, C.J., Husak, G., 
                                    and Michaelsen, J. (2015). Urbanization causes increased cloud base height and decreased 
                                    fog in coastal Southern California. Geophysical Research Letters, 42(5), 1527-1536."),
                                  br(),
                                  img(src = "landscape_soledad_oaks.jpg", width=900, height=600),
                                  p(em("Photo: Denise Knapp")),
                                  br(),
                                  br()
                                  
                         )
                       )
                       
                       ),
                       br(),
                       br(),
                       fluidRow(
                         column(2,img(src = "bren.png", width=180, height=60)),
                         column(2, offset = 2, img(src = "tnc_logo.png", width=210, height=60)),
                         column(2, offset = 2, img(src = "sbbg_logo.jpg", width=210, height=60))
                       ),
                       
                       br(),
                       br(),
                       br()),
              tabPanel(h5("Oaks"),
                       sidebarLayout(
                         sidebarPanel(width=3,
                            h3(strong("Island Oaks")),
                            p("The data shown here represent island oak presence points on Santa Cruz and Santa Rosa
                              Islands, where each presence point symbolizes individual trees, groups of individuals, 
                              or an entire grove of island oaks. For Santa Rosa Island, choose between adults and seedlings to 
                              highlight the stage structure category of interest in the chosen color, while all other points 
                              (oaks with unknown stage and the unselected category)  will appear in white. We recommend 
                              zooming in, to better see where the points fall on the landscape."),
                            br(),
                           selectInput("points_colors", h4(strong("Choose a Color")),
                                                  c("Periwinkle" = "#B0C4DE",
                                                    "Chartreuse" = "#ADFF2F",
                                                    "Vermillion" = "#FF4500",
                                                    "Pewter" = "#708090",
                                                    "Celadon" = "#48D1CC",
                                                    "Indigo" = "#4B0082",
                                                    "Saffron" = "#FF7F50",
                                                    "Fuchsia" = "#FF00FF",
                                                    "Lavender" = "#E6E6FA",
                                                    "Viridian" = "#2E8B57",
                                                    "Cerulean" = "#1E90FF",
                                                    "Salmon" = "#FA8072",
                                                    "Atomic Tangerine" = "#FF6347")),
                           p(em("Data Source: The National Park Service and Laura Kindsvater."))
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Santa Cruz",
                                      br(),
                                      leafletOutput("SCRpoints", width=800, height=400),
                                      br(),
                                      p("Notice how the oaks on Santa Cruz are concentrated on the northern side of the island. 
                                        In total, there are 271 total oak points on Santa Cruz.")),
                             
                             tabPanel("Santa Rosa",
                                      br(),
                                      sidebarPanel(
                                        radioButtons("age", h4(strong("Choose a Stage Group")),
                                                     c("Seedlings" = "seed",
                                                       "Adults" = "adult")),
                                        width = 3
                                      ),
                                      leafletOutput("SRIpoints", width=800, height=400),
                                      br(),
                                      p("Notice how the oaks on Santa Rosa are found mainly in the central portion of the island away from the 
                                        coast. In total, there are 1001 oak points on Santa Rosa. Out of these points, 202 are known seedling
                                        and 90 are known adults. Selected color highlights either known seedlings/saplings or known adults, 
                                        depending on the stage group option selected.")
                                      )
                           )
                           
                           )
                         
                       ),
                       br(),
                       br(),
                       fluidRow(
                         column(2,img(src = "bren.png", width=180, height=60)),
                         column(2, offset = 2, img(src = "tnc_logo.png", width=210, height=60)),
                         column(2, offset = 2, img(src = "sbbg_logo.jpg", width=210, height=60))
                       ),
                       br(),
                       br(),
                       br()
              ), #End of Oak Points
              tabPanel(h5("Islands"),
                       fluidRow(
                         sidebarPanel(width=3,
                          h3(strong("Island Variables")),
                          p("The Islands tab allows users to select between viewing digital elevation models (DEM)
                            or vegetation class layers for Santa Cruz and Santa Rosa Islands. The layers may
                            take a second to load as they are relatively detailed and fine resolution. More information 
                            on the layers can be found in the 'tab summary' and 'data' tab under 'Summary'."),
                          br(),
                           selectInput("islandvar", h4(strong("Choose an Island Variable")), c("DEM", "Vegetation")),
                           p(em("Data Source: The National Park Service."))
                           ),
                         column(4,leafletOutput("islandmap", width=1000, height=600))
                         ),
                       br(),
                       br(),
                       fluidRow(
                         column(2,img(src = "bren.png", width=180, height=60)),
                         column(2, offset = 2, img(src = "tnc_logo.png", width=210, height=60)),
                         column(2, offset = 2, img(src = "sbbg_logo.jpg", width=210, height=60))
                       ),
                       br(),
                       br(),
                       br()
                       
                       ),
              tabPanel(h5("Climate"), 
                       sidebarLayout(
                         sidebarPanel(width=3,
                          h3(strong("Climate Variables")),
                          p("Select a climate variable and compare the outputs of the historic time period and the
                                selected future climate projection and time period."),
                          br(),
                          selectInput("raster_color_climate", h4(strong("Choose a Color Theme")),
                                      c("Rainbow" = "Spectral",
                                        "Sailing on a Sunny Day" = "YlGnBu",
                                        "Dandelion" = "YlGn",
                                        "Pomegranate" = "PuRd",
                                        "Apricot" = "YlOrRd",
                                        "The Dichotomy of Man" = "BrBG",
                                        "Mango" = "RdYlGn",
                                        "Eggplant" = "PRGn",
                                        "Sunset Sailing" = "RdYlBu")),
                           selectInput("climate_variable", h4(strong("Choose an Environmental Variable")),
                                       choices = c("Climate Water Deficit", 
                                                   "Precipitation", 
                                                   "Minimum Winter Temperature", 
                                                   "Maximum Summer Temperature")),
                           selectInput("climate_scenario", h4(strong("Choose a Climate Scenario")),
                                       choices = c("Hot-Wet", 
                                                   "Warm-Wet", 
                                                   "Warm-Dry", 
                                                   "Hot-Dry")),
                           sliderTextInput("climate_time",h4(strong("Choose a Time Period")), 
                                           choices = c("2010-2039", 
                                                       "2040-2069", 
                                                       "2070-2099"),
                                           animate = TRUE),
                           p(em("Data Source: Flint and Flint 2014."))

                         ),
                         
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Santa Cruz",
                                      h3(strong("Historic")),
                                      leafletOutput("scrHC", width=800, height=400),
                                       h3(strong("Projected")),
                                       leafletOutput("SCRclimatemap", width=800, height=400)),
                             tabPanel("Santa Rosa",
                                      h3(strong("Historic")),
                                      leafletOutput("sriHC", width=800, height=400),
                                      h3(strong("Projected")),
                                      leafletOutput("SRIclimatemap", width=800, height=400))
                           )),
                         position = c("left", "right"),
                         fluid = FALSE
                       ),
                       
                       br(),
                       br(),
                       fluidRow(
                         column(2,img(src = "bren.png", width=180, height=60)),
                         column(2, offset = 2, img(src = "tnc_logo.png", width=210, height=60)),
                         column(2, offset = 2, img(src = "sbbg_logo.jpg", width=210, height=60))
                       ),
                       br(),
                       br(),
                       br()
              ), #end climate tab
              tabPanel(h5("Fog"),
                       sidebarLayout(
                         sidebarPanel(width=3,
                            h3(strong("Choose a Fog Scenario")),        
                            p("Select between the four possible fog scenarios, and visualize the changes in probability of fog inundation
                              from the historic time period (1981-2010) across the three projected time
                              periods. The fog layer is identical across the four scenarios for the historic time 
                              period."),
                            br(),
                             selectInput("fogscen", h4(strong("Fog Scenarios")), 
                             choices = c("Constant", "Increase", "Decrease", "Elevation Threshold")),
                            sliderTextInput("timeperiods",h4(strong("Choose a Time Period")) , 
                                 choices = c("1981-2010", "2010-2039", "2040-2069", "2070-2099"),
                                 animate = TRUE),
                           p(em("Data Source: Rastogi et al., 2016."))

                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Santa Cruz",
                                      br(),
                                      leafletOutput("scrfogmap", width=800, height=400)),
                             tabPanel("Santa Rosa",
                                      br(),
                                      leafletOutput("srifogmap", width=800, height=400))
                           )
                         )
                       ),
                       br(),
                       br(),
                       fluidRow(
                         column(2,img(src = "bren.png", width=180, height=60)),
                         column(2, offset = 2, img(src = "tnc_logo.png", width=210, height=60)),
                         column(2, offset = 2, img(src = "sbbg_logo.jpg", width=210, height=60))
                       ),
                       
                       br(),
                       br(),
                       br()
                       ),
              tabPanel(h5("SDM"),
                       fluidRow(
                         sidebarPanel(width=3,
                                h3(strong("Historic")),
                                p("Species distribution model (SDM) results for the island oak in the historic time
                                  period, with or without fog included in the analyses. We highly recommend reading the tab 
                                  summary and methodology tab under summary."),
                                br(),
                                selectInput("histsdmcolor", h4(strong("Choose a Color Palette")), 
                                            c("Spectral","Inverse Spectral" ,"Viridis", "Magma")),
                                selectInput("histscenario", h4(strong("Choose a Scenario")), 
                                            c("No Fog","Fog"))
                                ),
                         column(4,
                                leafletOutput("histsdmmap", width=800, height=400)),
                         column(6,
                                htmlOutput("historictable")
                                )
                       ),
                       br(),
                       # In case table should be made fluidrow instead of column for placement
                       # fluidRow(column(6, offset=3,
                       #                 htmlOutput("historictable")
                       #                 )),

                       br(),
                       fluidRow(
                         sidebarPanel(width=3,
                                h3(strong("Projected")),
                                p("Species distribution model (SDM) results for the island oak across projected fog scenarios,
                                  climate projections, and time periods. The no fog scenario corresponds with the historic no fog option,
                                  and the constant fog, fog increase, fog decrease, and fog elevation threshold scenarios correspond with the historic fog option. We highly recommend reading 
                                  the tab summary and methodology tab under summary."),
                                br(),
                                selectInput("sdmcolor", h4(strong("Choose a Color Palette")), 
                                            c("Spectral","Inverse Spectral" ,"Viridis", "Magma")),
                                selectInput("scenario", h4(strong("Choose a Scenario")), 
                                            c("No Fog","Constant Fog","Fog Increase","Fog Decrease","Fog Elevation Threshold")),
                                selectInput("projection", h4(strong("Choose a Projection")), 
                                            c("Hot-Wet","Warm-Wet","Warm-Dry","Hot-Dry")),
                                sliderTextInput("timeperiod", h4(strong("Choose a Time Period")), 
                                                choices = c("2010-2039","2040-2069","2070-2099"), animate=TRUE)
                                ),
                         column(4,
                                leafletOutput("sdmmap", width=800, height=400)),
                         column(6, 
                                htmlOutput("projtable")
                               )
                       ),
                       # fluidRow(column(6, offset=3,
                       #                 htmlOutput("projtable")))

                       br(),
                       br(),
                       fluidRow(
                         column(2,img(src = "bren.png", width=180, height=60)),
                         column(2, offset = 2, img(src = "tnc_logo.png", width=210, height=60)),
                         column(2, offset = 2, img(src = "sbbg_logo.jpg", width=210, height=60))
                       ),
                       
                       br(),
                       br(),
                       br()
                       ) #SDM tab panel
        )#nav bar page
)#ui 

# Define server logic ----
server <- function(input, output, session) {

  ###############################################
  ### Summary Tab ###
  ###############################################
  
  #Summary page links 
  url1 <- a("Oakology Wesbite", href="https://oakology19.wixsite.com/oakology/island-oaks")
  output$tab1 <- renderUI({
    tagList("", url1)
  })
  
  url2 <- a("BCM Data", href="http://climate.calcommons.org/bcm")
  output$tab2 <- renderUI({
    tagList("Link:", url2)
  })
  
  ###############################################
  ### Oak Tab ###
  ###############################################
  
  points_color <- reactive({
    input$points_colors
    
  })
  
  # Read in the data
  combo <- read.csv("data/oaks/sri/combo.csv")
  scr_points <- read.csv("data/oaks/scr/all_4326.csv")
  
  output$SCRpoints <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addCircleMarkers(radius = 4, fillColor = points_color(), stroke = FALSE, fillOpacity = 0.5, data = scr_points, lng = ~POINT_X, lat = ~POINT_Y)
    
  })
  
  filteredData <- reactive({
    combo[ combo$Age == input$age, ]
  })
  
  # Read in the data
  combo <- read.csv("data/oaks/sri/combo.csv")
  scr_points <- read.csv("data/oaks/scr/all_4326.csv")
  alloak <- combo[ combo$Age == "all", ]
  
  
  output$SRIpoints <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11)
  })
  
  
  observe({
    
    leafletProxy("SRIpoints") %>%
      clearMarkers() %>% 
      addCircleMarkers(data = alloak, lng = ~POINT_X, lat = ~POINT_Y, radius = 4, fillColor = "mintcream", stroke = FALSE, fillOpacity = 0.4) %>%
      addCircleMarkers(data = filteredData(), lng = ~POINT_X, lat = ~POINT_Y, radius = 4, fillColor = points_color(), stroke = FALSE, fillOpacity = 0.4)
  })
  
  
  ###############################################
  ### Island Tab ###
  ###############################################
  
  output$islandmap <- renderLeaflet({
    scrveg<-raster("data/islands/scr/veg.tif")
    sriveg<-raster("data/islands/sri/veg.tif")
    mergedveg<-raster::merge(scrveg, sriveg, tolerance = 0.5)
    
    dem<-raster("data/islands/both/DEM.tif")
    proj4string(dem) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    veg<-raster("data/islands/both/veg50.tif")
    proj4string(veg) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    island <- switch(input$islandvar,
                     "DEM" = island <- dem,
                     "Vegetation" = island <- veg)
    
    col <- switch(input$islandvar,
                  "DEM" = col <- colorNumeric(palette = grDevices::terrain.colors(20), domain=values(island), na.color = "transparent", reverse=FALSE),
                  "Vegetation" = col <- colorFactor(palette = "Set1", domain=values(island), na.color = "transparent", reverse=FALSE))
    
    DEMlabels<-""
    veglabels<-c(": Woodland",": Chaparral",": Coastal Scrub",": Grassland",": Riparian",": Dune",": Other")
    
    legendlabels <- switch(input$islandvar,
                           "DEM" = legendlabels <- DEMlabels,
                           "Vegetation" = legendlabels <- veglabels)
    
    DEMtitle<-"Elevation (m)"
    vegtitle<-"Vegetation Class"
    
    legendtitle <- switch(input$islandvar,
                          "DEM" = legendtitle <- DEMtitle,
                          "Vegetation" = legendtitle <- vegtitle)
    
    leaflet() %>% addTiles() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      addRasterImage(island, colors = col, opacity = 0.8, method = "ngb") %>% 
      addLegend("topright", pal = col, values = values(island), labFormat = labelFormat(suffix = legendlabels), title = legendtitle)
    
  }) #end render leaflet
  
  ###############################################
  ### Climate Tab ###
  ###############################################
  
  output$SRIclimatemap <- renderLeaflet({
    
    
    climate_scen <-switch(input$climate_scenario,
                          "Warm-Wet"=climate_scen<-"MPI_rcp45", 
                          "Hot-Wet"=climate_scen<-"CCSM4_rcp85", 
                          "Warm-Dry"=climate_scen<-"MIROC_rcp45", 
                          "Hot-Dry"=climate_scen<-"MIROC_rcp85")
    
    climate_hands <-switch(input$climate_time,
                           "2010-2039"=climate_hands<-"2010_2039",
                           "2040-2069"=climate_hands<-"2040_2069", 
                           "2070-2099"=climate_hands<-"2070_2099")
    
    climate_var<-switch(input$climate_variable,
                        "Climate Water Deficit"=climate_var<-"cwd",
                        "Precipitation"=climate_var<-"ppt", 
                        "Minimum Winter Temperature"=climate_var<-"tmn", 
                        "Maximum Summer Temperature"=climate_var<-"tmx")
    
    
    
    climate_sri<-raster(paste0("data/climate/sri/", climate_scen,  "_", climate_hands, "/", climate_var, ".tif"))
    # climate_sri<-raster(paste0("data/climate/sri/historic/cwd.tif")) 
    proj4string(climate_sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    
    
    climate_stack_list <- list.dirs("data/climate/sri/", recursive = TRUE, full.names = TRUE)
    files <- climate_stack_list[grep(paste0(climate_scen), climate_stack_list, fixed=T)]
    climate_files2 <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    climatehist_files <- list.files("data/climate/sri/historic", recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    climate_files <- c(climatehist_files, climate_files2)
    climate_stack <- stack(climate_files)
    climate_colors <- reactive({ 
      input$raster_color_climate
    })
    
    pal <- colorNumeric( 
      palette = climate_colors(),
      domain = values(climate_stack),
      na.color = NA,
      reverse = TRUE
    )
    
    if (input$climate_variable == "Climate Water Deficit") {
      climate_title = "CWD (mm)"
    } else if (input$climate_variable == "Precipitation") {
      climate_title = "PPT (mm)"
    } else if (input$climate_variable == "Maximum Summer Temperature") {
      climate_title = "TMX (°C)"
    } else if (input$climate_variable == "Minimum Winter Temperature") {
      climate_title = "TMN (°C)"
    } else
      climate_title = "CWD (mm)"
    
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) %>% 
      addRasterImage(climate_sri, colors = pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = pal, values= values(climate_stack),
                title = climate_title)
    
    
  })
  
  output$sriHC <- renderLeaflet({
    
    
    climate_scen <-switch(input$climate_scenario,
                          "Warm-Wet"=climate_scen<-"MPI_rcp45", 
                          "Hot-Wet"=climate_scen<-"CCSM4_rcp85", 
                          "Warm-Dry"=climate_scen<-"MIROC_rcp45", 
                          "Hot-Dry"=climate_scen<-"MIROC_rcp85")
    
    climate_hands <-switch(input$climate_time,
                           "2010-2039"=climate_hands<-"2010_2039",
                           "2040-2069"=climate_hands<-"2040_2069", 
                           "2070-2099"=climate_hands<-"2070_2099")
    
    
    climate_var<-switch(input$climate_variable,
                        "Climate Water Deficit"=climate_var<-"cwd",
                        "Precipitation"=climate_var<-"ppt", 
                        "Minimum Winter Temperature"=climate_var<-"tmn", 
                        "Maximum Summer Temperature"=climate_var<-"tmx")
    
    
    
    climate_sri<-raster(paste0("data/climate/sri/historic/", climate_var, ".tif"))
    
    proj4string(climate_sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    climate_stack_list <- list.dirs("data/climate/sri/", recursive = TRUE, full.names = TRUE)
    files <- climate_stack_list[grep(paste0(climate_scen), climate_stack_list, fixed=T)]
    climate_files2 <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climatehist_files <- list.files("data/climate/sri/historic", recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climate_files <- c(climatehist_files, climate_files2)
    
    climate_stack <- stack(climate_files)
    
    climate_colors <- reactive({
      input$raster_color_climate
    })
    
    pal <- colorNumeric( 
      palette = climate_colors(),
      domain = values(climate_stack),
      na.color = NA,
      reverse = TRUE
    )
    
    if (input$climate_variable == "Climate Water Deficit") {
      climate_title = "CWD (mm)"
    } else if (input$climate_variable == "Precipitation") {
      climate_title = "PPT (mm)"
    } else if (input$climate_variable == "Maximum Summer Temperature") {
      climate_title = "TMX (°C)"
    } else if (input$climate_variable == "Minimum Winter Temperature") {
      climate_title = "TMN (°C)"
    } else
      climate_title = "CWD (mm)"
    
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) %>% 
      addRasterImage(climate_sri, colors = pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = pal, values= values(climate_stack),
                title = climate_title)
    
    
  })
  
  
  
  output$SCRclimatemap <- renderLeaflet({
    
    
    climate_scen <-switch(input$climate_scenario,
                          "Warm-Wet"=climate_scen<-"MPI_rcp45", 
                          "Hot-Wet"=climate_scen<-"CCSM4_rcp85", 
                          "Warm-Dry"=climate_scen<-"MIROC_rcp45", 
                          "Hot-Dry"=climate_scen<-"MIROC_rcp85")
    
    climate_hands <-switch(input$climate_time,
                           "2010-2039"=climate_hands<-"2010_2039",
                           "2040-2069"=climate_hands<-"2040_2069", 
                           "2070-2099"=climate_hands<-"2070_2099")
    
    climate_var<-switch(input$climate_variable,
                        "Climate Water Deficit"=climate_var<-"cwd",
                        "Precipitation"=climate_var<-"ppt", 
                        "Minimum Winter Temperature"=climate_var<-"tmn", 
                        "Maximum Summer Temperature"=climate_var<-"tmx")
    
    
    
    climate_scr<-raster(paste0("data/climate/scr/", climate_scen,  "_", climate_hands, "/", climate_var, ".tif"))
    
    proj4string(climate_scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    climate_stack_list <- list.dirs("data/climate/scr/", recursive = TRUE, full.names = TRUE)
    files <- climate_stack_list[grep(paste0(climate_scen), climate_stack_list, fixed=T)]
    climate_files2 <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climatehist_files <- list.files("data/climate/scr/historic", recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climate_files <- c(climatehist_files, climate_files2)
    
    climate_stack <- stack(climate_files)
    
    climate_colors <- reactive({
      input$raster_color_climate
    })
    
    climate_pal <- colorNumeric(
      palette = climate_colors(),
      domain = values(climate_stack),
      na.color = NA,
      reverse = TRUE
    )
    
    
    if (input$climate_variable == "Climate Water Deficit") {
      climate_title = "CWD (mm)"
    } else if (input$climate_variable == "Precipitation") {
      climate_title = "PPT (mm)"
    } else if (input$climate_variable == "Maximum Summer Temperature") {
      climate_title = "TMX (°C)"
    } else if (input$climate_variable == "Minimum Winter Temperature") {
      climate_title = "TMN (°C)"
    } else
      climate_title = "CWD (mm)"
    
    
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addRasterImage(climate_scr, colors = climate_pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = climate_pal, values= values(climate_stack),
                title = climate_title)
    
    
    
  })
  
  output$scrHC <- renderLeaflet({
    
    
    climate_scen <-switch(input$climate_scenario,
                          "Warm-Wet"=climate_scen<-"MPI_rcp45", 
                          "Hot-Wet"=climate_scen<-"CCSM4_rcp85", 
                          "Warm-Dry"=climate_scen<-"MIROC_rcp45", 
                          "Hot-Dry"=climate_scen<-"MIROC_rcp85")
    
    climate_hands <-switch(input$climate_time,
                           "2010-2039"=climate_hands<-"2010_2039",
                           "2040-2069"=climate_hands<-"2040_2069", 
                           "2070-2099"=climate_hands<-"2070_2099")
    
    climate_var<-switch(input$climate_variable,
                        "Climate Water Deficit"=climate_var<-"cwd",
                        "Precipitation"=climate_var<-"ppt", 
                        "Minimum Winter Temperature"=climate_var<-"tmn", 
                        "Maximum Summer Temperature"=climate_var<-"tmx")
    
    climate_scr<-raster(paste0("data/climate/scr/historic/", climate_var, ".tif"))
    
    proj4string(climate_scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    climate_stack_list <- list.dirs("data/climate/scr/", recursive = TRUE, full.names = TRUE)
    files <- climate_stack_list[grep(paste0(climate_scen), climate_stack_list, fixed=T)]
    climate_files2 <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climatehist_files <- list.files("data/climate/scr/historic", recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climate_files <- c(climatehist_files, climate_files2)
    
    climate_stack <- stack(climate_files)
    
    climate_colors <- reactive({
      input$raster_color_climate
    })
    
    climate_pal <- colorNumeric(
      palette = climate_colors(),
      domain = values(climate_stack),
      na.color = NA,
      reverse = TRUE
    )
    
    
    if (input$climate_variable == "Climate Water Deficit") {
      climate_title = "CWD (mm)"
    } else if (input$climate_variable == "Precipitation") {
      climate_title = "PPT (mm)"
    } else if (input$climate_variable == "Maximum Summer Temperature") {
      climate_title = "TMX (°C)"
    } else if (input$climate_variable == "Minimum Winter Temperature") {
      climate_title = "TMN (°C)"
    } else
      climate_title = "CWD (mm)"
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addRasterImage(climate_scr, colors = climate_pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = climate_pal, values= values(climate_stack),
                title = climate_title)
 
  })
  

  ###############################################
  ### Fog Tab ###
  ###############################################
  
  output$scrfogmap <- renderLeaflet({
    foggy_scen<-switch(input$fogscen,
                       "Constant"=scen<-"const", 
                       "Increase"=scen<-"inc", 
                       "Decrease"=scen<-"dec", 
                       "Elevation Threshold"=scen<-"elev")
    
    fog_time<-switch(input$timeperiods,
                     "1981-2010"=time<-"historic",
                     "2010-2039"=time<-"2010_2039",
                     "2040-2069"=time<-"2040_2069", 
                     "2070-2099"=time<-"2070_2099")
    
    
    fog_scr<-raster(paste0("data/fog/scr/",foggy_scen,"/", fog_time, ".tif"))
    proj4string(fog_scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    fog_stack_list <- list.dirs("data/fog/scr/", full.names = TRUE)
    fog_files <- fog_stack_list[grep(paste0(foggy_scen), fog_stack_list, fixed=T)]
    fog_files_list <- list.files(fog_files, full.names = TRUE)
    fog_files2 <- fog_files_list[grep(".tif", fog_files_list, fixed=T)]
    fog_stack <- stack(fog_files2)
    
    fog_pal <- colorNumeric(
      palette = "Blues",
      domain = values(fog_stack),
      na.color = NA
    )
    
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addRasterImage(fog_scr, colors = fog_pal, opacity = 0.8) %>% 
      addLegend("topright", pal = fog_pal, values= values(fog_stack),
                title = "Probability",
                labFormat = labelFormat(transform=function(fog_scr) sort (fog_scr, decreasing=FALSE)))
    
    
  }) #end render leaflet
  
  output$srifogmap <- renderLeaflet({
    foggy_scen<-switch(input$fogscen,
                       "Constant"=scen<-"const", 
                       "Increase"=scen<-"inc", 
                       "Decrease"=scen<-"dec", 
                       "Elevation Threshold"=scen<-"elev")
    
    fog_time<-switch(input$timeperiods,
                     "1981-2010"=time<-"historic",
                     "2010-2039"=time<-"2010_2039",
                     "2040-2069"=time<-"2040_2069", 
                     "2070-2099"=time<-"2070_2099")
    
    
    fog_sri<-raster(paste0("data/fog/sri/",foggy_scen,"/", fog_time, ".tif"))
    proj4string(fog_sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    fog_stack_list <- list.dirs("data/fog/sri/", full.names = TRUE)
    fog_files <- fog_stack_list[grep(paste0(foggy_scen), fog_stack_list, fixed=T)]
    fog_files_list <- list.files(fog_files, full.names = TRUE)
    fog_files2 <- fog_files_list[grep(".tif", fog_files_list, fixed=T)]
    fog_stack <- stack(fog_files2)
    
    fog_pal <- colorNumeric(
      palette = "Blues",
      domain = values(fog_stack),
      na.color = NA
    )
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik)  %>%
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) %>% 
      addRasterImage(fog_sri, colors = fog_pal, opacity = 0.8) %>% 
      addLegend("topright", pal = fog_pal, values= values(fog_stack),
                title = "Probability",
                labFormat = labelFormat(transform=function(fog_stack) sort (fog_stack, decreasing=FALSE)))
    
    
  })#end render leaflet


  ###############################################
  ### SDM Tab ###
  ###############################################
  
  output$histsdmmap <- renderLeaflet({
    
    
    histscen<-switch(input$histscenario,
                     "No Fog"=histscen<-"nofog",
                     "Fog"=histscen<-"fogconstant")
    
    histscr<-raster(paste0("data/sdm/scr/",histscen,"/historic.tif")) 
    proj4string(histscr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    
    histsri<-raster(paste0("data/sdm/sri/",histscen,"/historic.tif")) 
    proj4string(histsri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    histmerged<-merge(histscr, histsri)
    
    histsdmcol <- switch(input$histsdmcolor,
                         "Spectral" = colorNumeric(palette = "Spectral", domain=values(histmerged), na.color = "transparent", reverse=TRUE),
                         "Inverse Spectral" = colorNumeric(palette = "Spectral", domain=values(histmerged), na.color = "transparent", reverse=FALSE),
                         "Viridis" = colorNumeric(palette = "viridis", domain=values(histmerged), na.color = "transparent", reverse=TRUE),
                         "Magma" = colorNumeric(palette = "magma", domain=values(histmerged), na.color = "transparent", reverse=TRUE))
    
    leaflet() %>% addTiles() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      addRasterImage(histmerged, colors = histsdmcol, opacity = 0.8) %>%
      addLegend("topright", pal = histsdmcol, values = values(histmerged),
                title = "Suitability", 
                labFormat = labelFormat(transform=function(histmerged) sort (histmerged, decreasing=FALSE))) #decreasing false until can figure out how to reverse legend colors
    
  }) #end render leaflet
  
  output$historictable <- renderText({ #switch to render_gt if want gt table
    
    histscendf<-switch(input$histscenario,
                     "No Fog"=histscendf<-"nofog",
                     "Fog"=histscendf<-"fogconstant")

    scrhistcsv<-read_csv(paste0("data/sdm/scr/", histscendf, "/historicsummarytable.csv"))
    scrhistdata<-scrhistcsv[1,2:4]
    colnames(scrhistdata)<-c("AUC", "HighestSuit", "PerSuit")
    scrhistdf<-scrhistdata %>% 
      mutate(AUC=round(AUC,2)) %>% 
      mutate(HighestSuit=round(HighestSuit,2)) %>% 
      mutate(PerSuit=PerSuit*100) %>% 
      mutate(PerSuit=round(PerSuit,2)) %>% 
      mutate(Island="Santa Cruz") %>% 
      select(Island, everything())

    srihistcsv<-read_csv(paste0("data/sdm/sri/", histscendf, "/historicsummarytable.csv"))
    srihistdata<-srihistcsv[1,2:4]
    colnames(srihistdata)<-c("AUC", "HighestSuit", "PerSuit")
    srihistdf<-srihistdata %>% 
      mutate(AUC=round(AUC,2)) %>% 
      mutate(HighestSuit=round(HighestSuit,2)) %>% 
      mutate(PerSuit=PerSuit*100) %>% 
      mutate(PerSuit=round(PerSuit,2)) %>% 
      mutate(Island="Santa Rosa") %>% 
      select(Island, everything())
    
    histdf<-rbind(scrhistdf,srihistdf)
    colnames(histdf)<- c("Island", "Avg Test AUC", "Highest Suitability", "% Suitable")
    #Renaming so I can have spaces
    
    kable(histdf, caption="Historic SDM Results for Santa Cruz and Santa Rosa Islands. 
             Results updated based on scenario selection, showing the  
          Average Test AUC for the model run, the highest predicted
          suitability value, and the percent of the island deemed suitable.",
          booktabs=TRUE, align=c(rep('c',times=4))) %>%
      kable_styling(bootstrap_options=c("condensed", font_size=12),full_width=F, position="left") %>%
      row_spec(0, color="black", background="lightblue", bold=TRUE) %>% 
      row_spec(1:2, background="#F8F9F9") %>% 
      column_spec(1:4, width="5cm")
    
  })
  
  
  
     output$sdmmap <- renderLeaflet({
       
       
    scen<-switch(input$scenario,
                 "No Fog"=scen<-"nofog",
                 "Constant Fog"=scen<-"fogconstant", 
                 "Fog Increase"=scen<-"foginc", 
                 "Fog Decrease"=scen<-"fogdec", 
                 "Fog Elevation Threshold"=scen<-"fogelev")
    proj<-switch(input$projection,
                 "Hot-Wet"=proj<-"CCSM4_rcp85",
                 "Warm-Wet"=proj<-"MPI_rcp45", 
                 "Warm-Dry"=proj<-"MIROC_rcp45", 
                 "Hot-Dry"=proj<-"MIROC_rcp85")
    time<-switch(input$timeperiod,
                 "2010-2039"=time<-"_2010_2039",
                 "2040-2069"=time<-"_2040_2069", 
                 "2070-2099"=time<-"_2070_2099")
    
     
       scr<-raster(paste0("data/sdm/scr/",scen,"/", proj, time, ".tif")) 
       proj4string(scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       
       
       sri<-raster(paste0("data/sdm/sri/",scen,"/", proj, time, ".tif")) 
       proj4string(sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       
       merged<-merge(scr, sri)
       
     sdmcol <- switch(input$sdmcolor,
                      "Spectral" = colorNumeric(palette = "Spectral", domain=values(merged), na.color = "transparent", reverse=TRUE),
                      "Inverse Spectral" = colorNumeric(palette = "Spectral", domain=values(merged), na.color = "transparent", reverse=FALSE),
                      "Viridis" = colorNumeric(palette = "viridis", domain=values(merged), na.color = "transparent", reverse=TRUE),
                      "Magma" = colorNumeric(palette = "magma", domain=values(merged), na.color = "transparent", reverse=TRUE))
     
     leaflet() %>% addTiles() %>%
       addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
       addRasterImage(merged, colors = sdmcol, opacity = 0.8) %>%
       addLegend("topright", pal = sdmcol, values = values(merged),
                 title = "Suitability", 
                 labFormat = labelFormat(transform=function(merged) sort (merged, decreasing=FALSE))) #decreasing false until can figure out how to reverse legend colors
     
   }) #end render leaflet
     
     output$projtable <- renderText({
       
       scendf<-switch(input$scenario,
                    "No Fog"=scendf<-"nofog",
                    "Constant Fog"=scendf<-"fogconstant", 
                    "Fog Increase"=scendf<-"foginc", 
                    "Fog Decrease"=scendf<-"fogdec", 
                    "Fog Elevation Threshold"=scendf<-"fogelev")
       projdf<-switch(input$projection,
                    "Hot-Wet"=projdf<-"CCSM4_rcp85",
                    "Warm-Wet"=projdf<-"MPI_rcp45", 
                    "Warm-Dry"=projdf<-"MIROC_rcp45", 
                    "Hot-Dry"=projdf<-"MIROC_rcp85")
       timedf<-switch(input$timeperiod,
                    "2010-2039"=timedf<-"_2010_2039",
                    "2040-2069"=timedf<-"_2040_2069", 
                    "2070-2099"=timedf<-"_2070_2099")
       
       projection<-paste0(projdf,timedf)
       if(projection=="MPI_rcp45_2010_2039"){
         rownum<-1
       }else if(projection=="MPI_rcp45_2040_2069"){
         rownum<-2
       }else if(projection=="MPI_rcp45_2070_2099"){
         rownum<-3
       }else if(projection=="CCSM4_rcp85_2010_2039"){
         rownum<-4
       }else if(projection=="CCSM4_rcp85_2040_2069"){
         rownum<-5
       }else if(projection=="CCSM4_rcp85_2070_2099"){
         rownum<-6
       }else if(projection=="MIROC_rcp45_2010_2039"){
         rownum<-7
       }else if(projection=="MIROC_rcp45_2040_2069"){
         rownum<-8
       }else if(projection=="MIROC_rcp45_2070_2099"){
         rownum<-9
       }else if(projection=="MIROC_rcp85_2010_2039"){
         rownum<-10
       }else if(projection=="MIROC_rcp85_2040_2069"){
         rownum<-11
       }else if(projection=="MIROC_rcp85_2070_2099"){
         rownum<-12
       }else
         rownum<-4 #default to hot-wet 2010-2039 (shiny first projection)
       
       scrprojcsv<-read_csv(paste0("data/sdm/scr/", scendf, "/projectionssummarytable.csv"))
       scrprojdata<-scrprojcsv[rownum,-(4:6)]
       colnames(scrprojdata)<-c("Project", "AUC", "HighestSuit", "PerSuit", "PerChg")#rename so easier to change
       scrprojdf<-scrprojdata[2:5] %>% 
         mutate(AUC=round(AUC,2)) %>% 
         mutate(HighestSuit=round(HighestSuit,2)) %>% 
         mutate(PerSuit=PerSuit*100) %>% 
         mutate(PerSuit=round(PerSuit,2)) %>% 
         mutate(PerChg=PerChg*100) %>% 
         mutate(PerChg=round(PerChg,2)) %>% 
         mutate(Island="Santa Cruz") %>% 
         select(Island, everything())
       
       sriprojcsv<-read_csv(paste0("data/sdm/sri/", scendf, "/projectionssummarytable.csv"))
       sriprojdata<-sriprojcsv[rownum,-(4:6)]
       colnames(sriprojdata)<-c("Project", "AUC", "HighestSuit", "PerSuit", "PerChg")#rename so easier to change
       sriprojdf<-sriprojdata[2:5] %>% 
         mutate(AUC=round(AUC,2)) %>% 
         mutate(HighestSuit=round(HighestSuit,2)) %>% 
         mutate(PerSuit=PerSuit*100) %>% 
         mutate(PerSuit=round(PerSuit,2)) %>% 
         mutate(PerChg=PerChg*100) %>% 
         mutate(PerChg=round(PerChg,2)) %>% 
         mutate(Island="Santa Rosa") %>% 
         select(Island, everything())
       
       projecttable<-rbind(scrprojdf,sriprojdf)
       colnames(projecttable)<- c("Island", "Avg Test AUC", "Highest Suitability", "% Suitable", "% Change")
       #Renaming so I can have spaces
       
       kable(projecttable, caption="Projected SDM Results for Santa Cruz and Santa Rosa Islands. 
             Results updated based on selected scenario combination, showing the  
          Average Test AUC for the model run, the highest predicted
             suitability value, the percent of the island deemed suitable, and percent change 
          in suitability relative to the historic model.",
             booktabs=TRUE, align=c(rep('c',times=5))) %>%
         kable_styling(bootstrap_options=c("striped", "condensed",font_size=12), full_width=FALSE,position="left") %>%
         row_spec(0, color="black", background="lightblue", bold=TRUE) %>% 
         row_spec(1:2, background="#F8F9F9") %>% 
         column_spec(1:5, width="4.5cm")
       
       
     })
   
   
}#end server

# Run the app ----
shinyApp(ui = ui, server = server)


