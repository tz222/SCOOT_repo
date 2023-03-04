#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(maps)
library(mapproj)
library(stringr)
library(htmltools)
library(sf)
library(ggplot2)
library(shinyBS)

rm(list = ls())
msa_tract_shp_path = "/Users/tianqizou/Documents/DOE/shapefiles/msa_puma_tract_join.shp"
msa_tract_shp = read_sf(msa_tract_shp_path)


geo_msa_tract = msa_tract_shp[msa_tract_shp$jmsa_geoid==12420,]
geo_results = read.csv("/Users/tianqizou/Documents/DOE/model literature/hybrid choice/aws/austin_base_utils.csv")

# geo_results$sum_exp = exp(geo_results$car.utility)*geo_results$car_av +
#     exp(geo_results$transit.utility)*geo_results$transit_av +
#     exp(geo_results$ridehail.utility)*geo_results$rd_av +
#     exp(geo_results$walk.utility)*geo_results$walk_av +
#     exp(geo_results$bike.utility)*geo_results$bike_av +
#     exp(geo_results$scooter.utility)*geo_results$scooter_av +
#     exp(geo_results$docklessbike.utility)*geo_results$dlbike_av +
#     exp(geo_results$dockedbike.utility)*geo_results$dkbike_av +
#     exp(geo_results$scooter_transit.utility)*geo_results$sctransit_av

#geo_results$accessibility = log(sum_exp)

#geo_agg = aggregate(geo_results[,c('TRACT','accessibility')],by=list(geo_results$TRACT),FUN = mean)

#geo_map = merge(geo_msa_tract,geo_agg,by.y='TRACT',by.x='geoid')


#example codes for state, need to change to MSA
msa_codes = list(
    "Select" = "00",
    "Austin" = 12420)

# Define UI for application that generate metrics of interests
ui <- fluidPage(

    # Application title
    titlePanel("Micromobility Screening for City Opportunities Online Tool (SCOOT)"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            selectInput("msa_codes", "Choose a MSA", 
                        choices = msa_codes, selected = "00"),
            selectInput("density_level", 
                        #label = "Implementation Location (by density)", 
                        label = tags$span("Implementation Location (by density)", 
                                          bsButton("density_info", label = "", icon = icon("info"), 
                                                   style = "info", size = "extra-small")),
                        choices = c("Everywhere" = 1,
                                    "Above 5,000 per sq.mile"=2,
                                    "Above 10,000 per sq.mile"=3,
                                    "Above 20,000 per sq.mile"=4)),
            bsPopover(
                id = "density_info",
                title = "More information",
                content = "Introduce micromobility services only to areas above a certain level of population density",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            ),
            radioButtons("veh_avail", 
                         label = tags$span("Access/Drop-off Walking Time", 
                                           bsButton("access_info", label = "", icon = icon("info"), 
                                                    style = "info", size = "extra-small")),
                         choices = list("1 min (157 bikes/scooters per sq.mi) on average" = 1, 
                                        "3 min (17 bikes/scooters per sq.mi) on average" = 3,
                                        "5 min (6 bikes/scooters per sq.mi) on average"=5), selected = 1),
            bsPopover(
                id = "access_info",
                title = "More information",
                content = "Average walking time to the nearest bike or scooter or average walking time from dropoff station location to final destination",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            ),
            radioButtons("bike_lane", 
                         label = tags$span("Bike Lane Availbility", 
                                           bsButton("bikelane_info", label = "", icon = icon("info"), 
                                                    style = "info", size = "extra-small")),
                         choices = list("Less than 80% of the whole trip" = 0, 
                                        "More than or equal to 80% of the whole trip"=1), selected = 0),
            bsPopover(
                id = "bikelane_info",
                title = "More information",
                content = "Bike lane coverage of the whole trip",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            ),
            checkboxGroupInput("service_types", 
                               label ="Service Types", 
                               choices = list("Scooter" = 1,
                                              "Docked Bike" = 2,
                                              "Dockless Bike" = 3, 
                                              "Scooter+Transit" = 4), 
                               selected = c(1,2,3,4)),
            radioButtons("report_metric", 
                         label = tags$span("Report Metric on Map", 
                                           bsButton("report_info", label = "", icon = icon("info"), 
                                                    style = "info", size = "extra-small")),
                                        
                                           choices = list("Trip Count (per day)" = 1, 
                                                                          "Net Accessibility" = 2,
                                                                          "Net Mobility Carbon Productivity (MCP)"=3,
                                                                          'Net GHG Emission (kg)'=4), selected = 1),
                         bsPopover(
                             id = "report_info",
                             title = "More information",
                             content = paste("Trip count: total number of daily trips",
                                             "Net accessibility: change in average accessibility per trip after introducing micromobility services",
                                             "Net MCP: change in average MCP per trip after introducing micromobility services",
                                             "Net GHG emission: change in GHG emission of total trips after introducing micromobility services", 
                                             sep = "<br><br>")
                             ,
                             placement = "right",
                             trigger = "hover",
                             options = list(container = "body")
                         )
                        
            #actionButton("show_results", "Show Results"),
            #actionButton("save_selection", "Save Selection")
            
            
            ),

        mainPanel(
            # fluidRow(
            # column(6,
            #        selectInput("density_level", 
            #                    label = "Implementation Location (by density)", 
            #                    choices = c("Everywhere" = 1,
            #                                "Above 5,000 per sq.mile"=2,
            #                                "Above 10,000 per sq.mile"=3,
            #                                "Above 20,000 per sq.mile"=4) )
            #        )),
            # fluidRow(
            # column(6,
            #        checkboxGroupInput("service_types", "Service Types", 
            #                           choices = list("Scooter" = 1,
            #                                          "Docked Bike" = 2,
            #                                          "Dockless Bike" = 3, 
            #                                          "Scooter+Transit" = 4), 
            #                           selected = c(1,2,3,4))
            # ),
            # column(6,
            #        radioButtons("report_metric", "Report Metric", choices = list("Trip Count (per day)" = 1, 
            #                                                                      "Net Accessibility" = 2,
            #                                                                      "Net Mobility Carbon Productivity (MCP)"=3,
            #                                                                      'Net GHG Emission (kg)'=4), selected = 1)
            # )),
            fluidRow(
                textOutput("totalGHG")
            ),
            fluidRow (
                leafletOutput("mymap")),
            fluidRow(
                column(6,
                       plotOutput('accessibility')),
                column(6,
                       plotOutput('mcp')))
           
        )
    )
)


server <- function(input, output) {
    map_start = reactive({
        if (input$msa_codes!= "00"){
            return(TRUE)
        }
        else{
            return(FALSE)
        }
    })
    
    #select msa_tract shapefile
    selectMSA_reactive = reactive({
        if (input$msa_codes!= "00"){
            geo_msa_tract = msa_tract_shp[msa_tract_shp$jmsa_geoid==input$msa_codes,]
            return(geo_msa_tract)
        }
        else{
            return()
        }
    })
    
    #select file corresponding to variables
    selectVar_reactive = reactive({
        path = "/Users/tianqizou/Documents/DOE/model literature/hybrid choice/aws/"
        filename = paste0('msa',input$msa_codes,'b',input$bike_lane,'v', input$veh_avail, '.csv')
        print(paste0(path, filename))
        geo_results = read.csv(paste0(path, filename))
        return(geo_results)
        
    })
    
    #calculate preliminary results
    prelim_reactive = reactive({
        geo_results = selectVar_reactive()
        scooter = 0
        docked = 0
        dockless = 0
        sc_transit = 0
        if (1 %in% input$service_types) {
            scooter = 1
        }
        if (2 %in% input$service_types) {
            docked = 1
        }
        if (3 %in% input$service_types) {
            dockless = 1
        }
        if (4 %in% input$service_types) {
            sc_transit = 1
        }
        
        if (input$density_level ==1){
            geo_results$density=1
        }
        if (input$density_level ==2){
            geo_results$density=ifelse(geo_results$popdensity>=5,1,0)
        }
        if (input$density_level ==3){
            geo_results$density=ifelse(geo_results$popdensity>=10,1,0)
        }
        if (input$density_level ==4){
            geo_results$density=ifelse(geo_results$popdensity>=20,1,0)
        }
        # sum of expontial utility
        ut_car = exp(geo_results$car.utility)*geo_results$car_av 
        ut_transit = exp(geo_results$transit.utility)*geo_results$transit_av 
        ut_ridehail = exp(geo_results$ridehail.utility)*geo_results$rd_av 
        ut_walk = exp(geo_results$walk.utility)*geo_results$walk_av 
        ut_bike = exp(geo_results$bike.utility)*geo_results$bike_av 
        ut_scooter = exp(geo_results$scooter.utility)*geo_results$scooter_av*scooter*geo_results$density
        ut_dockless = exp(geo_results$docklessbike.utility)*geo_results$dlbike_av*dockless*geo_results$density
        ut_docked = exp(geo_results$dockedbike.utility)*geo_results$dkbike_av*docked*geo_results$density 
        ut_sc_transit = exp(geo_results$scooter_transit.utility)*geo_results$sctransit_av*sc_transit*geo_results$density
        
        geo_results$sum_exp = ut_car + 
            ut_transit + 
            ut_ridehail + 
            ut_walk + 
            ut_bike + 
            ut_scooter + 
            ut_dockless + 
            ut_docked +
            ut_sc_transit
        
        geo_results$sum_exp_base = ut_car + 
            ut_transit + 
            ut_ridehail + 
            ut_walk + 
            ut_bike 
        
        # get probability
        geo_results$Pcar = ut_car / geo_results$sum_exp
        geo_results$Ptransit = ut_transit / geo_results$sum_exp
        geo_results$Pridehail = ut_ridehail / geo_results$sum_exp 
        geo_results$Pwalk = ut_walk / geo_results$sum_exp
        geo_results$Pbike = ut_bike / geo_results$sum_exp
        geo_results$Pscooter = ut_scooter / geo_results$sum_exp
        geo_results$Pdocklessbike = ut_dockless / geo_results$sum_exp
        geo_results$Pdockedbike = ut_docked / geo_results$sum_exp
        geo_results$Pscooter_transit =  ut_sc_transit / geo_results$sum_exp
        
        geo_results$Pcar_b = ut_car / geo_results$sum_exp_base
        geo_results$Ptransit_b = ut_transit / geo_results$sum_exp_base
        geo_results$Pridehail_b = ut_ridehail / geo_results$sum_exp_base 
        geo_results$Pwalk_b = ut_walk / geo_results$sum_exp_base
        geo_results$Pbike_b = ut_bike / geo_results$sum_exp_base
        # GHG emission factor: g CO2-eq/passenger-mile
        eCar = 424
        eTran = 291
        eDocked =105
        eDockless = 190
        eScoot = 202
        eRidehail = 443
        
        # SCC  $/metric ton
        scc = 51
        a=-0.799 # need to prepare a file to store all cost coefficients
        
        geo_results$pE =(eCar*geo_results$Pcar+
                 eTran*geo_results$Ptransit+
                 eDocked*geo_results$Pdockedbike+
                 eDockless*geo_results$Pdocklessbike+
                 eScoot*geo_results$Pscooter+
                 eRidehail*geo_results$Pridehail+
                 (0.3*eScoot + 0.7*eTran)*geo_results$Pscooter_transit)*geo_results$TRPMILES
        
        geo_results$pE_b = (eCar*geo_results$Pcar_b+eTran*geo_results$Ptransit_b+eRidehail*geo_results$Pridehail_b)*geo_results$TRPMILES
        return(geo_results)
        
    })
    #caculate and aggregate for reporting metrics
    aggregate_reactive = reactive({
        geo_msa_tract = selectMSA_reactive()
        geo_results = prelim_reactive()
        
        geo_tripcount = aggregate(geo_results[,'orig_tract'], by =list(geo_results$orig_tract), length)
        
        if (input$report_metric==1){#trip count
            # total number of trip per tract
            geo_agg = aggregate(geo_results[,c('orig_tract','Pscooter','Pdocklessbike','Pdockedbike','Pscooter_transit')],
                                by=list(geo_results$orig_tract),FUN = mean)
            geo_agg$trip_count = geo_tripcount$x
            geo_agg$report = rowSums(geo_agg[,c('Pscooter','Pdocklessbike','Pdockedbike','Pscooter_transit')])*geo_agg$trip_count/0.03
            #geo_map = merge(geo_msa_tract,geo_agg,by.y='orig_tract',by.x='geoid')
        }
        else if (input$report_metric==2){#accessibility
            geo_results$report = log(geo_results$sum_exp)-log(geo_results$sum_exp_base)
            geo_agg = aggregate(geo_results[,c('TRACT','report')],by=list(geo_results$TRACT),FUN = mean)
            #geo_map = merge(geo_msa_tract,geo_agg,by.y='TRACT',by.x='geoid')
            
           
        }
        else if (input$report_metric==3){#MCP
            # SCC  $/metric ton
            scc = 51
            a=-0.799 # need to prepare a file to store all cost coefficients
            
            # term2 = exp(a*scc*geo_results$pE/1000000)#ton
            # term2_b = exp(a*scc*geo_results$pE_b/1000000)
            # geo_results$report = log(geo_results$sum_exp + term2 )-log(geo_results$sum_exp_base + term2_b )
            
            term2 = a*scc*geo_results$pE/1000000#ton
            term2_b = a*scc*geo_results$pE_b/1000000
            geo_results$report = log(geo_results$sum_exp) + term2 -log(geo_results$sum_exp_base) - term2_b 
            
            geo_agg = aggregate(geo_results[,c('TRACT','report')],by=list(geo_results$TRACT),FUN = mean)
            #geo_map = merge(geo_msa_tract,geo_agg,by.y='TRACT',by.x='geoid')
        }
        else if (input$report_metric==4){#GHG (sum)
            geo_results$report = geo_results$pE- geo_results$pE_b
            geo_agg = aggregate(geo_results[,c('TRACT','report')],by=list(geo_results$TRACT),FUN = sum)
            geo_agg$report = geo_agg$report/0.03/1000 #kg
            #geo_map = merge(geo_msa_tract,geo_agg,by.y='Group.1',by.x='geoid')
        }
        return(geo_agg)
        
    })
    output$mymap <- renderLeaflet({
        if(map_start()){
            geo_msa_tract = selectMSA_reactive()
            geo_agg = aggregate_reactive()
            geo_map = merge(geo_msa_tract,geo_agg,by.y='Group.1',by.x='geoid')
            if (input$report_metric == 1) {
                label_report = sprintf("Trip count:%g trips per day %s", round(geo_map$report, 1),geo_map$namelsad)
                groupname = "Trip count"
                legendformat = labelFormat()
                legend_title = "Trip count per day"
 
            }
            else if (input$report_metric == 2) {
                label_report = sprintf("Net Logsum Accessibility:%g, %s", round(geo_map$report, 3),geo_map$namelsad)
                groupname = "Accessibility"
                legendformat = labelFormat()
                legend_title = "Net accessibility per trip"
            }
            else if (input$report_metric == 3) {
                label_report = sprintf("Net Mobility Carbon Productivity (MCP): %g, %s", round(geo_map$report, 3),geo_map$namelsad)
                groupname = "Mobility Carbon Productivity (MCP)"
                legendformat = labelFormat()
                legend_title = "Net MCP per trip"
            }
            else if (input$report_metric == 4) {
                label_report = sprintf("Net GHG Emission: %g kg, %s", round(geo_map$report, 2),geo_map$namelsad)
                groupname = "GHG Emission"
                legendformat = labelFormat()
                legend_title = "Net GHG Emission (kg) per day"
            }
            
             
            pal_report = colorNumeric("YlOrRd", domain = geo_map$report)
            leaflet(geo_map) %>%
                addTiles() %>%
                setView(lng = mean(as.numeric(geo_map$intptlon)), lat = mean(as.numeric(geo_map$intptlat)), zoom = 10)%>%
                addPolygons(fillColor = ~pal_report(geo_map$report), 
                            weight = 1,
                            opacity = 1,
                            color = "white",
                            dashArray = "3",
                            fillOpacity = 0.7, 
                            highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
                            label = label_report,
                            labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto"))%>%
                addLegend(title = legend_title,pal = pal_report, values = geo_map$report, group = groupname, opacity= 0.7, position = "bottomright", labFormat = legendformat)
            
        }
        else{
            leaflet() %>%
                addTiles() %>%
                setView(lng = -95, lat = 37, zoom = 4)
        }    
        
    })
    output$totalGHG <- renderText({
        if(map_start() & input$report_metric == 4){
            
            geo_msa_tract = selectMSA_reactive()
            geo_agg = aggregate_reactive()
            paste0("City Total Net GHG Emission per Day: ",round(sum(geo_agg$report)/1000,2)," Metric Ton")
        }
        else{""}
        
    })
    output$accessibility <- renderPlot({
        if(map_start()){
        geo_results = prelim_reactive()
        geo_agg = aggregate_reactive()
        q_hh = quantile(geo_results$mean_hhincomek)
        quantile_hh = function(x){
            if(x <= q_hh[[2]]){
                #return(paste0('under ', q_hh[[2]]))
                1
            }else if(x <= q_hh[[3]]){
                #return(paste0(q_hh[[2]],'-',q_hh[[3]]))
                2
            }else if(x <= q_hh[[4]] ){
                #return(paste0(q_hh[[3]],'-',q_hh[[4]]))
                3
            }else {
                #return(paste0('Over',q_hh[[4]]))
                4
            }
        }
        geo_results$qhh = apply(geo_results["mean_hhincomek"],MARGIN = 1, FUN=quantile_hh)
        
        geo_results$report = log(geo_results$sum_exp)-log(geo_results$sum_exp_base)
            
        geo_agg_hhincome = aggregate(geo_results[,c('qhh','report')],by=list(geo_results$qhh),FUN = mean)
        geo_agg_hhincome$quartile = c(paste0('Under ', q_hh[[2]]),
                                          paste0(q_hh[[2]],'-', q_hh[[3]]),
                                          paste0(q_hh[[3]],'-', q_hh[[4]]),
                                          paste0('Over ', q_hh[[4]]))
        p<-ggplot(data=geo_agg_hhincome, aes(x=quartile, y=report)) +
                geom_bar(stat="identity",fill="darkred",alpha=0.8) +
                theme_minimal()+
                ylab('Net Acceessibility')+
                xlab('Income group (in $1000)')+
                scale_x_discrete(limits= geo_agg_hhincome$quartile)
        p
        }else{
            plot.new()
        }
    })
    output$mcp <- renderPlot({
        if(map_start()){
            geo_results = prelim_reactive()
            geo_agg = aggregate_reactive()
            q_hh = quantile(geo_results$mean_hhincomek)
            quantile_hh = function(x){
                if(x <= q_hh[[2]]){
                    #return(paste0('under ', q_hh[[2]]))
                    1
                }else if(x <= q_hh[[3]]){
                    #return(paste0(q_hh[[2]],'-',q_hh[[3]]))
                    2
                }else if(x <= q_hh[[4]] ){
                    #return(paste0(q_hh[[3]],'-',q_hh[[4]]))
                    3
                }else {
                    #return(paste0('Over',q_hh[[4]]))
                    4
                }
            }
            geo_results$qhh = apply(geo_results["mean_hhincomek"],MARGIN = 1, FUN=quantile_hh)
            
            scc = 51
            a=-0.799 # eed to prepare a file to store all cost coefficients
            
            # term2 = exp(a*scc*geo_results$pE/1000000)#ton
            # term2_b = exp(a*scc*geo_results$pE_b/1000000)
            # geo_results$report = log(geo_results$sum_exp + term2 )-log(geo_results$sum_exp_base + term2_b )
            
            term2 = a*scc*geo_results$pE/1000000#ton
            term2_b = a*scc*geo_results$pE_b/1000000
            geo_results$report = log(geo_results$sum_exp) + term2 -log(geo_results$sum_exp_base) - term2_b 
            
            geo_agg_hhincome = aggregate(geo_results[,c('qhh','report')],by=list(geo_results$qhh),FUN = mean)
            geo_agg_hhincome$quartile = c(paste0('Under ', q_hh[[2]]),
                                          paste0(q_hh[[2]],'-', q_hh[[3]]),
                                          paste0(q_hh[[3]],'-', q_hh[[4]]),
                                          paste0('Over ', q_hh[[4]]))
            p<-ggplot(data=geo_agg_hhincome, aes(x=quartile, y=report)) +
                geom_bar(stat="identity",fill="darkred",alpha=0.8) +
                theme_minimal()+
                ylab('Net MCP')+
                xlab('Income group (in $1000)')+
                scale_x_discrete(limits= geo_agg_hhincome$quartile)
            p
            
        } else{
            plot.new()
        }
        

        })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
