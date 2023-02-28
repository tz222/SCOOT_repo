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

msa_tract_shp_path = "/Users/tianqizou/Documents/DOE/shapefiles/msa_puma_tract_join.shp"
msa_tract_shp = read_sf(msa_tract_shp_path)

geo_msa_tract = msa_tract_shp[msa_tract_shp$jmsa_geoid==12420,]
geo_results = read.csv("/Users/tianqizou/Documents/DOE/model literature/hybrid choice/aws/austin_base_utils.csv")

geo_results$sum_exp = exp(geo_results$car.utility)*geo_results$car_av +
    exp(geo_results$transit.utility)*geo_results$transit_av +
    exp(geo_results$ridehail.utility)*geo_results$rd_av +
    exp(geo_results$walk.utility)*geo_results$walk_av +
    exp(geo_results$bike.utility)*geo_results$bike_av +
    exp(geo_results$scooter.utility)*geo_results$scooter_av +
    exp(geo_results$docklessbike.utility)*geo_results$dlbike_av +
    exp(geo_results$dockedbike.utility)*geo_results$dkbike_av +
    exp(geo_results$scooter_transit.utility)*geo_results$sctransit_av

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
                        label = "Implementation Location (by density)", 
                        choices = c("Everywhere" = 1,
                                    "Above 5,000 per sq.mile"=2,
                                    "Above 10,000 per sq.mile"=3,
                                    "Above 20,000 per sq.mile"=4) ),
            radioButtons("veh_avail", "Access/Drop-off Walking Time", 
                         choices = list("1 min (157 bikes/scooters per sq.mi) on average" = 1, 
                                        "3 min (17 bikes/scooters per sq.mi) on average" = 3,
                                        "5 min (6 bikes/scooters per sq.mi) on average"=5), selected = 1),
            radioButtons("bike_lane", "Bike Lane Availbility", 
                         choices = list("Less than 80% of the whole trip" = 1, 
                                        "More than or equal to 80% of the whole trip"=2), selected = 1),
            
            actionButton("show_results", "Show Results"),
            actionButton("save_selection", "Save Selection")
            
            
            ),

        mainPanel(
            
            column(6,
                   checkboxGroupInput("service_types", "Service Types", 
                                      choices = list("Scooter" = 1,
                                                     "Doked Bike" = 2,
                                                     "Dockless Bike" = 3, 
                                                     "Scooter+Transit" = 4), 
                                      selected = c(1,2,3,4))
            ),
            column(6,
                   radioButtons("report_metric", "Report Metric", choices = list("Trip Count" = 1, 
                                                                                 "Accessibility" = 2,
                                                                                 "Mobility Carbon Productivity (MCP)"=3,
                                                                                 'GHG Emission'=4), selected = 1)
            ),
            leafletOutput("mymap"),
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
    selectMSA_reactive = reactive({
        if (input$msa_codes!= "00"){
            geo_msa_tract = msa_tract_shp[msa_tract_shp$jmsa_geoid==input$msa_codes,]
            return(geo_msa_tract)
        }
        else{
            return()
        }
    })
    aggregate_reactive = reactive({
        geo_msa_tract = selectMSA_reactive()
        
        # get probability
        geo_results$Pcar = exp(geo_results$car.utility)*geo_results$car_av / geo_results$sum_exp
        geo_results$Ptransit = exp(geo_results$transit.utility)*geo_results$transit_av / geo_results$sum_exp
        geo_results$Pridehail = exp(geo_results$ridehail.utility)*geo_results$rd_av / geo_results$sum_exp 
        geo_results$Pwalk = exp(geo_results$walk.utility)*geo_results$walk_av / geo_results$sum_exp
        geo_results$Pbike = exp(geo_results$bike.utility)*geo_results$bike_av / geo_results$sum_exp
        geo_results$Pscooter = exp(geo_results$scooter.utility)*geo_results$scooter_av / geo_results$sum_exp
        geo_results$Pdocklessbike = exp(geo_results$docklessbike.utility)*geo_results$dlbike_av / geo_results$sum_exp
        geo_results$Pdockedbike = exp(geo_results$dockedbike.utility)*geo_results$dkbike_av / geo_results$sum_exp
        geo_results$Pscooter_transit =  exp(geo_results$scooter_transit.utility)*geo_results$sctransit_av / geo_results$sum_exp
        
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
        
        pE =(eCar*geo_results$Pcar+
                 eTran*geo_results$Ptransit+
                 eDocked*geo_results$Pdockedbike+
                 eDockless*geo_results$Pdocklessbike+
                 eScoot*geo_results$Pscooter+
                 eRidehail*geo_results$Pridehail+
                 (0.3*eScoot + 0.7*eTran)*geo_results$Pscooter_transit)*geo_results$TRPMILES
        
        geo_tripcount = aggregate(geo_results[,'orig_tract'], by =list(geo_results$orig_tract), length)
        
        if (input$report_metric==1){#trip count
            # total number of trip per tract
            geo_agg = aggregate(geo_results[,c('orig_tract','Pscooter','Pdocklessbike','Pdockedbike','Pscooter_transit')],
                                by=list(geo_results$orig_tract),FUN = mean)
            geo_agg$trip_count = geo_tripcount$x
            geo_agg$report = rowSums(geo_agg[,c('Pscooter','Pdocklessbike','Pdockedbike','Pscooter_transit')])*geo_agg$trip_count/0.03
            geo_map = merge(geo_msa_tract,geo_agg,by.y='orig_tract',by.x='geoid')
        }
        else if (input$report_metric==2){#accessibility
            geo_results$report = log(sum_exp)
            geo_agg = aggregate(geo_results[,c('TRACT','report')],by=list(geo_results$TRACT),FUN = mean)
            geo_map = merge(geo_msa_tract,geo_agg,by.y='TRACT',by.x='geoid')
        }
        else if (input$report_metric==3){#MCP
            
            term2 = exp(a*scc*pE/1000000)#ton
            geo_results$report = log(sum_exp + term2 )

            geo_agg = aggregate(geo_results[,c('TRACT','report')],by=list(geo_results$TRACT),FUN = mean)
            geo_map = merge(geo_msa_tract,geo_agg,by.y='TRACT',by.x='geoid')
        }
        else if (input$report_metric==4){#GHG (sum)
            geo_results$report = pE
            geo_agg = aggregate(geo_results[,c('TRACT','report')],by=list(geo_results$TRACT),FUN = sum)
            geo_agg$report = geo_agg$report/0.03/1000000 #ton
            geo_map = merge(geo_msa_tract,geo_agg,by.y='Group.1',by.x='geoid')
        }
        return(geo_map)
        
    })
    output$mymap <- renderLeaflet({
        if(map_start()){
            geo_map = aggregate_reactive()
            if (input$report_metric == 1) {
                label_report = sprintf("Trip count:%g trips per day %s", round(geo_map$report, 1),geo_map$namelsad)
                groupname = "Trip count"
                legendformat = labelFormat()
            }
            else if (input$report_metric == 2) {
                label_report = sprintf("Logsum Accessibility:%g, %s", round(geo_map$report, 1),geo_map$namelsad)
                groupname = "Accessibility"
                legendformat = labelFormat()
            }
            else if (input$report_metric == 3) {
                label_report = sprintf("Mobility Carbon Productivity (MCP): %g, %s", round(geo_map$report, 1),geo_map$namelsad)
                groupname = "Mobility Carbon Productivity (MCP)"
                legendformat = labelFormat()
            }
            else if (input$report_metric == 4) {
                label_report = sprintf("GHG Emission: %g ton, %s", round(geo_map$report, 1),geo_map$namelsad)
                groupname = "GHG Emission"
                legendformat = labelFormat()
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
                addLegend(pal = pal_report, values = geo_map$report, group = groupname, opacity= 0.7, title = NULL, position = "bottomright", labFormat = legendformat)
            
        }
        else{
            leaflet() %>%
                addTiles() %>%
                setView(lng = -95, lat = 37, zoom = 4)
        }    
        
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
