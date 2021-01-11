library(shiny)
library(dplyr)
library(plotly)

# load data
data <- tbl_df(read.csv("www/pokemon.csv"))

# make slugs
data$Slug <- data$Pokemon %>%
    gsub(".", "", ., fixed=T) %>%
    gsub("'", "", .) %>%
    gsub(" ", "-", .) %>%
    tolower

# calculate PCA
data_pca <- data %>%
    select(HP:Spd) %>% as.matrix

row.names(data_pca) <- data$Pokemon

pca_result <- prcomp(data_pca, center=T, scale=T)

# get stats coords
stats_coords <- data.frame(pca_result$rotation)
rownames(stats_coords) <- c("HP", "Attack", "Defense", "Sp. Attack", "Sp. Defense", "Speed")

# make dataframe for plotting
poke_coords <- data.frame(Pokemon=data$Pokemon,
                          pca_result$x,
                          Type=data$Type.I,
                          EvolvesInto=data$Evolves.Into,
                          Sprite=paste0("www/", data$Slug, ".png"),
                          InfoText=paste0("<br>Type: ", data$Type.I,
                                          "<br>HP: ", data$HP,
                                          "<br>Attack: ", data$Atk,
                                          "<br>Defense: ", data$Def,
                                          "<br>Sp. Attack: ", data$SA,
                                          "<br>Sp. Defense: ", data$SD,
                                          "<br>Speed: ", data$Spd,
                                          "<br>Evolves from: ", data$Evolves.From,
                                          "<br>Evolves into: ", data$Evolves.Into)
                          )
poke_coords$HoverInfo <- paste0("<br>Number: ", data$Nat,
                                "<br>Name: ", data$Pokemon,
                                poke_coords$InfoText)


shinyServer(function(input, output, session) {
    
    pokePlot <- reactive({
        
        dim1 <-  input$dim1
        dim2 <-  input$dim2
        dim3 <-  input$dim3
        
        # trace evolution lines
        poke_evols <- poke_coords %>%
            filter(EvolvesInto != "--")
        poke_evols <- merge(poke_coords, poke_coords, by.x="Pokemon", by.y="EvolvesInto")
        poke_evols <- poke_evols[,as.vector(outer(c(dim1, dim2, dim3), c(".x", ".y"), paste0))]
        
        # make plot
        pokeplot <- plot_ly(source="hoverplotsource")
        
        ## add pokemon  
        pokeplot <- pokeplot %>%
            add_trace(data=poke_coords,
                      x=poke_coords[,dim1],
                      y=poke_coords[,dim2],
                      z=poke_coords[,dim3],
                      color=~Type,
                      type="scatter3d",
                      mode="markers",
                      text=~HoverInfo,
                      customdata=~Sprite,
                      hovertemplate="%{text}<extra></extra>",
                      marker=list(size=5)
                      )
        
        ## add stats
        pokeplot <- pokeplot %>%
            add_trace(x=stats_coords[,dim1]*8,
                      y=stats_coords[,dim2]*8,
                      z=stats_coords[,dim3]*8,
                      text=rownames(stats_coords),
                      type="scatter3d",
                      mode="text",
                      showlegend=F,
                      hoverinfo="none")
        
        # add evolution lines
        for (i in 1:nrow(poke_evols)) {
            pokeplot <- pokeplot %>%
                add_trace(x=c(poke_evols[i,1], poke_evols[i,4]),
                          y=c(poke_evols[i,2], poke_evols[i,5]),
                          z=c(poke_evols[i,3], poke_evols[i,6]),
                          hoverinfo="none",
                          type="scatter3d",
                          mode="lines",
                          line=list(color="gray"),
                          showlegend=F)
        }

    selected_poke <-  poke_coords %>%
            filter(Pokemon==input$select)

        # make annotation
        pokenotation <- list(
            showarrow=T,
            x=selected_poke[,dim1],
            y=selected_poke[,dim2],
            z=selected_poke[,dim3],
            text=selected_poke$Pokemon,
            font=list(
                color="black",
                size=12
            ),
            arrowcolor="black",
            arrowsize=3,
            arrowwidth=1,
            arrowhead=1)
        
    ## add selected pokemon
    pokeplot %>%
        layout(scene=list(annotations=list(pokenotation),
                          xaxis=list(title=dim1),
                          yaxis=list(title=dim2),
                          zaxis=list(title=dim3))) %>%
        event_register('plotly_hover') %>%
        event_register('plotly_unhover')
    
    })
    
    output$distPlot <- renderPlotly(pokePlot())
    
    hover_event <- reactive({
        event_data(event = "plotly_hover", source = "hoverplotsource")
    })
    
    unhover_event <- reactive({
        event_data(event = "plotly_unhover", source = "hoverplotsource")
    })
    
    hoverplotlyProxy <- plotlyProxy("distPlot", session)
    
    observeEvent(unhover_event(), {
        hoverplotlyProxy %>%
            plotlyProxyInvoke("relayout", list(images = list(NULL)))
    })
    
    observeEvent(hover_event(), {
        hoverplotlyProxy %>%
            plotlyProxyInvoke("relayout", list(images = list(
                list(
                    source = hover_event()$customdata,
                    xref = "paper",
                    yref = "paper",
                    x = 0,
                    y = 1,
                    sizex = 1,
                    sizey = 1,
                    opacity = 1
                )
            )))
    })
    
    pokeImage <- reactive({
        selected_poke <-  poke_coords %>%
            filter(Pokemon==input$select) %>%
            pull(Sprite) %>% as.character
        
        return(list(
            src = selected_poke,
            width = 200,
            height = 200,
            contentType = "image/png"))
    })
    
    output$distImage <- renderImage(pokeImage(),
                                    deleteFile=F)
    
    pokeStats <- reactive({
        selected_poke <-  poke_coords %>%
            filter(Pokemon==input$select) %>%
            pull(InfoText) %>% as.character
        })
    
    output$distText <- renderText(pokeStats())
    
})
