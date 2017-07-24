library(shiny)
library(DT)

not_dead_predictions=read.csv("got_predictions_after_EP2.csv")
not_dead_predictions$image=paste0('<img src="got_images/',not_dead_predictions$image,'" height="80"></img>')
not_dead_predictions=not_dead_predictions[,-2]

not_dead_predictions$highprob=as.numeric(as.character(not_dead_predictions$dead_prob))>.5
not_dead_predictions$highprob=as.numeric(not_dead_predictions$highprob)
server = function(input, output) {
  ndp=datatable(not_dead_predictions,options = list(order=list(1,"desc"),columnDefs = list(list(className = 'dt-center', targets = 1),list(targets = 6, visible = FALSE)),
                                                    pageLength = 20, autoWidth = TRUE), filter = 'top',class = 'hover',
                escape = FALSE,rownames=F,colnames = c('Name', 'Probability of Death', 'Current Location', 'Main House Interaction', 'House Allegiance',"Character Image","High Prob")) %>% formatPercentage('dead_prob', 2) %>% formatStyle(
                  "highprob",
                  target = 'row',
                  backgroundColor = styleEqual(c(0, 1), c('#B2DDB2', '#ddb2b2'))
                )%>% formatStyle("name",fontWeight="bold")
  output$tbl = DT::renderDataTable(
    ndp
  )
}